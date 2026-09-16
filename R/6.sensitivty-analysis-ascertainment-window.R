##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: Sensitivity analyses
##
## Purpose:
## Assess the effect of different clinical sign ascertainment
## windows on recording completeness. Calculate daily and
## cumulative completeness, assess the relative contribution
## of individual days, and repeat the adjusted completeness
## analysis using a -7 to +7-day ascertainment window.
##
## Author: Dr William Elson
##
## Date created: 2026-09-07
##
## Copyright (c) William Elson, 2026
##
## Inputs:
## - episode_outcome: Episode-level dataset containing
##   patient and practice identifiers, episode dates,
##   clinical sign indicators and their recording dates,
##   ARI diagnoses, demographic characteristics and
##   clinical risk group indicators.
##
## Outputs:
## - Daily completeness for days -7 to +14.
## - Cumulative completeness from day -7 to each day.
## - Daily completeness relative to the full window.
## - Supplementary heatmaps saved to:
##   output/supplementary-figure-completeness-sensitivity-analysis-1.png
##   output/supplementary-figure-completeness-sensitivity-analysis-2.png
## - Logistic regression model using a -7 to +7-day window.
## - Adjusted odds ratios and 95% confidence intervals
##   using practice-level cluster-robust standard errors.
## - Sensitivity analysis forest plot saved to:
##   output/sensitivity-analysis-comp-forest-plot-7-7.png
##
## Notes:
## - Individual-level patient data are not publicly
##   available.
## - Data-loading code has been removed. The required
##   input dataset must be supplied before execution.
## - Ascertainment windows are defined relative to the
##   ARI episode start date.
## - The full ascertainment window covers days -7 to +14.
## - Daily relative completeness is calculated against
##   completeness across the full ascertainment window.
## - One episode per patient per respiratory season is
##   randomly selected for the regression analysis.
## - Respiratory seasons begin at ISO week 40.
## - Standard errors are clustered at practice level.
## - A random seed is set for reproducible episode selection.
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Load required packages -----------------------------------------------

library(data.table)  # Data manipulation and aggregation.
library(dplyr)       # Data preparation and manipulation of model results.
library(lubridate)   # Deriving ISO weeks and years from episode dates.
library(stringr)     # Processing regression coefficient labels.
library(ggplot2)     # Creating and saving heatmaps and forest plots.
library(lmtest)      # Calculating coefficient tests with robust SEs.

# The sandwich, tibble and grid packages are called explicitly
# using their package namespaces below.


# Load data -------------------------------------------------------------

# Data not available.
# The episode_outcome dataset must be supplied before
# running this script.

# Convert episode data to a data.table.
setDT(episode_outcome)


# Create output directory ----------------------------------------------

# Create the output directory if it does not exist.
dir.create(
  "output",
  showWarnings = FALSE,
  recursive = TRUE
)


# Define sensitivity analysis functions --------------------------------

# Specify clinical sign recording indicators.
signs <- c(
  "temp",
  "pulse_rate",
  "resp_rate",
  "sats",
  "bp",
  "any_sign"
)


# Recalculate clinical sign recording within a specified
# ascertainment window.
make_window_data <- function(
    DT,
    start_day,
    end_day,
    episode_date = "episode_date_min",
    signs = c(
      "temp",
      "pulse_rate",
      "resp_rate",
      "sats",
      "bp"
    )
) {
  
  # Check that the window is within the permitted range.
  if (start_day < -7 || end_day > 14) {
    stop("Window must be within -7 to +14 days.")
  }
  
  # Check that the start precedes or equals the end.
  if (start_day > end_day) {
    stop("start_day must be less than or equal to end_day.")
  }
  
  
  # Create a copy to avoid modifying the original dataset.
  out <- copy(DT)
  
  
  # Recalculate recording indicators for each clinical sign.
  for (s in signs) {
    
    # Identify the corresponding recording date variable.
    date_col <- paste0(s, "_date")
    
    # Determine whether the clinical sign was recorded
    # within the specified ascertainment window.
    out[
      ,
      (s) := as.integer(
        get(s) == 1 &
          !is.na(get(date_col)) &
          get(date_col) >= get(episode_date) + start_day &
          get(date_col) <= get(episode_date) + end_day
      )
    ]
    
  }
  
  
  # Recalculate the indicator for any clinical sign recorded.
  out[
    ,
    any_sign := as.integer(
      Reduce(`|`, .SD)
    ),
    .SDcols = signs
  ]
  
  
  # Return the dataset with recalculated indicators.
  return(out)
  
}


# Define function to summarise completeness ----------------------------

# Calculate recording completeness for a specified
# ascertainment window.
summarise_window <- function(
    DT,
    start_day,
    end_day,
    window_type
) {
  
  # Recalculate clinical sign indicators.
  tmp <- make_window_data(
    DT = DT,
    start_day = start_day,
    end_day = end_day
  )
  
  
  # Calculate the percentage of episodes with each sign recorded.
  out <- tmp[
    ,
    lapply(
      .SD,
      function(x) mean(x, na.rm = TRUE) * 100
    ),
    .SDcols = signs
  ]
  
  
  # Reshape the completeness summary into long format.
  out <- melt(
    out,
    measure.vars = signs,
    variable.name = "sign",
    value.name = "completeness"
  )
  
  
  # Add ascertainment window information.
  out[
    ,
    `:=`(
      window_type = window_type,
      start_day = start_day,
      end_day = end_day,
      day = end_day
    )
  ]
  
  
  # Return the completeness summary.
  return(out)
  
}


# Calculate daily completeness -----------------------------------------

# Calculate recording completeness for each individual day
# from seven days before to 14 days after the episode start.
daily_completeness <- rbindlist(
  lapply(
    -7:14,
    function(d) {
      
      print(d)
      
      summarise_window(
        DT = episode_outcome,
        start_day = d,
        end_day = d,
        window_type = "Individual day"
      )
      
    }
  )
)


# Calculate cumulative completeness ------------------------------------

# Calculate cumulative recording completeness from day -7
# to each subsequent day up to day +14.
cumulative_completeness <- rbindlist(
  lapply(
    -7:14,
    function(d) {
      
      print(d)
      
      summarise_window(
        DT = episode_outcome,
        start_day = -7,
        end_day = d,
        window_type = "Cumulative from day -7"
      )
      
    }
  )
)


# Calculate relative daily completeness --------------------------------

# Create a copy of the daily completeness dataset.
relative_daily <- copy(daily_completeness)


# Extract completeness across the full ascertainment window.
full_window <- cumulative_completeness[
  day == 14,
  .(
    sign,
    full_window_completeness = completeness
  )
]


# Add full-window completeness to each daily observation.
relative_daily[
  full_window,
  on = "sign",
  full_window_completeness := i.full_window_completeness
]


# Calculate daily completeness relative to the full window.
# Return NA if full-window completeness is zero.
relative_daily[
  ,
  completeness := fifelse(
    full_window_completeness > 0,
    100 * completeness / full_window_completeness,
    NA_real_
  )
]


# Assign the relative completeness category.
relative_daily[
  ,
  window_type := "Daily relative to full window"
]


# Remove the temporary denominator variable.
relative_daily[
  ,
  full_window_completeness := NULL
]


# Prepare completeness heatmap data ------------------------------------

# Combine daily and cumulative completeness summaries.
heatmap_data <- rbindlist(
  list(
    daily_completeness,
    cumulative_completeness
  ),
  use.names = TRUE,
  fill = TRUE
)


# Define clinical sign labels and ordering.
heatmap_data[
  ,
  sign := factor(
    sign,
    levels = rev(c(
      "temp",
      "pulse_rate",
      "resp_rate",
      "sats",
      "bp",
      "any_sign"
    )),
    labels = rev(c(
      "Temperature",
      "Pulse rate",
      "Respiratory rate",
      "O2 saturation",
      "Blood pressure",
      "Any sign"
    ))
  )
]


# Define the order of ascertainment window categories.
heatmap_data[
  ,
  window_type := factor(
    window_type,
    levels = c(
      "Individual day",
      "Cumulative from day -7"
    )
  )
]


# Create daily and cumulative completeness heatmap ---------------------

# Plot daily and cumulative recording completeness
# by clinical sign and day relative to the episode.
window_heatmap_daily_cumulative_completeness <- ggplot(
  heatmap_data,
  aes(
    x = day,
    y = sign,
    fill = completeness
  )
) +
  
  # Add heatmap tiles.
  geom_tile(
    color = "white",
    linewidth = 0.3
  ) +
  
  # Display completeness percentages within tiles.
  geom_text(
    aes(
      label = sprintf("%.1f", completeness)
    ),
    size = 2.5
  ) +
  
  # Separate daily and cumulative completeness.
  facet_wrap(
    ~ window_type,
    ncol = 1,
    strip.position = "top"
  ) +
  
  # Display all days in the ascertainment window.
  scale_x_continuous(
    breaks = -7:14
  ) +
  
  # Define the colour scale.
  scale_fill_gradient(
    name = "Completeness (%)",
    low = "white",
    high = "#CC3311"
  ) +
  
  # Define axis labels.
  labs(
    x = "Day relative to ARI episode",
    y = ""
  ) +
  
  # Apply plot theme and formatting.
  theme_bw() +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5
    ),
    
    strip.text = element_text(
      face = "bold"
    )
  )


# Save daily and cumulative completeness heatmap -----------------------

# Save the first supplementary sensitivity analysis figure.
ggsave(
  filename = paste0(
    "output/",
    "supplementary-figure-completeness-",
    "sensitivity-analysis-1.png"
  ),
  plot = window_heatmap_daily_cumulative_completeness
)


# Prepare relative completeness heatmap data ---------------------------

# Define clinical sign labels and ordering.
relative_daily[
  ,
  sign := factor(
    sign,
    levels = rev(c(
      "temp",
      "pulse_rate",
      "resp_rate",
      "sats",
      "bp",
      "any_sign"
    )),
    labels = rev(c(
      "Temperature",
      "Pulse rate",
      "Respiratory rate",
      "O2 saturation",
      "Blood pressure",
      "Any sign"
    ))
  )
]


# Create relative daily completeness heatmap ---------------------------

# Plot the contribution of each individual day relative
# to completeness across the full ascertainment window.
window_heatmap_daily_relative_contribution <- ggplot(
  relative_daily,
  aes(
    x = day,
    y = sign,
    fill = completeness
  )
) +
  
  # Add heatmap tiles.
  geom_tile(
    color = "white",
    linewidth = 0.3
  ) +
  
  # Display relative completeness percentages.
  geom_text(
    aes(
      label = sprintf("%.1f", completeness)
    ),
    size = 2.5
  ) +
  
  # Separate plots by ascertainment window category.
  facet_wrap(
    ~ window_type,
    ncol = 1,
    strip.position = "top"
  ) +
  
  # Display all days in the ascertainment window.
  scale_x_continuous(
    breaks = -7:14
  ) +
  
  # Define the colour scale.
  scale_fill_gradient(
    name = "Contribution (%)",
    low = "white",
    high = "#CC3311"
  ) +
  
  # Define axis labels.
  labs(
    x = "Day relative to ARI episode",
    y = ""
  ) +
  
  # Apply plot theme and formatting.
  theme_bw() +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5
    ),
    
    strip.text = element_text(
      face = "bold"
    )
  )


# Save relative completeness heatmap -----------------------------------

# Save the second supplementary sensitivity analysis figure.
ggsave(
  filename = paste0(
    "output/",
    "supplementary-figure-completeness-",
    "sensitivity-analysis-2.png"
  ),
  plot = window_heatmap_daily_relative_contribution,
  height = 3.5
)


# Model sensitivity analysis --------------------------------------------

# Recalculate clinical sign recording indicators using
# a -7 to +7-day ascertainment window.
eo_7_7 <- make_window_data(
  DT = episode_outcome,
  start_day = -7,
  end_day = 7
)


# Define respiratory season --------------------------------------------

# Extract ISO week and year from the episode start date.
eo_7_7[
  ,
  `:=`(
    episode_iso_week = isoweek(episode_date_min),
    episode_iso_year = isoyear(episode_date_min)
  )
]


# Assign respiratory season, beginning at ISO week 40.
eo_7_7[
  ,
  season := fifelse(
    episode_iso_week >= 40,
    paste0(
      episode_iso_year,
      "-",
      episode_iso_year + 1
    ),
    paste0(
      episode_iso_year - 1,
      "-",
      episode_iso_year
    )
  )
]


# Prepare model data ----------------------------------------------------

# Select required variables.
keepers <- c(
  "practice_id",
  "pseudo_id",
  "l2_diagnosis",
  "sex",
  "ethnicity_5p",
  "age_band_episode",
  "rg_resp",
  "rg_any",
  "temp",
  "pulse_rate",
  "resp_rate",
  "sats",
  "bp",
  "any_sign",
  "season"
)


# Define categorical variable levels.
# The first level of each factor is the reference category.
level_orders <- list(
  
  "ARI subtype" = c(
    "URTI", "LRTI", "ILI", "ECLD",
    "Sus COVID", "ARI NOS"
  ),
  
  "Age band" = c(
    "15 to 64yrs",
    "Under 1yr",
    "1 to 4yrs",
    "5 to 14yrs",
    "65yrs and over"
  ),
  
  "Sex" = c(
    "Female", "Male"
  ),
  
  "Ethnicity" = c(
    "Asian", "Black", "Mixed",
    "White", "Other", "Missing"
  ),
  
  "Any RG" = c(
    "No", "Yes"
  ),
  
  "Resp RG" = c(
    "No", "Yes"
  )
)


# Recode patient characteristics and ARI subtypes.
model_data_7_7 <- eo_7_7 %>%
  mutate(
    
    sex = case_when(
      sex == "M" ~ "Male",
      sex == "F" ~ "Female",
      .default = NA_character_
    ),
    
    ethnicity_5p = case_when(
      ethnicity_5p == "A" ~ "Asian",
      ethnicity_5p == "B" ~ "Black",
      ethnicity_5p == "M" ~ "Mixed",
      ethnicity_5p == "W" ~ "White",
      ethnicity_5p == "O" ~ "Other",
      is.na(ethnicity_5p) ~ "Missing"
    ),
    
    l2_diagnosis = case_when(
      l2_diagnosis == "l2_ecld" ~ "ECLD",
      l2_diagnosis == "l2_ili" ~ "ILI",
      l2_diagnosis == "l2_lrti" ~ "LRTI",
      l2_diagnosis == "l2_urti" ~ "URTI",
      l2_diagnosis == "l2_ari_nos" ~ "ARI NOS",
      l2_diagnosis == "l2_suscovid" ~ "Sus COVID"
    ),
    
    age_band_episode = case_when(
      age_band_episode == "1-4yrs" ~ "1 to 4yrs",
      age_band_episode == "5-14yrs" ~ "5 to 14yrs",
      age_band_episode == "15-64yrs" ~ "15 to 64yrs",
      age_band_episode == "65+yrs" ~ "65yrs and over",
      age_band_episode == "<1yr" ~ "Under 1yr"
    ),
    
    rg_any = ifelse(
      rg_any == 1,
      "Yes",
      "No"
    ),
    
    rg_resp = ifelse(
      rg_resp,
      "Yes",
      "No"
    ),
    
    sex = factor(
      sex,
      levels = level_orders$Sex
    ),
    
    age_band_episode = factor(
      age_band_episode,
      levels = level_orders$`Age band`
    ),
    
    ethnicity_5p = factor(
      ethnicity_5p,
      levels = level_orders$Ethnicity
    ),
    
    l2_diagnosis = factor(
      l2_diagnosis,
      levels = level_orders$`ARI subtype`
    ),
    
    rg_any = factor(
      rg_any,
      levels = level_orders$`Any RG`
    ),
    
    rg_resp = factor(
      rg_resp,
      levels = level_orders$`Resp RG`
    )
  ) %>%
  select(
    all_of(keepers)
  )


# Assign descriptive variable names.
names(model_data_7_7) <- c(
  "practice_id",
  "pseudo_id",
  "ARI subtype",
  "Sex",
  "Ethnicity",
  "Age band",
  "Resp risk group",
  "Any risk group",
  "Temp",
  "Pulse rate",
  "Resp rate",
  "O2 sats",
  "BP",
  "Any sign",
  "Season"
)


# Convert model data to a data.table.
setDT(model_data_7_7)


# Select episodes for regression ---------------------------------------

# Set the seed for reproducible episode selection.
set.seed(42)


# Randomly select one episode per patient per respiratory season.
rnd_rows <- model_data_7_7[
  ,
  .I[sample.int(.N, 1L)],
  by = .(
    practice_id,
    pseudo_id,
    Season
  )
]


# Create the sampled analysis dataset.
sample_data_7_7 <- model_data_7_7[
  rnd_rows$V1
]


# Restrict to complete observations for the model variables
# and the practice identifier used for clustered standard errors.
model_vars <- c(
  "practice_id",
  "Any sign",
  "ARI subtype",
  "Age band",
  "Sex",
  "Ethnicity",
  "Any risk group"
)


sample_data_7_7 <- sample_data_7_7[
  complete.cases(
    sample_data_7_7[, ..model_vars]
  )
]


# Fit logistic regression model ----------------------------------------

# Fit a multivariable logistic regression model for
# any clinical sign recorded within the -7 to +7-day window.
completeness_mod_any_sign_7_7 <- glm(
  `Any sign` ~
    `ARI subtype` +
    `Age band` +
    Sex +
    Ethnicity +
    `Any risk group`,
  family = binomial,
  data = sample_data_7_7
)


# Calculate cluster-robust standard errors ------------------------------

# Calculate the variance-covariance matrix with clustering
# at practice level.
vcov_practice <- sandwich::vcovCL(
  completeness_mod_any_sign_7_7,
  cluster = sample_data_7_7$practice_id,
  type = "HC1"
)


# Obtain coefficient estimates with cluster-robust standard errors.
robust_result <- coeftest(
  completeness_mod_any_sign_7_7,
  vcov. = vcov_practice
)


# Display the cluster-robust regression results.
print(robust_result)


# Construct regression results -----------------------------------------

# Extract regression coefficients.
beta <- coef(
  completeness_mod_any_sign_7_7
)


# Extract cluster-robust standard errors.
se <- sqrt(
  diag(vcov_practice)
)


# Calculate adjusted odds ratios, confidence intervals and p-values.
results <- tibble::tibble(
  term = names(beta),
  
  estimate = exp(beta),
  
  conf.low = exp(
    beta - 1.96 * se
  ),
  
  conf.high = exp(
    beta + 1.96 * se
  ),
  
  p_value = 2 * pnorm(
    abs(beta / se),
    lower.tail = FALSE
  )
)


# Prepare forest plot data ----------------------------------------------

# Create reference category rows.
reference_rows <- tibble::tibble(
  
  term = c(
    "URTI",
    "15 to 64yrs",
    "Female",
    "Asian",
    "No risk group"
  ),
  
  estimate = 1,
  conf.low = 1,
  conf.high = 1,
  p_value = NA_real_,
  
  cov_group = c(
    "ARI subtype",
    "Age band",
    "Sex",
    "Ethnicity",
    "Risk group"
  )
)


# Format regression results for the forest plot.
completeness_forest_plot_df_7_7 <- results %>%
  
  # Identify the covariate group for each coefficient.
  mutate(
    cov_group = case_when(
      str_detect(term, "Age") ~ "Age band",
      str_detect(term, "Ethnicity") ~ "Ethnicity",
      str_detect(term, "risk") ~ "Risk group",
      str_detect(term, "Sex") ~ "Sex",
      str_detect(term, "subtype") ~ "ARI subtype"
    )
  ) %>%
  
  # Remove variable names from coefficient labels.
  mutate(
    term = str_remove(term, "`ARI subtype`"),
    term = str_remove(term, "`Age band`"),
    term = str_remove(term, "Ethnicity"),
    term = str_remove(term, "Sex"),
    term = str_remove(term, "`Any risk group`")
  ) %>%
  
  # Assign descriptive category labels.
  mutate(
    term = case_when(
      term == "Yes" ~ "In risk group",
      TRUE ~ term
    )
  ) %>%
  
  # Remove the intercept.
  filter(
    term != "(Intercept)"
  ) %>%
  
  # Create formatted odds ratio and confidence interval labels.
  mutate(
    CI_label = paste0(
      sprintf("%.2f", estimate),
      " (",
      sprintf("%.2f", conf.low),
      "-",
      sprintf("%.2f", conf.high),
      ")"
    )
  ) %>%
  
  # Add reference categories.
  bind_rows(reference_rows) %>%
  
  # Define the order of variables in the forest plot.
  mutate(
    term = factor(
      term,
      levels = rev(c(
        "URTI",
        "LRTI",
        "ILI",
        "ECLD",
        "Sus COVID",
        "ARI NOS",
        
        "15 to 64yrs",
        "Under 1yr",
        "1 to 4yrs",
        "5 to 14yrs",
        "65yrs and over",
        
        "Female",
        "Male",
        
        "Asian",
        "Black",
        "Mixed",
        "White",
        "Other",
        "Missing",
        
        "No risk group",
        "In risk group"
      ))
    ),
    
    cov_group = factor(
      cov_group,
      levels = c(
        "ARI subtype",
        "Age band",
        "Sex",
        "Ethnicity",
        "Risk group"
      )
    )
  ) %>%
  
  # Label reference categories.
  mutate(
    CI_label = ifelse(
      is.na(p_value),
      "1.00 (Reference)",
      CI_label
    )
  )


# Define forest plot layout --------------------------------------------

# Calculate the number of terms.
n_terms <- nlevels(
  completeness_forest_plot_df_7_7$term
)


# Define minor and major separator positions.
minor_lines <- seq(
  0.5,
  n_terms + 0.5,
  by = 1
)


major_lines <- c(
  2.5,
  8.5,
  10.5,
  15.5
)


# Define the position of the confidence interval header.
header_x <- n_terms + 0.75


# Create sensitivity analysis forest plot -------------------------------

# Plot adjusted odds ratios and 95% confidence intervals.
completeness_forest_plot_7_7 <- completeness_forest_plot_df_7_7 %>%
  
  ggplot(
    aes(
      x = term,
      y = estimate,
      color = cov_group,
      group = cov_group
    )
  ) +
  
  # Plot adjusted odds ratio estimates.
  geom_point(
    size = 2
  ) +
  
  # Add 95% confidence intervals.
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    color = "black",
    width = 0.1,
    linewidth = 0.3
  ) +
  
  # Add the null-effect reference line.
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "red"
  ) +
  
  # Add minor separators between terms.
  geom_vline(
    xintercept = minor_lines,
    color = "gray",
    linewidth = 0.1
  ) +
  
  # Add major separators between covariate groups.
  geom_vline(
    xintercept = major_lines,
    color = "darkgrey",
    linewidth = 0.5
  ) +
  
  # Add formatted odds ratios and confidence intervals.
  geom_text(
    aes(
      label = CI_label,
      y = 2
    ),
    hjust = -0.1,
    size = 2.5,
    color = "black"
  ) +
  
  # Add the confidence interval column header.
  annotate(
    "text",
    x = header_x,
    y = 2.25,
    label = "OR (95% CI)",
    size = 3
  ) +
  
  # Rotate the plot and allow labels outside the plotting area.
  coord_flip(
    clip = "off"
  ) +
  
  # Define colours for covariate groups.
  scale_color_manual(
    values = c(
      "#D55E00",
      "#4DA3D9",
      "#E69F00",
      "#009E73",
      "#CC79A7"
    )
  ) +
  
  # Define plot title and axis labels.
  labs(
    title = "Sensitivity analysis: -7 to +7-day window",
    subtitle = "",
    x = "",
    y = "Odds ratio (OR) for sign recording",
    color = ""
  ) +
  
  # Apply plot theme and formatting.
  theme_minimal() +
  
  theme(
    axis.ticks.y = element_blank(),
    
    plot.margin = grid::unit(
      c(1, 4, 1, 1),
      "cm"
    ),
    
    plot.title = element_text(
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    
    plot.background = element_rect(
      fill = "white"
    ),
    
    axis.title.x = element_text(
      hjust = 0.35
    ),
    
    legend.position = "top",
    
    legend.title = element_text(
      size = 9
    ),
    
    legend.text = element_text(
      size = 8
    ),
    
    legend.key.size = grid::unit(
      0.5,
      "lines"
    ),
    
    legend.margin = margin(
      b = 15
    ),
    
    panel.grid = element_blank()
  )


# Save sensitivity analysis forest plot --------------------------------

# Save the -7 to +7-day sensitivity analysis figure.
ggsave(
  filename = "output/sensitivity-analysis-comp-forest-plot-7-7.png",
  plot = completeness_forest_plot_7_7,
  dpi = 300,
  width = 6,
  height = 7
)
