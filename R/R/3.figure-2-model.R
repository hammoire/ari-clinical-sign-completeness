##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: ARI data quality adjusted completeness analysis
##
## Purpose:
## Investigate associations between patient characteristics,
## ARI subtype and clinical sign recording using multivariable
## logistic regression. Generate a forest plot of adjusted
## odds ratios and 95% confidence intervals.
##
## Author: Dr William Elson
##
## Date created: 2025-03-11
##
## Copyright (c) William Elson, 2025
##
## Inputs:
## - episode_outcome: Episode-level dataset containing
##   patient and practice identifiers, episode dates,
##   ARI diagnoses, demographic characteristics,
##   clinical risk group indicators and clinical sign
##   recording variables.
##
## Outputs:
## - Multivariable logistic regression model for the
##   recording of any clinical sign.
## - Variance inflation factors.
## - Regression coefficients with practice-level
##   cluster-robust standard errors.
## - Adjusted odds ratios and 95% confidence intervals.
## - Forest plot saved to:
##   output/figure-2-comp-forest-plot.png
##
## Notes:
## - Individual-level patient data are not publicly
##   available.
## - Data-loading code has been removed. The required
##   input dataset must be supplied before execution.
## - One episode per patient per respiratory season is
##   randomly selected for the analysis.
## - Respiratory seasons begin at ISO week 40.
## - The model adjusts for ARI subtype, age band, sex,
##   ethnicity and any clinical risk group.
## - Standard errors are clustered at practice level.
## - A random seed is set to ensure reproducibility
##   of episode selection.
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Load required packages -----------------------------------------------

library(data.table)  # Data manipulation and random episode selection.
library(lubridate)   # Deriving ISO weeks and years from episode dates.
library(dplyr)       # Data manipulation and preparation of model results.
library(stringr)     # Processing regression coefficient labels.
library(ggplot2)     # Creating and saving the forest plot.
library(grid)        # Specifying plot dimensions and margins.
library(lmtest)      # Calculating coefficient tests with robust SEs.
library(car)         # Calculating variance inflation factors.

# The sandwich and tibble packages are called explicitly
# using their package namespaces below.


# Load data -------------------------------------------------------------

# Data not available.
# The episode_outcome dataset must be supplied before
# running this script.

# Convert episode data to a data.table.
setDT(episode_outcome)


# Define respiratory season --------------------------------------------

# Extract ISO week and year from the episode start date.
episode_outcome[
  ,
  `:=`(
    episode_iso_week = isoweek(episode_date_min),
    episode_iso_year = isoyear(episode_date_min)
  )
]


# Assign respiratory season, beginning at ISO week 40.
episode_outcome[
  ,
  season := fifelse(
    episode_iso_week >= 40,
    paste0(episode_iso_year, "-", episode_iso_year + 1),
    paste0(episode_iso_year - 1, "-", episode_iso_year)
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


# Define the order of categorical variable levels.
# The first level of each factor is the reference category.
level_orders <- list(
  "ARI subtype" = c(
    "URTI", "LRTI", "ILI", "ECLD", "Sus COVID", "ARI NOS"
  ),
  
  "Age band" = c(
    "15 to 64yrs", "<1yr", "1 to 4yrs",
    "5 to 14yrs", "≥65yrs"
  ),
  
  "Sex" = c(
    "Female", "Male"
  ),
  
  "Ethnicity" = c(
    "Asian", "Black", "Mixed", "White", "Other", "Missing"
  ),
  
  "Any RG" = c(
    "No", "Yes"
  ),
  
  "Resp RG" = c(
    "No", "Yes"
  )
)


# Recode patient characteristics and ARI subtypes.
model_data <- episode_outcome %>%
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
      age_band_episode == "65+yrs" ~ "≥65yrs",
      age_band_episode == "<1yr" ~ "<1yr"
    ),
    
    rg_any = ifelse(rg_any == 1, "Yes", "No"),
    rg_resp = ifelse(rg_resp, "Yes", "No"),
    
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
  select(all_of(keepers))


# Assign descriptive variable names.
names(model_data) <- c(
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
setDT(model_data)


# Select episodes for regression ---------------------------------------

# Set seed for reproducible random episode selection.
set.seed(42)


# Randomly select one episode per patient per respiratory season.
rnd_rows <- model_data[
  ,
  .I[sample.int(.N, 1L)],
  by = .(practice_id, pseudo_id, Season)
]


# Create the sampled analysis dataset.
sample_data <- model_data[rnd_rows$V1]


# Restrict to complete observations for variables used in the model
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

sample_data <- sample_data[
  complete.cases(sample_data[, ..model_vars])
]


# Logistic regression ---------------------------------------------------

# Fit a multivariable logistic regression model for
# the recording of any clinical sign.
completeness_mod_any_sign <- glm(
  `Any sign` ~ `ARI subtype` +
    `Age band` +
    Sex +
    Ethnicity +
    `Any risk group`,
  family = binomial,
  data = sample_data
)


# Assess multicollinearity ----------------------------------------------

# Calculate variance inflation factors.
vif_results <- car::vif(completeness_mod_any_sign)


# Calculate cluster-robust standard errors ------------------------------

# Calculate the variance-covariance matrix with clustering
# at practice level.
vcov_practice <- sandwich::vcovCL(
  completeness_mod_any_sign,
  cluster = sample_data$practice_id,
  type = "HC1"
)


# Obtain coefficient estimates with cluster-robust standard errors.
robust_result <- coeftest(
  completeness_mod_any_sign,
  vcov. = vcov_practice
)


# Construct regression results -----------------------------------------

# Extract regression coefficients and robust standard errors.
beta <- coef(completeness_mod_any_sign)

se <- sqrt(diag(vcov_practice))


# Calculate adjusted odds ratios, 95% confidence intervals
# and p-values using cluster-robust standard errors.
results <- tibble::tibble(
  term = names(beta),
  estimate = exp(beta),
  conf.low = exp(beta - 1.96 * se),
  conf.high = exp(beta + 1.96 * se),
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
completeness_forest_plot_df <- results %>%
  
  # Identify the covariate group for each model coefficient.
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
  
  # Assign descriptive labels.
  mutate(
    term = case_when(
      term == "Yes" ~ "In risk group",
      TRUE ~ term
    )
  ) %>%
  
  # Remove the intercept.
  filter(term != "(Intercept)") %>%
  
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
        "<1yr",
        "1 to 4yrs",
        "5 to 14yrs",
        "≥65yrs",
        
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
      estimate == 1 & conf.low == 1,
      "1.00 (Reference)",
      CI_label
    )
  )


# Define forest plot layout --------------------------------------------

# Calculate the number of terms.
n_terms <- nlevels(completeness_forest_plot_df$term)


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


# Create forest plot ----------------------------------------------------

# Plot adjusted odds ratios and 95% confidence intervals.
completeness_forest_plot <- completeness_forest_plot_df %>%
  
  ggplot(
    aes(
      x = term,
      y = estimate,
      color = cov_group,
      group = cov_group
    )
  ) +
  
  # Plot adjusted odds ratio estimates.
  geom_point(size = 2) +
  
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
  
  # Add minor separators between individual terms.
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
      y = 1.8
    ),
    hjust = -0.1,
    size = 2.5,
    color = "black"
  ) +
  
  # Add the confidence interval column header.
  annotate(
    "text",
    x = header_x,
    y = 2,
    label = "OR (95% CI)",
    size = 3
  ) +
  
  # Rotate the plot and allow labels outside the plotting area.
  coord_flip(clip = "off") +
  
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
  
  # Define axis labels.
  labs(
    subtitle = "",
    x = "",
    y = "Odds ratio (OR) for sign recording",
    color = ""
  ) +
  
  # Apply plot theme and formatting.
  theme_minimal() +
  
  theme(
    axis.ticks.y = element_blank(),
    
    plot.margin = unit(
      c(1, 1, 1, 1),
      "cm"
    ),
    
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    
    plot.background = element_rect(
      fill = "white"
    ),
    
    axis.title.x = element_text(hjust = 0.35),
    
    legend.position = "top",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    
    legend.key.size = unit(
      0.5,
      "lines"
    ),
    
    legend.margin = margin(b = 15),
    
    panel.grid = element_blank()
  )


# Save forest plot ------------------------------------------------------

# Create the output directory if it does not exist.
dir.create(
  "output",
  showWarnings = FALSE,
  recursive = TRUE
)


# Save Figure 2 as a high-resolution PNG.
ggsave(
  filename = "output/figure-2-comp-forest-plot.png",
  plot = completeness_forest_plot,
  dpi = 300,
  width = 6,
  height = 7
)