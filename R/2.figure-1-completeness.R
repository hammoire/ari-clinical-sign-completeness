##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: ARI data quality heatmap
##
## Purpose:
## Calculate the percentage of ARI episodes with clinical
## signs recorded and generate a heatmap showing recording
## completeness overall and by patient characteristics,
## ARI subtype and clinical risk group.
##
## Author: Dr William Elson
##
## Date created: 2025-03-11
##
## Copyright (c) William Elson, 2025
##
## Inputs:
## - episode_outcome: Episode-level dataset containing
##   ARI diagnoses, demographic characteristics, clinical
##   risk group indicators and clinical sign variables.
##
## Outputs:
## - Summary of clinical sign recording completeness
##   across patient groups and ARI subtypes.
## - Heatmap of clinical sign recording completeness.
## - Figure saved to:
##   output/figure-1-completeness.png
##
## Notes:
## - Individual-level patient data are not publicly
##   available.
## - Data-loading code has been removed. The required
##   input dataset must be supplied before execution.
## - Completeness is calculated as the percentage of
##   episodes with each clinical sign recorded.
## - Missing values in clinical sign indicators are
##   excluded from percentage calculations.
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Load required packages -----------------------------------------------

library(dplyr)    # Data manipulation and summarisation.
library(tidyr)    # Reshaping data into long format.
library(purrr)    # Applying functions across stratification variables.
library(ggplot2)  # Creating and saving the heatmap.


# Load data -------------------------------------------------------------

# Data not available.
# The episode_outcome dataset must be supplied before
# running this script.


# Prepare data ----------------------------------------------------------

# Recode demographic characteristics and ARI subtypes.
completeness_data <- episode_outcome %>%
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
      l2_diagnosis == "l2_suscovid" ~ "Sus COVID",
      l2_diagnosis == "l2_ari_nos" ~ "ARI NOS"
    ),
    
    age_band_episode = case_when(
      age_band_episode == "1-4yrs" ~ "1 to 4yrs",
      age_band_episode == "5-14yrs" ~ "5 to 14yrs",
      age_band_episode == "15-64yrs" ~ "15 to 64yrs",
      age_band_episode == "65+yrs" ~ "\u226565yrs",
      age_band_episode == "<1yr" ~ "<1yr"
    )
  )


# Check the distribution of episodes by age band.
table(completeness_data$age_band_episode)


# Summarise recording completeness -------------------------------------

# Define clinical sign variables.
sign_vars <- c(
  "temp",
  "pulse_rate",
  "resp_rate",
  "sats",
  "bp",
  "any_sign"
)


# Define variables used to stratify completeness.
strat_vars <- c(
  "sex",
  "age_band_episode",
  "ethnicity_5p",
  "l2_diagnosis",
  "rg_resp",
  "rg_any"
)


# Calculate recording completeness by patient characteristics,
# ARI subtype and clinical risk group.
summary_completeness_groups <- map_dfr(strat_vars, function(v) {
  
  completeness_data %>%
    group_by(.data[[v]]) %>%
    summarise(
      across(
        all_of(sign_vars),
        ~ mean(.x, na.rm = TRUE) * 100
      ),
      .groups = "drop"
    ) %>%
    rename(level = all_of(v)) %>%
    mutate(
      group = v,
      level = as.character(level)
    ) %>%
    select(group, level, everything())
  
})


# Calculate recording completeness across all ARI episodes.
summary_completeness_all_ari <- completeness_data %>%
  summarise(
    across(
      all_of(sign_vars),
      ~ mean(.x, na.rm = TRUE) * 100
    )
  ) %>%
  mutate(
    group = "l2_diagnosis",
    level = "All ARI"
  ) %>%
  select(group, level, everything())


# Combine stratified and overall completeness summaries.
summary_completeness <- bind_rows(
  summary_completeness_groups,
  summary_completeness_all_ari
)


# Prepare heatmap data --------------------------------------------------

# Define the order of groups in the heatmap.
group_order <- c(
  "ARI subtype",
  "Sex",
  "Age band",
  "Ethnicity",
  "Any risk group",
  "Resp risk group"
)


# Define the order of levels within each group.
level_orders <- list(
  "ARI subtype" = rev(c(
    "All ARI", "URTI", "LRTI", "ILI", "ECLD",
    "Sus COVID", "ARI NOS"
  )),
  
  "Sex" = rev(c(
    "Female", "Male"
  )),
  
  "Age band" = rev(c(
    "<1yr", "1 to 4yrs", "5 to 14yrs",
    "15 to 64yrs", "\u226565yrs"
  )),
  
  "Ethnicity" = rev(c(
    "Asian", "Black", "Mixed", "White",
    "Other", "Missing"
  )),
  
  "Any risk group" = rev(c(
    "Risk group", "No risk group"
  )),
  
  "Resp risk group" = rev(c(
    "Resp risk group", "No resp risk group"
  ))
)


# Combine level orders for use in the heatmap.
level_order_global <- unlist(level_orders[group_order])


# Reshape completeness summaries into long format.
summary_completeness_long <- summary_completeness %>%
  pivot_longer(
    cols = all_of(sign_vars),
    names_to = "sign",
    values_to = "completeness"
  ) %>%
  mutate(
    
    # Convert group levels to character.
    level = as.character(level),
    
    # Recode risk group indicators.
    level = case_when(
      group == "rg_any" &
        level %in% c("1", "TRUE") ~ "Risk group",
      
      group == "rg_any" &
        level %in% c("0", "FALSE") ~ "No risk group",
      
      group == "rg_resp" &
        level %in% c("1", "TRUE") ~ "Resp risk group",
      
      group == "rg_resp" &
        level %in% c("0", "FALSE") ~ "No resp risk group",
      
      TRUE ~ level
    ),
    
    # Assign descriptive group labels.
    group = case_when(
      group == "sex" ~ "Sex",
      group == "age_band_episode" ~ "Age band",
      group == "ethnicity_5p" ~ "Ethnicity",
      group == "rg_resp" ~ "Resp risk group",
      group == "rg_any" ~ "Any risk group",
      group == "l2_diagnosis" ~ "ARI subtype",
      TRUE ~ group
    ),
    
    # Assign descriptive clinical sign labels.
    sign = case_when(
      sign == "temp" ~ "Temp",
      sign == "pulse_rate" ~ "Pulse rate",
      sign == "resp_rate" ~ "Resp rate",
      sign == "sats" ~ "O2 sats",
      sign == "bp" ~ "BP",
      sign == "any_sign" ~ "Any sign",
      TRUE ~ sign
    )
  ) %>%
  mutate(
    group = factor(group, levels = group_order),
    level = factor(level, levels = level_order_global)
  )


# Create heatmap --------------------------------------------------------

# Plot recording completeness by clinical sign and patient group.
heatmap_completeness <- summary_completeness_long %>%
  ggplot(aes(
    x = sign,
    y = level,
    fill = completeness
  )) +
  
  # Add heatmap tiles.
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  
  # Display completeness percentages within tiles.
  geom_text(
    aes(label = paste0(round(completeness), "%"))
  ) +
  
  # Define the colour scale.
  scale_fill_gradient(
    name = "Completeness",
    low = "white",
    high = "#CC3311"
  ) +
  
  # Separate the heatmap by patient characteristic.
  facet_grid(
    group ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y",
    labeller = labeller(
      group = c(
        "Any risk group" = "Any\nrisk group",
        "Resp risk group" = "Resp\nrisk group"
      )
    )
  ) +
  
  # Position axis labels.
  scale_y_discrete(position = "right") +
  scale_x_discrete(position = "top") +
  
  # Apply plot theme and formatting.
  theme_bw(base_size = 12) +
  
  labs(
    x = "",
    y = ""
  ) +
  
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 90),
    strip.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )


# Save heatmap ----------------------------------------------------------

# Create the output directory if it does not exist.
dir.create(
  "output",
  showWarnings = FALSE,
  recursive = TRUE
)


# Save Figure 1 as a high-resolution PNG.
ggsave(
  filename = "output/figure-1-completeness.png",
  plot = heatmap_completeness,
  dpi = 300,
  height = 10,
  width = 7
)
