##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: Temporal dynamics time series plots
##
## Purpose:
## Investigate temporal changes in ARI episode counts and
## clinical sign recording completeness. Generate Figure 3,
## showing weekly episode counts and recording completeness,
## and Table 2, summarising recording completeness by
## respiratory season.
##
## Author: Dr William Elson
##
## Date created: 2025-08-08
##
## Copyright (c) William Elson, 2025
##
## Inputs:
## - episode_outcome_raw: Episode-level dataset containing
##   episode dates and clinical sign recording indicators
##   for temperature, pulse rate, respiratory rate,
##   oxygen saturation and blood pressure.
##
## Outputs:
## - Weekly ARI episode counts and clinical sign recording
##   completeness.
## - Figure 3: Time series plots of weekly episode counts
##   and clinical sign recording completeness.
## - Figure saved to:
##   output/figure-3-ts-combined.png
## - Table 2: Summary of recording completeness by
##   respiratory season, stored as season_summary.
##
## Notes:
## - Individual-level patient data are not publicly
##   available.
## - Data-loading code has been removed. The required
##   input dataset must be supplied before execution.
## - Recording completeness is calculated as the
##   percentage of episodes with a clinical sign recorded.
## - Respiratory seasons begin at ISO week 40.
## - The week commencing 29 September 2024 is excluded
##   from Figure 3 but retained in the seasonal analysis.
## - The SARS-CoV-2 pandemic period is highlighted in
##   Figure 3 using predefined dates.
## - Table 2 is generated in R but is not exported.
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Load required packages -----------------------------------------------

library(data.table)  # Efficient data manipulation and aggregation.
library(lubridate)   # Deriving weeks, years and seasons from dates.
library(dplyr)       # Data manipulation and summarisation.
library(tidyr)       # Reshaping data into long format.
library(ggplot2)     # Creating and saving time series plots.
library(cowplot)     # Combining individual plots into one figure.

# The scales and grid packages are called explicitly
# using their package namespaces below.


# Load data -------------------------------------------------------------

# Data not available.
# The episode_outcome_raw dataset must be supplied before
# running this script.


# Prepare episode data --------------------------------------------------

# Create an indicator for the recording of any clinical sign.
episode_outcome <- episode_outcome_raw %>%
  mutate(
    any_sign = p_sign_temp_rec |
      p_sign_pr_rec |
      p_sign_rr_rec |
      p_sign_sats_rec |
      p_sign_bp_rec
  )


# Convert episode data to a data.table.
setDT(episode_outcome)


# Define recording completeness variables ------------------------------

# Specify clinical sign recording indicators.
rec_rate_cols <- c(
  "any_sign",
  "p_sign_temp_rec",
  "p_sign_pr_rec",
  "p_sign_rr_rec",
  "p_sign_sats_rec",
  "p_sign_bp_rec"
)


# Define respiratory seasons -------------------------------------------

# Extract ISO week and weekly date from the episode start date.
episode_outcome[
  ,
  `:=`(
    episode_iso_week = isoweek(episode_date_min),
    episode_week = floor_date(
      episode_date_min,
      unit = "week"
    )
  )
]


# Determine the starting year of the respiratory season.
# Use the calendar year to avoid misclassification around
# the ISO year boundary in late December and early January.
episode_outcome[
  ,
  season_start_year := fifelse(
    month(episode_date_min) == 1L,
    year(episode_date_min) - 1L,
    fifelse(
      episode_iso_week >= 40L |
        month(episode_date_min) == 12L,
      year(episode_date_min),
      year(episode_date_min) - 1L
    )
  )
]


# Assign respiratory season labels.
episode_outcome[
  ,
  season := paste0(
    season_start_year,
    "-",
    season_start_year + 1L
  )
]


# Remove the temporary season variable.
episode_outcome[
  ,
  season_start_year := NULL
]


# Figure 3: Weekly recording completeness -------------------------------

# Calculate weekly episode counts.
si_weekly_denominator <- episode_outcome[
  ,
  .(episodes_n = .N),
  by = .(episode_week)
]


# Calculate the weekly number of episodes with each
# clinical sign recorded.
si_weekly_numerator <- episode_outcome[
  ,
  lapply(.SD, sum),
  by = .(episode_week),
  .SDcols = rec_rate_cols
] %>%
  pivot_longer(
    cols = all_of(rec_rate_cols),
    names_to = "severity_marker",
    values_to = "freq"
  )


# Combine weekly numerators and denominators.
si_weekly_raw <- merge(
  si_weekly_denominator,
  si_weekly_numerator,
  by = "episode_week",
  all = FALSE
)


# Calculate weekly recording completeness as a percentage.
si_weekly_raw[
  ,
  pc := 100 * (freq / episodes_n)
]


# Prepare weekly data for plotting -------------------------------------

# Exclude the final incomplete week and assign
# descriptive clinical sign labels.
si_weekly <- si_weekly_raw %>%
  filter(
    episode_week != as.Date("2024-09-29")
  ) %>%
  mutate(
    severity_marker = case_when(
      severity_marker == "any_sign" ~ "Any sign",
      severity_marker == "p_sign_temp_rec" ~ "Temp",
      severity_marker == "p_sign_pr_rec" ~ "Pulse rate",
      severity_marker == "p_sign_rr_rec" ~ "Resp rate",
      severity_marker == "p_sign_sats_rec" ~ "O2 sats",
      severity_marker == "p_sign_bp_rec" ~ "BP",
      TRUE ~ severity_marker
    )
  ) %>%
  select(
    episode_week,
    severity_marker,
    episodes_n,
    freq,
    pc
  )


# Define pandemic annotation dates -------------------------------------

# Define the SARS-CoV-2 pandemic period used for plot shading.
pandemic_start_cov <- as.Date("2020-01-01")
pandemic_end_cov <- as.Date("2022-09-30")


# Calculate the midpoint for the pandemic annotation.
pandemic_mid_cov <- as.Date(
  (
    as.numeric(pandemic_start_cov) +
      as.numeric(pandemic_end_cov)
  ) / 2,
  origin = "1970-01-01"
)


# Plot weekly episode counts -------------------------------------------

# Prepare data for plotting total episodes and episodes
# with at least one clinical sign recorded.
episode_recording_plot <- si_weekly %>%
  filter(
    severity_marker == "Any sign"
  ) %>%
  mutate(
    `Episodes (n)` = episodes_n,
    `Sign recorded` = freq
  ) %>%
  pivot_longer(
    cols = c(
      "Episodes (n)",
      "Sign recorded"
    ),
    names_to = "parameter",
    values_to = "n"
  ) %>%
  
  ggplot(
    aes(
      x = episode_week,
      y = n,
      color = parameter
    )
  ) +
  
  # Highlight the SARS-CoV-2 pandemic period.
  annotate(
    "rect",
    xmin = pandemic_start_cov,
    xmax = pandemic_end_cov,
    ymin = -Inf,
    ymax = Inf,
    fill = "#FFF5E6"
  ) +
  
  # Add the pandemic annotation.
  annotate(
    "text",
    x = pandemic_mid_cov,
    y = 75000,
    label = "SARS-CoV-2\npandemic",
    size = 3.5,
    color = "#B2182B",
    fontface = "bold"
  ) +
  
  # Plot weekly episode counts.
  geom_line(
    linewidth = 0.4
  ) +
  
  # Define line colours.
  scale_color_manual(
    values = c(
      "#D55E00",
      "#0072B2"
    )
  ) +
  
  # Define the date axis.
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  
  # Display episode counts in thousands.
  scale_y_continuous(
    labels = scales::label_number(
      scale = 1e-3
    )
  ) +
  
  # Define axis and legend labels.
  labs(
    x = "Episode date",
    y = "Frequency (1000s)",
    color = ""
  ) +
  
  # Apply plot theme and formatting.
  theme_bw() +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    panel.grid.minor.x = element_line(
      linetype = "dashed"
    ),
    
    legend.position = c(0.085, 0.85),
    
    legend.background = element_rect(
      fill = NA,
      color = NA
    ),
    
    legend.key = element_rect(
      fill = NA,
      color = NA
    ),
    
    legend.text = element_text(size = 6.5),
    
    legend.key.size = grid::unit(
      12.5,
      "pt"
    )
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 1,
        size = 1
      )
    )
  )


# Plot weekly recording completeness -----------------------------------

# Plot the percentage of episodes with clinical signs recorded.
rate_plot <- si_weekly %>%
  
  ggplot(
    aes(
      x = episode_week,
      y = pc,
      color = severity_marker
    )
  ) +
  
  # Highlight the SARS-CoV-2 pandemic period.
  annotate(
    "rect",
    xmin = pandemic_start_cov,
    xmax = pandemic_end_cov,
    ymin = -Inf,
    ymax = Inf,
    fill = "#FFF5E6"
  ) +
  
  # Plot weekly recording completeness.
  geom_line(
    linewidth = 0.4
  ) +
  
  # Define the date axis.
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  
  # Define line colours.
  scale_color_manual(
    values = c(
      "black",
      "#D55E00",
      "#0072B2",
      "#E69F00",
      "#CC79A7",
      "#009E73"
    )
  ) +
  
  # Define axis and legend labels.
  labs(
    x = "Date (week)",
    y = "Completeness (%)",
    color = ""
  ) +
  
  # Apply plot theme and formatting.
  theme_bw() +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    
    axis.ticks.x = element_blank(),
    
    panel.grid.minor.x = element_line(
      linetype = "dashed"
    ),
    
    legend.position = c(0.1, 0.7),
    
    legend.background = element_rect(
      fill = NA,
      color = NA
    ),
    
    legend.key = element_rect(
      fill = NA,
      color = NA
    ),
    
    legend.text = element_text(size = 7),
    
    legend.key.size = grid::unit(
      12.5,
      "pt"
    )
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 1,
        size = 1
      )
    )
  )


# Combine Figure 3 panels ----------------------------------------------

# Arrange the episode count and completeness plots vertically.
combined_plots <- plot_grid(
  episode_recording_plot,
  rate_plot,
  ncol = 1
)


# Save Figure 3 ---------------------------------------------------------

# Create the output directory if it does not exist.
dir.create(
  "output",
  showWarnings = FALSE,
  recursive = TRUE
)


# Save Figure 3 as a high-resolution PNG.
ggsave(
  filename = "output/figure-3-ts-combined.png",
  plot = combined_plots,
  dpi = 300,
  width = 8.5,
  height = 6
)


# Table 2: Seasonal recording completeness ------------------------------

# Calculate episode counts by respiratory season and ISO week.
si_season_denominator <- episode_outcome[
  ,
  .(episodes_n = .N),
  by = .(season, episode_iso_week)
]


# Calculate the number of episodes with each clinical
# sign recorded by respiratory season and ISO week.
si_season_numerator <- episode_outcome[
  ,
  lapply(.SD, sum),
  by = .(season, episode_iso_week),
  .SDcols = rec_rate_cols
] %>%
  pivot_longer(
    cols = all_of(rec_rate_cols),
    names_to = "severity_marker",
    values_to = "freq"
  )


# Combine seasonal numerators and denominators.
si_season_raw <- merge(
  si_season_denominator,
  si_season_numerator,
  by = c(
    "season",
    "episode_iso_week"
  ),
  all = FALSE
)


# Calculate weekly recording completeness.
si_season_raw[
  ,
  pc := 100 * (freq / episodes_n)
]


# Summarise completeness by respiratory season -------------------------

# Calculate seasonal episode counts, median weekly completeness,
# interquartile range and peak weekly completeness.
season_summary <- si_season_raw %>%
  mutate(
    severity_marker = case_when(
      severity_marker == "any_sign" ~ "Any sign",
      severity_marker == "p_sign_temp_rec" ~ "Temp",
      severity_marker == "p_sign_pr_rec" ~ "Pulse rate",
      severity_marker == "p_sign_rr_rec" ~ "Resp rate",
      severity_marker == "p_sign_sats_rec" ~ "O2 sats",
      severity_marker == "p_sign_bp_rec" ~ "BP",
      TRUE ~ severity_marker
    )
  ) %>%
  
  group_by(
    season,
    severity_marker
  ) %>%
  
  summarise(
    weeks = n(),
    
    episodes_n_all = sum(episodes_n),
    
    median_pc = median(
      pc,
      na.rm = TRUE
    ),
    
    lq_pc = quantile(
      pc,
      0.25,
      na.rm = TRUE
    ),
    
    uq_pc = quantile(
      pc,
      0.75,
      na.rm = TRUE
    ),
    
    peak_pc = max(
      pc,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  # Select Table 2 variables.
  select(
    season,
    severity_marker,
    weeks,
    episodes_n_all,
    median_pc,
    lq_pc,
    uq_pc,
    peak_pc
  ) %>%
  
  # Retain results for the recording of any clinical sign.
  filter(
    severity_marker == "Any sign"
  )


# Display Table 2 -------------------------------------------------------

print(season_summary)
