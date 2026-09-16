##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: Segmented analysis of temporal trends
##
## Purpose:
## Investigate temporal changes in clinical sign recording
## completeness using interrupted time-series analysis.
## Estimate pre-pandemic trends, the immediate change at
## pandemic onset and post-pandemic trends. Compare
## quasibinomial, Poisson and negative binomial models,
## and generate a plot of observed and predicted completeness.
##
## Author: Dr William Elson
##
## Date created: 2026-09-08
##
## Copyright (c) William Elson, 2026
##
## Inputs:
## - si_weekly: Weekly ARI episode summary containing
##   episode dates, clinical sign categories, episode
##   counts, recording counts and completeness percentages.
##
## Outputs:
## - Primary interrupted time-series regression model.
## - Residual autocorrelation diagnostic.
## - Primary results using Newey-West standard errors
##   with a four-week lag.
## - Sensitivity analysis using a 13-week lag.
## - Poisson and negative binomial comparison models.
## - Model comparison statistics and predicted
##   completeness percentages.
## - Supplementary figure saved to:
##   output/supplement-5-its-plot.png
##
## Notes:
## - Individual-level patient data are not publicly
##   available.
## - Data-loading code has been removed. The required
##   weekly summary dataset must be supplied before
##   execution.
## - The analysis includes observations from
##   1 January 2013 onwards.
## - The interruption is defined as 1 April 2020.
## - The primary model uses quasibinomial regression
##   with a logit link.
## - Newey-West standard errors account for residual
##   autocorrelation, using four-week and 13-week lags.
## - Pre-pandemic and post-pandemic trends are expressed
##   as annual changes in the odds of recording a sign.
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Load required packages -----------------------------------------------

library(dplyr)     # Data preparation and manipulation of model results.
library(ggplot2)   # Creating and saving the model comparison plot.
library(sandwich)  # Calculating Newey-West robust covariance matrices.

# MASS and tibble are called explicitly using their
# package namespaces below.


# Load data -------------------------------------------------------------

# Data not available.
# The si_weekly dataset must be supplied before
# running this script.


# Define breakpoint -----------------------------------------------------

# Define the interruption date for the SARS-CoV-2 pandemic.
its_start <- as.Date("2020-04-01")


# Prepare interrupted time-series dataset -------------------------------

# Restrict the analysis to observations from January 2013
# and episodes with any clinical sign recorded.
its_data <- si_weekly %>%
  filter(
    episode_week >= as.Date("2013-01-01")
  ) %>%
  filter(
    severity_marker == "Any sign"
  ) %>%
  arrange(episode_week) %>%
  mutate(
    
    # Continuous time in years from the first included week.
    time_years = as.numeric(
      difftime(
        episode_week,
        min(episode_week),
        units = "weeks"
      )
    ) / 52.18,
    
    # Indicator for observations on or after the breakpoint.
    pandemic = as.integer(
      episode_week >= its_start
    ),
    
    # Years elapsed since the breakpoint.
    # Set to zero for all pre-breakpoint observations.
    time_after_pandemic = pmax(
      0,
      as.numeric(
        difftime(
          episode_week,
          its_start,
          units = "weeks"
        )
      ) / 52.18
    ),
    
    # Observed recording completeness percentage.
    observed_pc = pc
  )


# Fit primary quasibinomial model --------------------------------------

# Fit a segmented regression model using weekly counts
# of episodes with and without a recorded clinical sign.
its_model <- glm(
  cbind(freq, episodes_n - freq) ~
    time_years +
    pandemic +
    time_after_pandemic,
  family = quasibinomial(link = "logit"),
  data = its_data
)


# Display the initial model summary.
# Default standard errors are not used for final inference.
summary(its_model)


# Assess residual autocorrelation --------------------------------------

# Extract Pearson residuals from the primary model.
its_resid <- residuals(
  its_model,
  type = "pearson"
)


# Test residual autocorrelation using the Ljung-Box test
# with a lag of 13 weeks.
autocorrelation_test <- Box.test(
  its_resid,
  lag = 13,
  type = "Ljung-Box"
)


# Display the autocorrelation test results.
print(autocorrelation_test)


# Calculate Newey-West covariance matrices -----------------------------

# Extract model coefficients.
b <- coef(its_model)


# Calculate an autocorrelation-consistent covariance matrix
# using a four-week lag for the primary analysis.
V_hac <- NeweyWest(
  its_model,
  lag = 4,
  prewhite = FALSE,
  adjust = TRUE
)


# Calculate a covariance matrix using a 13-week lag
# for the sensitivity analysis.
V_hac_13 <- NeweyWest(
  its_model,
  lag = 13,
  prewhite = FALSE,
  adjust = TRUE
)


# Extract model residual degrees of freedom.
model_df <- df.residual(its_model)


# Calculate the critical value for 95% confidence intervals.
crit <- qt(
  0.975,
  model_df
)


# Define function to calculate effect estimates ------------------------

# Calculate odds ratios, confidence intervals and p-values
# for specified contrasts using a robust covariance matrix.
get_effect <- function(weights, label, V) {
  
  # Construct the contrast vector.
  L <- setNames(
    rep(0, length(b)),
    names(b)
  )
  
  L[names(weights)] <- weights
  
  
  # Calculate the effect estimate on the log-odds scale.
  estimate <- as.numeric(
    sum(L * b)
  )
  
  
  # Calculate the robust standard error.
  se <- sqrt(
    drop(
      t(L) %*% V %*% L
    )
  )
  
  
  # Calculate the two-sided p-value.
  t_value <- estimate / se
  
  p_value <- 2 * stats::pt(
    abs(t_value),
    df = model_df,
    lower.tail = FALSE
  )
  
  
  # Return effect estimates.
  tibble::tibble(
    Effect = label,
    log_odds = estimate,
    SE = se,
    OR = exp(estimate),
    conf_low = exp(estimate - crit * se),
    conf_high = exp(estimate + crit * se),
    p_value = p_value
  )
  
}


# Define function to summarise ITS effects -----------------------------

# Calculate the pre-pandemic annual trend, immediate
# level change and post-pandemic annual trend.
make_its_summary <- function(V) {
  
  dplyr::bind_rows(
    
    # Pre-pandemic annual trend.
    get_effect(
      c(time_years = 1),
      "Pre-pandemic annual trend",
      V
    ),
    
    # Immediate change at the breakpoint.
    get_effect(
      c(pandemic = 1),
      "Immediate change after pandemic onset",
      V
    ),
    
    # Post-pandemic annual trend.
    # Combines the pre-pandemic slope and slope change.
    get_effect(
      c(
        time_years = 1,
        time_after_pandemic = 1
      ),
      "Post-pandemic annual trend",
      V
    )
  )
  
}


# Calculate primary and sensitivity results ----------------------------

# Primary analysis using a four-week lag.
its_summary <- make_its_summary(V_hac)


# Sensitivity analysis using a 13-week lag.
its_summary_13 <- make_its_summary(V_hac_13)


# Format primary results table -----------------------------------------

# Format odds ratios, confidence intervals, changes in odds
# and p-values for the supplementary results table.
its_results_table <- its_summary %>%
  mutate(
    
    `OR (95% CI)` = sprintf(
      "%.2f (%.2f-%.2f)",
      OR,
      conf_low,
      conf_high
    ),
    
    `Change in odds (%)` = sprintf(
      "%.1f",
      (OR - 1) * 100
    ),
    
    `p value` = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  ) %>%
  select(
    Effect,
    `OR (95% CI)`,
    `Change in odds (%)`,
    `p value`
  )


# Display the primary results table.
print(its_results_table)


# Format sensitivity analysis results ----------------------------------

# Format the results using a 13-week Newey-West lag.
its_results_table_13 <- its_summary_13 %>%
  mutate(
    
    `OR (95% CI)` = sprintf(
      "%.2f (%.2f-%.2f)",
      OR,
      conf_low,
      conf_high
    ),
    
    `Change in odds (%)` = sprintf(
      "%.1f",
      (OR - 1) * 100
    ),
    
    `p value` = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  ) %>%
  select(
    Effect,
    `OR (95% CI)`,
    `Change in odds (%)`,
    `p value`
  )


# Display the sensitivity analysis results.
print(its_results_table_13)


# Fit comparison models -------------------------------------------------

# Poisson regression ----------------------------------------------------

# Fit a Poisson regression model for weekly recording counts,
# using the total number of episodes as an offset.
its_model_poisson <- glm(
  freq ~
    time_years +
    pandemic +
    time_after_pandemic +
    offset(log(episodes_n)),
  family = poisson(link = "log"),
  data = its_data
)


# Calculate the Pearson dispersion statistic.
dispersion <- sum(
  residuals(
    its_model_poisson,
    type = "pearson"
  )^2
) / df.residual(its_model_poisson)


# Display the dispersion statistic.
print(dispersion)


# Negative binomial regression -----------------------------------------

# Fit a negative binomial model using the same predictors
# and offset as the Poisson model.
its_model_nbin <- MASS::glm.nb(
  freq ~
    time_years +
    pandemic +
    time_after_pandemic +
    offset(log(episodes_n)),
  data = its_data
)


# Compare Poisson and negative binomial models -------------------------

# Compare model fit using Akaike's information criterion.
model_comparison <- AIC(
  its_model_poisson,
  its_model_nbin
)


# Display model comparison results.
print(model_comparison)


# Calculate predicted recording completeness ---------------------------

# Obtain predicted completeness from the quasibinomial model.
# The response is a probability and is converted to a percentage.
its_data$pred_rate_qb <- predict(
  its_model,
  type = "response"
) * 100


# Obtain predicted weekly counts from the negative binomial model
# and convert them to completeness percentages.
its_data$pred_rate_nb <- predict(
  its_model_nbin,
  type = "response"
) / its_data$episodes_n * 100


# Obtain predicted weekly counts from the Poisson model
# and convert them to completeness percentages.
its_data$pred_rate_pois <- predict(
  its_model_poisson,
  type = "response"
) / its_data$episodes_n * 100


# Create model comparison plot -----------------------------------------

# Plot observed completeness and predictions from
# the quasibinomial, Poisson and negative binomial models.
its_plot <- its_data %>%
  
  ggplot(
    aes(x = episode_week)
  ) +
  
  # Plot observed recording completeness.
  geom_line(
    aes(
      y = observed_pc,
      color = "Observed"
    ),
    linewidth = 0.35
  ) +
  
  # Plot negative binomial predictions.
  geom_line(
    aes(
      y = pred_rate_nb,
      color = "Negative binomial"
    ),
    linewidth = 1
  ) +
  
  # Plot Poisson predictions.
  geom_line(
    aes(
      y = pred_rate_pois,
      color = "Poisson"
    ),
    linewidth = 1
  ) +
  
  # Plot quasibinomial predictions.
  geom_line(
    aes(
      y = pred_rate_qb,
      color = "Quasibinomial"
    ),
    linewidth = 1
  ) +
  
  # Mark the interruption date.
  geom_vline(
    aes(
      xintercept = as.numeric(its_start),
      color = "Breakpoint"
    ),
    linetype = "dashed",
    linewidth = 0.75
  ) +
  
  # Define colours for observed and predicted values.
  scale_color_manual(
    name = "",
    values = c(
      "Observed" = "black",
      "Negative binomial" = "#009E73",
      "Poisson" = "#E69F00",
      "Quasibinomial" = "#0072B2",
      "Breakpoint" = "red"
    )
  ) +
  
  # Define the date axis.
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  # Define axis labels.
  labs(
    x = "Episode week",
    y = "Completeness (%)"
  ) +
  
  # Apply the plot theme.
  theme_bw()


# Display model comparison plot.
print(its_plot)


# Save supplementary figure --------------------------------------------

# Create the output directory if it does not exist.
dir.create(
  "output",
  showWarnings = FALSE,
  recursive = TRUE
)


# Save the model comparison plot.
ggsave(
  filename = "output/supplement-5-its-plot.png",
  plot = its_plot,
  width = 10
)