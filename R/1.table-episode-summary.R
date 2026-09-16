##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Script name: Study population results and Table 1
##
## Purpose:
## Describe the study population, calculate the number
## of eligible patients and ARI episodes, and generate
## Table 1 of episode-level characteristics stratified
## by ARI subtype.
##
## Author: Dr William Elson
##
## Date created: 2026-09-04
##
## Copyright (c) William Elson, 2026
##
## Inputs:
## - cohort: Study population dataset containing
##   eligible patients.
## - episode_outcome: Episode-level dataset containing
##   ARI diagnoses, demographic characteristics and
##   clinical risk group indicators.
##
## Outputs:
## - Number of eligible patients.
## - Number of ARI episodes.
## - Number of individuals with at least one episode.
## - Table 1: Episode-level characteristics stratified
##   by ARI subtype, including an overall column.
## - Summary statistics for Table 1.
##
## Notes:
## - Individual-level patient data are not publicly
##   available.
## - Data-loading code has been removed. The required
##   input datasets must be supplied before execution.
## - Results are generated in R and printed to the
##   console. No output files are written.
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Load required packages -----------------------------------------------
library(data.table)
library(dplyr)
library(tableone)


# Load data -------------------------------------------------------------
# Data not available.
# The cohort and episode_outcome datasets must be supplied before
# running this script.

# Convert episode data to a data.table.
setDT(episode_outcome)


# Number of eligible patients ------------------------------------------
(n_eligible_patients <- nrow(cohort))


# Number of episodes ----------------------------------------------------
(n_episodes <- episode_outcome[, .N])


# Number of individuals with at least one episode ----------------------
(n_individuals_with_episodes <- nrow(
  episode_outcome[, .N, by = c("practice_id", "pseudo_id")]
))


# Define Table 1 --------------------------------------------------------

# Select required variables.
keepers <- c(
  "l2_diagnosis",
  "age_band_episode",
  "age_years",
  "sex",
  "ethnicity_5p",
  "rg_resp",
  "rg_any"
)


# Define the order of categorical variable levels.
level_orders <- list(
  "ARI subtype" = c("URTI", "LRTI", "ILI", "ECLD", "Sus COVID", "ARI NOS"),
  "Age band" = c("Under 1yr", "1 to 4yrs", "5 to 14yrs",
                 "15 to 64yrs", "65yrs and over"),
  "Sex" = c("Female", "Male"),
  "Ethnicity" = c("Asian", "Black", "Mixed", "White", "Other", "Missing"),
  "Any RG" = c("Risk grp", "No risk grp"),
  "Resp RG" = c("Risk grp", "No risk grp")
)


# Prepare episode-level data for Table 1.
t1_data <- episode_outcome %>%
  select(all_of(keepers)) %>%
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
      age_band_episode == "65+yrs" ~ "65yrs and over",
      age_band_episode == "<1yr" ~ "Under 1yr"
    ),
    
    rg_any = ifelse(rg_any, "Risk grp", "No risk grp"),
    rg_resp = ifelse(rg_resp, "Risk grp", "No risk grp"),
    
    sex = factor(sex, levels = level_orders$Sex),
    
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
    
    rg_any = factor(rg_any, levels = level_orders$`Any RG`),
    rg_resp = factor(rg_resp, levels = level_orders$`Resp RG`),
    
    episode = 1L
  )


# Assign descriptive variable names.
names(t1_data) <- c(
  "ARI subtype",
  "Age band",
  "Age years",
  "Sex",
  "Ethnicity",
  "Resp RG",
  "Any RG",
  "Episode"
)


# Create Table 1 --------------------------------------------------------

# Define variables to include in Table 1.
vars <- c(
  "Age band",
  "Age years",
  "Sex",
  "Ethnicity",
  "Any RG",
  "Resp RG"
)


# Identify categorical variables.
fact_vars <- c(
  "Age band",
  "Sex",
  "Ethnicity",
  "Any RG",
  "Resp RG"
)


# Generate Table 1, stratified by ARI subtype.
tab1 <- CreateTableOne(
  vars = vars,
  strata = "ARI subtype",
  data = t1_data,
  factorVars = fact_vars,
  addOverall = TRUE
)


# Print Table 1.
print(
  tab1,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE,
  format = "p",
  nonnormal = TRUE
)


# Display a summary of Table 1.
summary(tab1)
