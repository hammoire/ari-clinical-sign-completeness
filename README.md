# Completeness and temporal stability of clinical sign recording in primary care EHRs to inform acute respiratory infection severity surveillance: a retrospective cohort study, 2008–2024

**Author:** Dr William Elson  
**Contact:** [william.elson@phc.ox.ac.uk](mailto:william.elson@phc.ox.ac.uk) AND [william.elson@lshtm.ac.uk](mailto:william.elson@lshtm.ac.uk)


## Overview

This repository contains analysis code, clinical codelists and supporting documentation for the study:

*Completeness and temporal stability of clinical sign recording in primary care EHRs to inform acute respiratory infection severity surveillance: a retrospective cohort study, 2008–2024.*

The study examined the completeness and temporal stability of structured clinical-sign recording for acute respiratory infection (ARI) episodes in primary care electronic health records (EHRs) over 16 influenza seasons, from 2008–09 to 2023–24.

The five objective clinical signs examined were respiratory rate, oxygen saturation, systolic blood pressure, pulse rate and temperature.

This repository is intended to support transparency and reproducibility by sharing the analysis code and relevant clinical codelists alongside the manuscript, subject to data-governance and licensing restrictions.

## Data availability

**No patient-level data are included in this repository.**

The study used routinely collected primary care EHR data from the Oxford–Royal College of General Practitioners Research and Surveillance Centre (RSC), securely hosted within the Oxford Clinical Informatics Digital Hub (ORCHID).

The underlying patient-level data cannot be made publicly available because they contain confidential health information and are subject to information-governance, data-protection and data-access restrictions. This repository does not contain patient-level EHR data, identifiable or potentially identifiable clinical information, extracts from the underlying research database, or credentials and access information for RSC/ORCHID data.

Researchers wishing to access the underlying data must follow the relevant RSC/ORCHID data-access and information-governance procedures.

## Repository contents

The analysis scripts are kept directly in the top-level `R/` directory, rather than in a nested `R/R/` directory. A simplified layout is:

```text
ari-clinical-sign-completeness/
├── README.md
├── LICENSE
├── R/
│   └── [analysis scripts in .R format]
└── codelists/
    └── [shareable codelists and associated documentation]
```

The bracketed entries describe contents rather than literal file names. Other supporting files may be present. The repository does not include the restricted input datasets required to execute the scripts.

## Analysis code

The `R/` directory contains scripts for the analyses and outputs described below. They document the analytical steps that can be shared, but do not include the restricted data-loading procedures or the underlying patient-level inputs.

### Study population and descriptive completeness

Scripts report eligible patient and ARI episode counts, describe episode-level characteristics, and summarise the completeness of individual clinical signs and of any clinical sign, including analyses by ARI subtype and patient characteristics.

### Adjusted completeness analysis

A multivariable logistic regression examines factors associated with recording any clinical sign, including ARI subtype, age group, sex, ethnicity and clinical risk-group status.

One ARI episode is randomly selected per person per respiratory season for this analysis. Standard errors are clustered at general-practice level.

### Sensitivity analyses

Scripts examine alternative clinical-sign ascertainment windows, including daily and cumulative recording completeness and an adjusted analysis using a −7 to +7-day window. Daily results based on the earliest recorded date indicate when a sign was **first recorded**, not every day on which it may have been recorded.

### Temporal analysis and interrupted time series

Scripts generate weekly episode-count and completeness plots, seasonal summaries, and a post hoc interrupted time-series analysis from 2013 onwards. The segmented quasibinomial model estimates the pre-interruption trend, immediate level change and subsequent trend. Newey–West covariance estimates are used to account for serial correlation, with four-week and 13-week lag specifications.

**Breakpoint date to reconcile before publication:** the draft manuscript description supplied for this README gives **1 March 2020**, whereas the shared R script sets **1 April 2020**. The README and manuscript must match the code used for the reported results.

### Figures and tables

The scripts generate the principal descriptive tables, completeness heatmap, adjusted-odds-ratio forest plots, time-series figure, interrupted time-series comparison plot and sensitivity-analysis figures. Figures are written to an `output/` directory created by the scripts; the restricted input data are not included.

## Clinical codelists

Clinical codelists used in the study are provided or linked where redistribution is permitted. These include codelists for structured recordings of respiratory rate, oxygen saturation, systolic blood pressure, pulse rate and temperature.

Ethnicity was derived using a SNOMED CT-based phenotype aligned with the 2021 UK Census ethnicity classification. Supporting phenotype documentation and codelists are included or linked where appropriate.

Clinical risk groups were defined using relevant UK Health Security Agency guidance and PRIMIS business rules. Where third-party licensing or intellectual-property restrictions prevent redistribution, the relevant material is not reproduced; its source and implementation are documented where possible.

## Reproducibility

The repository cannot be run end-to-end without authorised access to the underlying RSC/ORCHID data. Some scripts expect pre-derived episode-level or weekly summary datasets and contain placeholders in place of restricted data-import commands.

The repository provides shareable analysis code, definitions and documentation of derived variables where available, codelists where redistribution is permitted, and documentation of analytical decisions and sensitivity analyses. No synthetic or reconstructed patient-level dataset is provided.

## Software

Analyses were conducted in **R version 4.5.1**. Required packages are described alongside their `library()` calls in the scripts. Package versions or session information may be added to improve reproducibility.

## Manuscript

**Title:** *Completeness and temporal stability of clinical sign recording in primary care EHRs to inform acute respiratory infection severity surveillance: a retrospective cohort study, 2008–2024.*  
**Journal:** BMJ Health & Care Informatics  
**Manuscript DOI:** To be added following publication.

## Citation

If you use material from this repository, please cite the associated manuscript once published. A formal citation and `CITATION.cff` file may be added when publication details are available.

## Versioning

The repository may be updated during peer review. The version corresponding to the final accepted manuscript should be preserved as a tagged release so that the associated code and codelists remain accessible.

## Licence

Original analysis code and associated documentation authored by William Elson in this repository are made available under the **MIT License**; see [LICENSE](LICENSE). The licence permits use, modification and redistribution subject to retention of the copyright and licence notice.

This licence does **not** apply to confidential patient-level data, which are not included. It also does not grant rights to third-party clinical terminologies, codelists, business rules or other materials for which the author does not hold redistribution or sublicensing rights. Such materials remain subject to their own applicable terms and should be used only where permission has been obtained.

## Contact

For questions about the analysis or repository, contact Dr William Elson at [william.elson@phc.ox.ac.uk](mailto:william.elson@phc.ox.ac.uk).
