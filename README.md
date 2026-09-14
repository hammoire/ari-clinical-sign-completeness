# Completeness and temporal stability of clinical sign recording in primary care EHRs to inform acute respiratory infection severity surveillance: a retrospective cohort study, 2008–2024

## Overview

This repository contains analysis code, clinical codelists, and supporting documentation for the study:

**Completeness and temporal stability of clinical sign recording in primary care EHRs to inform acute respiratory infection severity surveillance: a retrospective cohort study, 2008–2024.**

The study examined the completeness and temporal stability of structured clinical-sign recording for acute respiratory infection (ARI) episodes in primary care electronic health records (EHRs) over 16 influenza seasons, from 2008–09 to 2023–24.

The analyses assessed recording of five objective clinical signs:

- respiratory rate
- oxygen saturation
- systolic blood pressure
- pulse rate
- temperature

The repository is intended to support transparency and reproducibility by making the analysis code and relevant clinical codelists available alongside the manuscript.

## Data availability

No individual-level patient data are included in this repository.

The study used routinely collected primary care EHR data from the Oxford–Royal College of General Practitioners Research and Surveillance Centre (RSC), securely hosted within the Oxford Clinical Informatics Digital Hub (ORCHID).

The underlying patient-level data cannot be made publicly available because they contain confidential health information and are subject to information governance, data protection, and data-access restrictions.

Accordingly, this repository does **not** contain:

- patient-level EHR data
- identifiable or potentially identifiable clinical information
- extracts from the underlying research database
- credentials or access information for ORCHID or RSC data

Researchers wishing to access the underlying data must follow the relevant RSC/ORCHID data-access and information-governance procedures.

## Repository contents

The repository contains the code and supporting materials used to document and reproduce the analytical methods described in the manuscript, subject to data-governance and licensing restrictions.

A suggested structure is:

```text
ari-clinical-sign-completeness/
│
├── README.md
├── CITATION.cff
├── LICENSE
│
├── code/
│   ├── 01_cohort_and_completeness.R
│   ├── 02_regression_analysis.R
│   ├── 03_sensitivity_analyses.R
│   ├── 04_temporal_analysis.R
│   └── 05_figures_tables.R
│
├── codelists/
│   ├── clinical_signs/
│   ├── ethnicity/
│   └── README.md
│
└── documentation/
    └── analysis_notes.md
```

## Analysis code

The analysis code documents the main analytical workflow used in the study.

### 1. Cohort and completeness

Code used to:

- identify eligible ARI episodes
- apply the study observation period
- define the primary clinical-sign ascertainment window
- calculate recording completeness for each clinical sign
- calculate completeness for recording of any clinical sign
- stratify completeness by ARI subtype and patient characteristics

### 2. Adjusted regression analysis

Code used for the multivariable logistic regression examining factors associated with recording of any clinical sign.

The model includes:

- ARI subtype
- age group
- sex
- ethnicity
- clinical risk-group status

To reduce the influence of multiple ARI episodes within the same individual while retaining information across time, one ARI episode was randomly selected per person per surveillance season.

Robust standard errors were clustered at general-practice level.

### 3. Sensitivity analyses

Code used to examine the effect of alternative clinical-sign ascertainment windows and other prespecified sensitivity analyses described in the manuscript and supplementary material.

### 4. Temporal analysis

Code used to assess temporal patterns in clinical-sign recording, including the interrupted time-series analysis.

The post hoc interrupted time-series analysis used segmented quasibinomial regression from 2013 onwards with a breakpoint at 1 March 2020.

The model estimated:

- the pre-pandemic trend
- the immediate pandemic-associated level change
- the post-pandemic-onset trend

Newey–West standard errors were used to account for residual serial correlation.

### 5. Figures and tables

Code used to generate the principal analytical outputs reported in the manuscript, where these can be reproduced without releasing restricted patient-level data.

## Clinical codelists

Clinical codelists used in the study are provided where redistribution is permitted.

These include codelists used to identify structured recordings of:

- respiratory rate
- oxygen saturation
- systolic blood pressure
- pulse rate
- temperature

Ethnicity was derived using a SNOMED CT-based phenotype aligned with the 2021 UK Census ethnicity classification. Supporting phenotype documentation and codelists are included or linked where appropriate.

Clinical risk groups were defined using the relevant UK Health Security Agency guidance and PRIMIS business rules. Where third-party licensing or intellectual-property restrictions prevent redistribution of particular codelists or business rules, these materials are not reproduced in this repository; instead, their source and implementation are documented.

## Reproducibility

Because the source patient-level data cannot be shared publicly, the code in this repository cannot be run end-to-end without authorised access to the underlying RSC/ORCHID data.

The repository is therefore intended to provide:

- transparency about the analytical methods
- reproducible code for the statistical analyses
- definitions of derived variables where shareable
- clinical codelists where redistribution is permitted
- documentation of analytical decisions and sensitivity analyses

No synthetic or reconstructed patient-level dataset is provided.

## Software

Analyses were conducted in:

- **R version 4.5.1**

Package versions and software dependencies should be recorded in the repository to support reproducibility.

Where practical, a package environment or session information file should be included.

## Manuscript

**Title:**  
*Completeness and temporal stability of clinical sign recording in primary care EHRs to inform acute respiratory infection severity surveillance: a retrospective cohort study, 2008–2024.*

**Journal:** BMJ Health & Care Informatics

**Manuscript DOI:** To be added following publication.

## Citation

If you use material from this repository, please cite the associated manuscript once published.

A formal citation and `CITATION.cff` file will be added when publication details are available.

## Versioning

The repository may be updated during peer review.

The version corresponding to the final accepted manuscript should be preserved as a tagged release so that the exact code and codelists associated with the published analysis remain accessible.

## Licence

A licence will be applied to material created by the authors where appropriate.

Some clinical terminologies, codelists, business rules, or other third-party materials may be subject to separate licensing or redistribution restrictions. Inclusion in this repository does not override those restrictions.

## Contact

For questions about the analysis or repository, please contact the corresponding author through the contact details provided in the manuscript.
