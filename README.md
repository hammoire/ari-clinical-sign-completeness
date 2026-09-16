# Completeness and temporal stability of clinical sign recording in primary care EHRs to inform acute respiratory infection severity surveillance: a retrospective cohort study, 2008–2024

**Author:** Dr William Elson  
**Contact:** [william.elson@phc.ox.ac.uk](mailto:william.elson@phc.ox.ac.uk) AND [william.elson@lshtm.ac.uk](mailto:william.elson@lshtm.ac.uk)

## Associated publication

**Manuscript:** *Completeness and temporal stability of clinical sign recording in primary care EHRs to inform acute respiratory infection severity surveillance: a retrospective cohort study, 2008–2024.*  
**Journal:** [BMJ Health & Care Informatics](https://informatics.bmj.com/)  
**Publication DOI:** Pending publication. Add the live DOI link here when assigned, using the format `https://doi.org/INSERT_DOI_HERE`. This placeholder is **not** a publication link.  
**Citation:** Full bibliographic details will be added when available.

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

## Clinical codelists and phenotype definitions

The manuscript's supplementary material (Supplement 6) provides context on the software, codelists, data availability and analysis code. This section identifies the main phenotype sources and explains which materials can be shared. Codelists developed for the study are made available in the repository where redistribution is permitted; the underlying EHR extracts are not shared. Refer to the manuscript and supplementary material for the full study-specific implementation details.

### Acute respiratory infection (ARI) phenotype

ARI episodes and subtypes were identified using a clinical phenotyping algorithm previously described and validated by Elson and colleagues:

Elson WH, et al. *Validation of an acute respiratory infection phenotyping algorithm to support robust computerised medical record-based respiratory sentinel surveillance, England, 2023.* **Eurosurveillance**. 2024;29(35):2300682. [https://doi.org/10.2807/1560-7917.ES.2024.29.35.2300682](https://doi.org/10.2807/1560-7917.ES.2024.29.35.2300682).

The analysis distinguishes upper respiratory tract infection (URTI), lower respiratory tract infection (LRTI), influenza-like illness (ILI), exacerbation of chronic lung disease (ECLD), suspected COVID-19 and ARI not otherwise specified (ARI NOS). Consult the validation publication, associated phenotype material and shareable ARI codelists for the underlying definitions rather than treating these short labels as standalone diagnostic criteria.

### Clinical-sign recording codelists

The clinical-sign codelists identify structured EHR recordings of the five objective measurements investigated in the study:

- Respiratory rate
- Oxygen saturation
- Systolic blood pressure
- Pulse rate
- Temperature

The analysis derives an episode-level recording indicator for each sign and an additional indicator for whether **any** of the five signs was recorded within the specified ascertainment window. These indicators describe *recording completeness*, not whether a measurement was clinically normal or abnormal. The sensitivity analysis varies the ascertainment window relative to the ARI episode date; its daily analysis uses the earliest eligible recording date per sign, so it describes the timing of first recording rather than every repeated measurement.

Shareable clinical-sign codelists and their supporting documentation are provided under `codelists/` where available. The data extracts and the records in which the codes occurred are not included.

### Ethnicity phenotype

Ethnicity was derived from SNOMED CT-coded primary care records using a phenotype aligned with the 2021 UK Census classification. The analysis uses five broad groups—Asian, Black, Mixed, White and Other—and an explicit Missing category where ethnicity was not recorded. Shareable ethnicity codelists and phenotype documentation are included or linked in the repository where permitted; consult the manuscript supplement for the study-specific derivation.

### Clinical risk groups and PRIMIS business rules

Clinical risk-group variables were defined with reference to relevant UK Health Security Agency (UKHSA) guidance and PRIMIS business rules. The analysis distinguishes **any clinical risk group** (`rg_any`) from a **respiratory clinical risk group** (`rg_resp`). The UKHSA [influenza Green Book chapter](https://www.gov.uk/government/publications/influenza-the-green-book-chapter) provides background on clinical risk categories; the manuscript and supplement should be consulted for the guidance and implementation applicable to this particular study, rather than assuming that subsequent revisions of the guidance were used retrospectively.

The PRIMIS-derived codelists and business rules are third-party materials and may be subject to licensing or intellectual-property restrictions. Where redistribution is not authorised, the full lists or business rules are **not** reproduced in this repository. For enquiries about the relevant PRIMIS materials and permission to access or reuse them, contact PRIMIS at **[enquiries@primis.nottingham.ac.uk](mailto:enquiries@primis.nottingham.ac.uk)**. This contact is for PRIMIS materials, **not** for obtaining access to the restricted RSC/ORCHID patient-level data.

### Availability and licensing of codelists

Not every phenotype definition can necessarily be redistributed in full. Availability of a reference or a code list in this repository does not imply permission to redistribute its underlying third-party terminology or business rules. The [MIT License](LICENSE) covers original code and documentation authored for this repository; third-party clinical terminologies, PRIMIS materials and other restricted content remain subject to their own terms. Consult the relevant source or rights holder before reusing those materials.

## Reproducibility

The repository cannot be run end-to-end without authorised access to the underlying RSC/ORCHID data. Some scripts expect pre-derived episode-level or weekly summary datasets and contain placeholders in place of restricted data-import commands.

The repository provides shareable analysis code, definitions and documentation of derived variables where available, codelists where redistribution is permitted, and documentation of analytical decisions and sensitivity analyses. No synthetic or reconstructed patient-level dataset is provided.

## Software

Analyses were conducted in **R version 4.5.1**. Required packages are described alongside their `library()` calls in the scripts. Package versions or session information may be added to improve reproducibility.

## Manuscript and citation details

The associated manuscript title, journal and DOI placeholder are listed in [Associated publication](#associated-publication) at the top of this README. The DOI will be linked once assigned.

## Citation

If you use material from this repository, please cite the associated manuscript once published. A formal citation and `CITATION.cff` file may be added when publication details are available.

## Versioning

The repository may be updated during peer review. The version corresponding to the final accepted manuscript should be preserved as a tagged release so that the associated code and codelists remain accessible.

## Licence

Original analysis code and associated documentation authored by William Elson in this repository are made available under the **MIT License**; see [LICENSE](LICENSE). The licence permits use, modification and redistribution subject to retention of the copyright and licence notice.

This licence does **not** apply to confidential patient-level data, which are not included. It also does not grant rights to third-party clinical terminologies, codelists, business rules or other materials for which the author does not hold redistribution or sublicensing rights. Such materials remain subject to their own applicable terms and should be used only where permission has been obtained.

## Contact

For questions about the analysis or repository, contact Dr William Elson at [william.elson@phc.ox.ac.uk](mailto:william.elson@phc.ox.ac.uk).
