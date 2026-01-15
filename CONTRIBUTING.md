# 🎉 Contributing to The Mental Wellness Index™ (MWI)
Thank you for considering contributing to The Mental Wellness Index!
This document provides a comprehensive guide for contributors, project maintainers, and users building upon this project. By participating, you agree to follow our Code of Conduct and interact respectfully with the community.

## Introduction
The Mental Wellness Index™ (MWI) is a community-level mental wellness framework and dashboard that provides a score (0–100) for every ZIP Code Tabulation Area (ZCTA) in the United States. The MWI integrates 28 measures across the domains of Social Determinants of Health, Healthcare Access, and Health Status, while acknowledging how Structural Racism and Community & Cultural Assets influence these measures.

This `CONTRIBUTING.md` serves:

### Project Owners
Maintain the codebase, datasets, documentation, and scientific foundations of the tool.

### Project Contributors
Developers and researchers who want to add measures, fix bugs, improve documentation, update the pipeline, or enhance the tool.

### Project Consumers
Users who want to build on or customize the MWI for their community or research needs.

This file complements:
* `README.md` - project overview and basic setup
* `LICENSE.md` - reuse permissions

If contribution pathway evolve, this document will be updated accordingly.

## 🙌 Ways to Contribute
You can contribute to the MWI in many ways:
* Report a bug
* Request or design a new feature
* Improve documentation
* Add or update measures in the metadata
* Support local customization workflows
* Contribute to the R Shiny app or R scripts
* Improve the data engineering pipeline
* Support validation and analysis
We welcome both code and non-code contributions

## 🐞 Reporting Bugs
To report a bug, please open an issue and include:
* Required Information
* Clear steps to reproduce
* Expected vs. actual behavior
* Browser, OS, and R/RStudio version
* Screenshots (if UI related)
* Relevant logs (if data or pipeline related)

### Bug Report Template
```
### Description
A concise description of the issue.

### Steps to Reproduce
1.
2.
3.

### Expected Behavior

### Actual Behavior

### Environment
- OS:
- Browser:
- R Version:
- Additional context:
```

## ✨ Requesting Enhancements
Enhancements include new features, improvements to UI, adding new measures, updating metadata, or expanding the data pipeline.

When opening an enhancement request:
* Describe the use case
* Provide examples if applicable
* Link supporting research or datasets
* Suggest implementation approaches

Tag your issue with:
```
enhancement
```

## 💻 Environment Setup
To run or contribute to the MWI:

__Required software__
* **R** (latest version)
* **RStudio**
* Modern browser (Chrome, Firefox, Edge, etc.)

1. Install required packages
In the RStudio console:
```
install.packages(
    c(
        'readxl','writexl','htmltools','shiny','tigris','leaflet',
        'RColorBrewer','sf','plotly','ggbeeswarm','shinyWidgets',
        'sass','shinycssloaders','shinyBS','DT','dplyr'
    )
)
```

## 🧰 Running & Customizing the MWI
1. Download the repository
From GitHub → Code → Download ZIP
2. Open `app.R` in RStudio
3. Set `app_local <- TRUE`
Line 11 in app.R must be:

```
app_local <- TRUE
```
4. Run the application
Click:
**Run App → Run External → Run App**

The dashboard should open in your browser.

To create your own MWI, follow the Create Your Own MWI tab in the app or use the workflows below.

## 🗂 Data Structure & Requirements
To add custom datasets:
1. Use CSV format
2. Required columns:
    * Geographical ID
    * Numerator
    * Denominator (if applicable)

**Accepted Geographical IDs**
* ZCTA — 5-digit
* County FIPS — 5-digit
* ZIP Code — USPS ZIP
* Census Tract — 11-digit

**Race-stratified data**
Two columns required:
* `<measure>_pop`
* `<measure>_black`
Set `Preprocessed = TRUE` in `Metadata.xlsx`.

**Missing values**
Leave cells blank.

**Multiple measures per file**
Add separate rows to `Metadata.xlsx` for each measure.

## 🏗 MWI Data Pipeline
The pipeline includes:
1. Raw Data
2. Preprocessed Data
3. Cleaned Data
4. Combined Measures
5. Weighting
6. Score Creation
7. Dashboard Output
Data folders appear in:
```
Teams > BHN Score > Data
```
**Folder meanings:**
* Raw: Unprocessed datasets
* Preprocessed: Ready for pipeline, one row per geography
* Cleaned: Final measures and converted scores

Refer to the included **Data Pipeline diagram** in the repo.

## 🧾 Measure Registration & Metadata
The core metadata file is:
```
Metadata.xlsx
```
It includes measure names, descriptions, scaling, directionality, transformations, weighting, and file references.

**Pre-Processed Data**
Indicates:
* Required transformations
* Directionality alignment
* Scaling
* Geographic conversions

**Cleaned Data**
Final standardized measures used to compute the MWI.

Combined measures available in:

* `HSE_ZCTA_Converted_Measures.csv`
* `HSE_ZCTA_Percentile_Ranked_Measures.csv`

## ⚖️ Weights & Score Creation

MWI supports three sets of weights:

1. Equal weights (parsimonious)
2. Child Opportunity Index framework
3. County Health Rankings framework

Final scores:
* `HSE_BHN_ZCTA_Score_Black.csv`
* `HSE_BHN_ZCTA_Score_Population.csv`

Each score is scaled 0–100.

## 📐 Style Guide

Language & Documentation
* Use clear, consistent language
* Comment R code thoughtfully
* Update documentation when adding or altering measures

Coding Conventions
* Follow tidyverse principles
* Prefer functional, pipeline-based R code
* Keep UI and server logic modular where possible

Commit Messages
Use **Conventional Commits**, e.g.:
```
feat: add new healthcare access measure
fix: correct scaling for SDOH variable
docs: update metadata instructions
```

## 🏅 Recognition Model
We appreciate and recognize contributor efforts through:
* Acknowledgment in GitHub PR merges
* Optional inclusion in humans.txt
* Potential inclusion in release notes
* Community shout-outs
Contributors are essential to improving the scientific and technical quality of the MWI.

## 👥 Who Is Involved?
The MWI is developed by contributors at The MITRE Corporation with support from community collaborators and researchers.

**Contact**
For questions:
📧 socialjustice@mitre.org

## 💬 Where to Get Help
* GitHub Issues
* GitHub Discussions (if enabled)
* Email (above)
* README.md and Documentation folders
* Metadata and Data folder README files

No question is too small—please reach out.

## 📖 Glossary

**Bug**

A problem in code or data causing unexpected behavior.

**Bug Tracker / Issues**

GitHub’s issue system used for reporting and tracking bugs.

**ZCTA**

ZIP Code Tabulation Area, the geographic unit used for MWI scoring.

**Metadata**

Structured measure definitions used to transform raw data into standardized MWI components.

## 📚 References & Attribution

Resources informing this CONTRIBUTING.md:

* GitHub Guidelines for CONTRIBUTING.md
* Atom Editor CONTRIBUTING.md
* Open Government CONTRIBUTING.md
* Working Open Guide
* SlideWinder Contributor Site
* Humans.txt project
* MITRE internal MWI documentation

Approved for Public Release; Distribution Unlimited.
**Public Release Case Number 21-3708. ©2021 The MITRE Corporation. ALL RIGHTS RESERVED.**
