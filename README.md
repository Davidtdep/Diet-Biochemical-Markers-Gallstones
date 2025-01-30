# Diet-Biochemical-Markers-Gallstones

This repository contains the R script used for the statistical analyses in the study on dietary intake and biochemical markers in patients with gallstones.

## Purpose
This script demonstrates the statistical analysis of dietary intake patterns and their associations with biochemical markers in patients diagnosed with gallstones. The analyses aim to:
- Examine associations between weekly consumption of different food categories and biochemical markers.
- Investigate potential confounders through multivariable logistic regression models.
- Assess the effects of cooking methods and food additives on biochemical markers.
- Explore associations between alcohol consumption and biochemical markers.
- Identify the best set of covariates for adjustment in different analyses.

## Required R packages
The following R packages are necessary to execute the analyses:
- **Data manipulation and visualization**: dplyr, ggplot2, readxl, openxlsx
- **Statistical modeling**: broom, MASS, pROC
- **Parallel processing**: parallel
- **Visualization**: forestplot, grid

## Analyses included
This script performs the following analyses:
- **Association between diet and biochemical markers**: Logistic regression models were used to assess the association between weekly consumption of food categories and elevated biochemical markers, adjusting for age, sex, obesity, education level, health insurance, BMI, alcohol consumption, and physical activity.
- **Evaluation of cooking methods and additives**: Logistic regression and Fisher's exact test were used to explore associations between cooking methods, commonly used food additives, and biochemical markers.
- **Alcohol consumption analysis**: Logistic regression models were applied to evaluate the association between alcohol consumption patterns and biochemical markers, including subgroup analysis by gallstone family history.
- **Identification of optimal covariate adjustments**: Systematic testing of all possible combinations of covariates to determine the most informative adjustment models.
- **Visualization of significant results**: Forest plots were created to display odds ratios (OR) and confidence intervals (CI) for significant associations.

## How to use
1. Download or clone this repository.
2. Prepare a dataset (`data.xlsx`) with the format described in the script.
3. Open the script (`main_analysis.R`) in RStudio.
4. Install the required R packages (see above).
5. Run the script to replicate the analyses.

## Data availability
The dataset (`data.xlsx`) used in this study is not publicly available but can be provided upon reasonable request. The script demonstrates the workflow for the analyses without requiring access to the raw data.

## License
This repository is licensed under CC0 1.0 Universal (Public Domain). The code is free to use, modify, and distribute without restrictions.
