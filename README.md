# Repository overview

This repository hosts the **R scripts** and **raw data** for a common-garden study examining rapid evolutionary divergence of multidimensional trait syndromes in the invasive plant *Spartina alterniflora* between its native (USA) and introduced (China) ranges. The workflow reproduces the core statistical analyses and figures.

For full methods and results, please refer to our study:

> "Rapid evolution of multidimensional traits and climate-driven syndromes in an invasive plant across latitudinal gradients" *Journal of Ecology* (in review)

## What's in this repository

### R scripts

- `1__R_code_-_Map_-_0701.R` Generates the collection-site map.
- `2__R_code_-_LMM_-_0701.R` Fits linear mixed models comparing traits between ranges and genetic groups, and latitudinal regressions within the introduced range.
- `3__R_code_-_Trait_screening_-_0701.R` Computes trait correlation matrices and screens candidate defense traits against herbivore consumption.
- `4__R_code_-_Syndrome_analysis_-_0701.R` Performs hierarchical cluster analysis and linear discriminant analysis to identify multivariate trait syndromes.
- `5__R_code_-_Selection_analysis_-_0701.R` Performs phenotypic selection analysis on syndrome-defining traits.
- `6__R_code_-_Climate_analysis_-_0701.R` Fits multiple regression and redundancy analysis models relating traits to range, temperature, and precipitation.

### Data files

- `Raw data - 0701.xlsx` Family-level dataset used as the input for all analyses.
- `README_Raw_data_0701.docx` Column descriptions for the raw dataset.
