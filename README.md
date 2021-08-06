This repository contains the datasets and R scripts for a research paper that analysis the effects of disability benefits on labor supply and health status in Ecuador.

**ENSANUT_datasets** folder containing the datasets from the National Health Survey of Ecuador
* 1_BDD_ENS2018_f1_personas.dta: .dta file with the survey data used in this study

**R_scripts** folder containing the R scripts used to generate the data wrangling and analysis
* download_disability.R: downloads .dta file to data directory
* wrangle_disability.R: creates a derived datasets and saves as R object in the rds directory

**rds_files**
* disability_ec.rds: .rds file with all the variables generated through the wrangling process