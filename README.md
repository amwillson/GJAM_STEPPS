# Overview

This repo contains code written by AM Willson for Willson et al. (in prep). The code is broken into four sections. The first section organizes the output of the STEPPS pollen vegetation model (previously published). The second section processes downscaled climate reconstructions from Rena Guaita. The third section formats data, runs the model, plots output, and performs out-of-sample validation using the mean relative abundance estimates from STEPPS. The fourth section is identical to the third section, but uses the posterior draws from STEPPS instead of the means. We use the GJAM model to fit the relationship between environmental variables and taxon-level relative abundances across the last 2,000 years of the pre-Industrial Holocene across the Upper Midwest , US. We use the model to estimate environment-vegetation relationships and residual relationships between taxa.

 # License
 
This repository holds an MIT License, as described in the LICENSE file.

# Software versions

This repository was built in the R environment on two machines. The majority of files were originally built to be run on R version 4.4.0. Scripts clearly identified as being run on a virtual machine (VM) were run on a Docker stack with R version 4.1.3.

# Package versions

* `car` v. 3.1.2
* `corrplot` v. 0.92
* `cowplot` v. 1.1.3 (v. 1.1.1 on VM)
* `dplyr` v. 1.1.4 (v. 1.0.8 on VM)
* `fields` v. 15.2 (v. 13.3 on VM)
* `ggplot2` v. 3.5.1 (v. 3.3.6 on VM)
* `gjam` v. 2.6.2 (v. 2.6 on VM)
* `lme4` v. 1.1.35.3 (v. 1.1.29 on VM)
* `ncdf4` v. 1.22
* `R.matlab` v. 3.7.0
* `RColorBrewer` v. 1.1.3
* `reshape2` v. 1.4.4
* `sf` v. 1.0.16 (v. 1.0.7 on VM)
* `sfheaders` v. 0.4.4 (v. 0.4.0 on VM)
* `stats` v 4.4.0 (v. 4.1.3 on VM)
* `tibble` v. 3.2.1 (v. 3.1.8 on VM)
* `tidyr` v. 1.3.1 (v. 1.2.0 on VM)
* `tidytext` v. 0.4.2

# Directory structure

* 
