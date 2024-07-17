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
* `tigris` v. 2.1 (v. 1.6 on VM)

# Directory structure

## Within repository

* data: **Input data**, **intermediate data products**, and **processed data products** from processing data and combining data sources in space and time. Any data product < ~250 MB is contained in these directories. Any data prodcut < 100 MB is pushed to Github. Anything between 100-250 MB is available upon request because the files are too large to be housed on Github. The input data products are ultimately processed and then used in the analysis portions of the workflow.
  * input: inputs to this workflow
    * 8km.RData: Dataframe with grid cell IDs and grid cell centroids from PLS data. This file was created in https://github.com/amwillson/historic-modern-environment/. It is required because the soil data used in this analysis were originally geospatially matched to the PLS data. The PLS grid is the same as the STEPPS grid, so this product is used to simply match the soil and STEPPS grids in the same way.
    * gridded_soil.RData: Dataframe with soil data across space from gSSURGO. The file was created in https://github.com/amwillson/historic-modern-environment/ for the modern period, which we assume is consistent with the paleorecord.
    * total_matched.RData: Dataframe with the mapping from PLS points to PLS grid cells. This file was created in https://github.com/amwillson/historic-modern-environment/. This dataframe tells us which grid cell each PLS point falls within. Used to determine which STEPPS grid cell each climate grid cell falls within, based on which PLS point each climate grid cell is closest to.
    * msb-paleon-2: directory downloaded from https://doi.org/10.6073/pasta/338e778c00b13acd278302e82f992415. Contains mean and SD of STEPPS relative abundance
      * 2Kyrs_Comp_Mean_Level2_v1.0.nc: mean relative abundance from STEPPS
      * 2Kyrs_Comp_SD_Level2_v1.0.nc: standard deviation of relative abundance from STEPPS. Not used.
      * manifest.txt: Manifest file from archived data product
      * msb-paleon.22.1.report.xml: Documentation for archived data product
      * msb-paleon.22.1.txt: Documentation for archived data product
      * msb-paleon.22.1.xml: Documentation for archived data product
    * PLS_point: directory with RData files each containing point-level observations of taxon occurrences from the Public Land Survey (PLS). Files were generated in https://github.com/amwillson/historic-modern-environment/. The STEPPS model was calibrated on the PLS. We therefore have the PLS points that are in each STEPPS grid cell. Used this relationship to 1) find which PLS point each climate reconstruction is closest to; 2) identify which grid cell each PLS point falls within; and 3) average over all climate reconstrutions in teh same grid cell according to which PLS point it is closest to.
      * minnesota_process.RData: PLS point-level observations for Minnesota
      * upmichigan_process.RData: PLS point-level observations for Upper Michigan
      * wisconsin_process.RData: PLS point-level observations for Wisconsin
    * posterior_stepps: directory with posterior draws from the STEPPS model. This was not previously published. It will be made available upon request.
      * input.rdata: Contains indexing associated with the posterior draws. For this project, the R object `centers_veg` is the data object of interest, which contains coordinates for the centroids of each location in the samples array
      * rIts_sub.RDS: array with relative abundance estimates for 100 posterior samples from STEPPS. Dimensions: 704 spatial locations x 12 taxa x 20 time steps x 100 posterior samples
  * intermediate: intermediate outputs from data processing steps
    * clipped_clim_50.RData: Dataframe with coordinates, time, and climate reconstructions for narrower spatial extent and coarser temporal resolution that matches STEPPS data product. Only contains data from the 50 CE timestep. This is useful because all spatial locations are represented once.
    *  clipped_clim_alltime.RData: Dataframe with coordinates, time, and climate reconstructions for narrower spatial extent and coarser temporal resolution that matches STEPPS data product. Contains data from all time steps and spatial locations are repeated at each time step.
    *  clipped_pls.RData: PLS data frame with clipped spatial extent (removes lower Michigan since it is not within our domain). Only kept so that this step does not need to be run again.
    *  matching_intermediate.RData: This file was created in https://github.com/amwillson/historic-modern-environment/. Intermediate data product with match between grid cell ID and lat/lon for PLS. Used to add grid cell centroid coordinates to climate reconstruction grid cells after aggregating to the STEPPS grid cell.
    *  mean_average_annual_temperature.RData: Average annual temperature climate temporal summary. The downscaled climate product has monthly resolution and this is averaged to 51-year annual average.
    *  mean_precipitation_seasonality.RData: Precipitation seasonality (SD and CV) temporal summary. The downscaled climate product has monthly resolution and this is averaged to 51-year annual average.
    *  mean_temperature_seasonality.Rdata: Temperature seasonality (SD) temporal summary. The downscaled climate product has monthly resolution and this is averaged to 51-year annual average.
    *  mean_total_annual_precipitation.RData: Total annual precipitation climate temporal summary. The downscaled climate product has monthly resolution and this is averaged to 51-year annual average.
    *  point_matched_clim.RData: Dataframe with contents of clipped_clim_50.RData plus the coordinate sof the closest PLS point
    *  stepps_post_subsampled.RData: Relative abundance estimates from posterior draws of STEPPS, but for a subset of spatio-temporal locations. Formatted in data frame
    *  stepps_subsampled.RData: Mean estimates of STEPPS relative abundances, but for a subset of spatio-temporal locations. Formatted in data frame
    *  taxon_insample_soil.RData: Intermediate product with STEPPS mean relative abundance and soil, but not climate covariates. In sample spatiotemporal locations only
    *  taxon_oos_soil.Rdata: Intermediate data product with STEPPS mean relative abundance and soil, but not climate covariates. OOS time step (300 YBP) only
  *  processed: processed data for analysis
    * gridded_climate.RData: Data frame with gridded climate reconstructions at the spatial and temporal scales of the STEPPS data product
    * mean_stepps_full_oos.RData: Data frame with all spatiotemporal observations not used to fit the final model with mean STEPPS relative abundances. Used for the final validation step
    * mean_stepps_soil_clim.RData: Data frames (in sample and OOS for the 300 YBP time step) for fitting original models and running first validation step
    * mean_STEPPS.RData: processed mean relative abundances
    * post_stepps_soil_clim.RData: Data frame with all 
