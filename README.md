# Overview

This repo contains code written by AM Willson for Willson et al. (in prep). The code is broken into four sections. The first section organizes the output of the STEPPS pollen vegetation model (previously published). The second section processes downscaled climate reconstructions from Rena Guaita. The third section formats data, runs the model, plots output, and performs out-of-sample validation using the mean relative abundance estimates from STEPPS. The fourth section is identical to the third section, but uses the posterior draws from STEPPS instead of the means. We use the GJAM model to fit the relationship between environmental variables and taxon-level relative abundances across the last 2,000 years of the pre-Industrial Holocene across the Upper Midwest , US. We use the model to estimate environment-vegetation relationships and residual relationships between taxa.

# License

This repository holds an MIT License, as described in the LICENSE file.

# Software versions

This repository was built in the R environment on two machines. The majority of files were originally built to be run on R version 4.4.0. Scripts clearly identified as being run on a virtual machine (VM) were run on a Docker stack with R version 4.1.3.

# Package versions

-   `car` v. 3.1.2
-   `corrplot` v. 0.92
-   `cowplot` v. 1.1.3 (v. 1.1.1 on VM)
-   `dplyr` v. 1.1.4 (v. 1.0.8 on VM)
-   `fields` v. 15.2 (v. 13.3 on VM)
-   `ggplot2` v. 3.5.1 (v. 3.3.6 on VM)
-   `gjam` v. 2.6.2 (v. 2.6 on VM)
-   `lme4` v. 1.1.35.3 (v. 1.1.29 on VM)
-   `ncdf4` v. 1.22
-   `R.matlab` v. 3.7.0
-   `RColorBrewer` v. 1.1.3
-   `reshape2` v. 1.4.4
-   `sf` v. 1.0.16 (v. 1.0.7 on VM)
-   `sfheaders` v. 0.4.4 (v. 0.4.0 on VM)
-   `stats` v 4.4.0 (v. 4.1.3 on VM)
-   `tibble` v. 3.2.1 (v. 3.1.8 on VM)
-   `tidyr` v. 1.3.1 (v. 1.2.0 on VM)
-   `tidytext` v. 0.4.2
-   `tigris` v. 2.1 (v. 1.6 on VM)

# Directory structure

## data

**Input** data, **intermediate** data products, and **processed** data products from processing data and combining data sources in space and time. Any data product \< \~250 MB is contained in these directories. Any data product \< 100 MB is pushed to Github. Anything between 100-250 MB is available upon request because the files are too large to be housed on Github. Anything \> 250 MB is stored on an external hard drive, which is reflected in the directory naming conventions. The input data products are ultimately processed and then used in the analysis portion of the workflow.

### input: inputs to this workflow

-   8km.RData: Dataframe with grid cell IDs and grid cell centroids from PLS data. This file was created in <https://github.com/amwillson/historic-modern-environment/>. It is required because the soil data used in this analysis were originally geospatially matched to the PLS data. The PLS grid is the same as the STEPPS grid, so this product is used to simply match the soil and STEPPS grids in the same way.
-   gridded_soil.RData: Dataframe with soil data across space from gSSURGO. The file was created in <https://github.com/amwillson/historic-modern-environment/> for the modern period, which we assume is consistent with the paleorecord.
-   total_matched.RData: Dataframe with the mapping from PLS points to PLS grid cells. This file was created in <https://github.com/amwillson/historic-modern-environment/>. This dataframe tells us which grid cell each PLS point falls within. Used to determine which STEPPS grid cell each climate grid cell falls within, based on which PLS point each climate grid cell is closest to.
-   msb-paleon-2: directory downloaded from <https://doi.org/10.6073/pasta/338e778c00b13acd278302e82f992415>. Contains mean and SD of STEPPS relative abundance
    -   2Kyrs_Comp_Mean_Level2_v1.0.nc: mean relative abundance from STEPPS
    -   2Kyrs_Comp_SD_Level2_v1.0.nc: standard deviation of relative abundance from STEPPS. Not used.
    -   manifest.txt: Manifest file from archived data product
    -   msb-paleon.22.1.report.xml: Documentation for archived data product
    -   msb-paleon.22.1.txt: Documentation for archived data product
    -   msb-paleon.22.1.xml: Documentation for archived data product
-   PLS_point: directory with RData files each containing point-level observations of taxon occurrences from the Public Land Survey (PLS). Files were generated in <https://github.com/amwillson/historic-modern-environment/>. The STEPPS model was calibrated on the PLS. We therefore have the PLS points that are in each STEPPS grid cell. Used this relationship to 1) find which PLS point each climate reconstruction is closest to; 2) identify which grid cell each PLS point falls within; and 3) average over all climate reconstrutions in teh same grid cell according to which PLS point it is closest to.
    -   minnesota_process.RData: PLS point-level observations for Minnesota
    -   upmichigan_process.RData: PLS point-level observations for Upper Michigan
    -   wisconsin_process.RData: PLS point-level observations for Wisconsin
-   posterior_stepps: directory with posterior draws from the STEPPS model. This was not previously published. It will be made available upon request.
    -   input.rdata: Contains indexing associated with the posterior draws. For this project, the R object `centers_veg` is the data object of interest, which contains coordinates for the centroids of each location in the samples array
    -   rIts_sub.RDS: array with relative abundance estimates for 100 posterior samples from STEPPS. Dimensions: 704 spatial locations x 12 taxa x 20 time steps x 100 posterior samples

### intermediate: intermediate outputs from data processing steps

-   clipped_clim_50.RData: Dataframes with coordinates, time, and climate reconstructions for narrower spatial extent and coarser temporal resolution that matches STEPPS data product. Only contains data from the 50 CE timestep. This is useful because all spatial locations are represented once. Two dataframes, one with climate estimates and one with debiased climate estimates, with dimensions of 8188 (observations in space and time) x 9 variables
-   clipped_clim_alltime.RData: Dataframe with coordinates, time, and climate reconstructions for narrower spatial extent and coarser temporal resolution that matches STEPPS data product. Contains data from all time steps and spatial locations are repeated at each time step. Two dataframes, one with climate estimates and one with debiased climate estimates, with dimensions of 132768 (observations in space and time) x 9 variables
-   clipped_pls.RData: PLS data frame with clipped spatial extent (removes lower Michigan since it is not within our domain). Only kept so that this step does not need to be run again. One dataframe with 507062 (spatial locations) x 3 variables (coordinates + unique identifier)
-   matching_intermediate.RData: This file was created in <https://github.com/amwillson/historic-modern-environment/>. Intermediate data product with match between grid cell ID and lat/lon for PLS. Used to add grid cell centroid coordinates to climate reconstruction grid cells after aggregating to the STEPPS grid cell. Two dataframes, each with 691349 observations (spatial locations). One has all PLS data (35 variables) and one has only information relevant for matching points and grid cells (6 variables)
-   mean_average_annual_temperature.RData: Average annual temperature climate temporal summary. The downscaled climate product has monthly resolution and this is averaged to 51-year annual average. Two arrays, one with climate estimates and one with debiased climate estimates, each with dimensions 201 (lat) x 113 (lon) x 18 (time)
-   mean_precipitation_seasonality.RData: Precipitation seasonality (SD and CV) temporal summary. The downscaled climate product has monthly resolution and this is averaged to 51-year annual average. Two arrays, one with climate estimates and one with debiased climate estimates, each with dimensions 201 (lat) x 113 (lon) x 18 (time)
-   mean_temperature_seasonality.Rdata: Temperature seasonality (SD) temporal summary. The downscaled climate product has monthly resolution and this is averaged to 51-year annual average. Two arrays, one with climate estimates and one with debiased climate estimates, each with dimensions 201 (lat) x 113 (lon) x 18 (time)
-   mean_total_annual_precipitation.RData: Total annual precipitation climate temporal summary. The downscaled climate product has monthly resolution and this is averaged to 51-year annual average. Two arrays, one with climate estimates and one with debiased climate estimates, each with dimensions 201 (lat) x 113 (lon) x 18 (time)
-   point_matched_clim.RData: Dataframe with contents of clipped_clim_50.RData plus the coordinate sof the closest PLS point. Dataframe with 507062 (spatial locations) x 12 variables (climate variables and spatial indexing)
-   stepps_post_subsampled.RData: Relative abundance estimates from posterior draws of STEPPS, but for a subset of spatio-temporal locations. Formatted in data frame with 32000 observations (in space and time, for 100 posetrior draws) of 16 variables (relative abundances and spatiotemporal indexing) (in sample) and dataframe with 8000 observations of 16 variables (out-of-sample)
-   stepps_subsampled.RData: Mean estimates of STEPPS relative abundances, but for a subset of spatio-temporal locations. Formatted in data frame with 320 observations (in space and time) of 15 variables (relative abundances and spatiotemporal indexing) (in sample) and dataframe with 80 observations of 15 variables (out-of-sample)
-   taxon_insample_soil.RData: Intermediate product with STEPPS mean relative abundance and soil, but not climate covariates. In sample spatiotemporal locations only (320 observations of 21 variables)
-   taxon_oos_soil.Rdata: Intermediate data product with STEPPS mean relative abundance and soil, but not climate covariates. OOS time step (300 YBP) only (80 observations of 21 variables)

### processed: processed data for analysis

-   gridded_climate.RData: Data frame with gridded climate reconstructions at the spatial and temporal scales of the STEPPS data product
-   mean_stepps_full_oos.RData: Data frame with all spatiotemporal observations not used to fit the final model with mean STEPPS relative abundances. Used for the final validation step
-   mean_stepps_soil_clim.RData: Data frames (in sample and OOS for the 300 YBP time step) for fitting original models and running first validation step
-   mean_STEPPS.RData: processed mean relative abundances
-   post_stepps_full_oos.RData: Data frame with all spatiotemporal observations not used
-   post_stepps_soil_clim.RData: Data frame with posterior estimates from STEPPS with co-located climate and soil
-   post_STEPPS.RData: processed relative abundances from 100 posterior draws of STEPPS

### Climate_Downscaling_RG: climate data stored on external hard drive

-   Each file is in .mat format and contains downscaled climate estimates of one climate variable (average annual temperature [aat] or total annual precipitation [pr]) for one month (numbered)
-   processed_monthly_Rdata: contains an RData file for each month of each climate variable in array format with dimensions lon x lat x year for estimate and lon x lat for bias

## out

**Outputs** and out-of-sample **validation** for GJAM using either mean relative abundances (**mean**) or posterior draws from STEPPS (**posteriors**)

### mean

-   mean_sand_aat_tpr_prsd_300YBP.RData: Output of fitting GJAM with mean relative abundances from STEPPS at 1900, 1500, 1100, 700, 300 YBP time steps with soil sand content, average annual temperature, total annual precipitation, precipitation seasonality covariates
-   mean_sand_aat_tpr_prsd.RData: Output of fitting GJAM with mean relative abundances from STEPPS at 1900, 1500, 1100, and 700 YBP time steps with soil sand content, average annual temperature, total annual precipitation, precipitation seasonality covariates
-   mean_sand_aat_tsd_prsd.RData: Output of fitting GJAM with mean relative abundances from STEPPS at 1900, 1500, 1100, and 700 YBP time steps with soil sand content, average annual temperature, temperature seasonality, precipitation seasonality covariates
-   mean_silt_aat_tpr_prsd.RData: Output of fitting GJAM with mean relative abundances from STEPPS at 1900, 1500, 1100, and 700 YBP time steps with soil silt content, average annual temperature, total annual precipitation, precipitation seasonality covariates
-   mean_silt_aat_tsd_prsd.RData: Output of fitting GJAM with mean relative abundances from STEPPS at 1900, 1500, 1100, and 700 YBP time steps with soil silt content, average annual temperature, temperature seasonality, and precipitation seasonality covariates
-   oos_prediction_all.RData: GJAM output for predicting mean relative abundances using non-conditional, full conditional, and conditional on oak prediction methods. The non-conditional and conditional on oak outputs are one prediction object. The full conditional output is a list of outputs for 11 out-of-sample predictions (one for each taxon, given all the others). Prediction is made for all spatiotemporal locations not used to fit the model including 300 YBP
-   oos_prediction_time.RData: GJAM output for predicting mean relative abundances using non-conditional, full conditional, and conditional on oak prediction methods. The non-conditional and conditional on oak outputs are one prediction object. The full conditional object is a list of outputs for 11 out-of-sample predictions (one for each taxon, given all the others). Prediction is made for a subest of spatial locations at the 300 YBP time step
-   processed_sand_aat_tpr_prsd.RData: Dataframes for each type of parameter (bFacGibbs, bgibbs, bgibbsUn, fSensGibbs, sgibbs) with Gibbs samples from GJAM model fitted with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality and mean relative abundance
-   processed_sand_aat_tsd_prsd.RData: Dataframes for each type of parameter (bFacGibbs, bgibbs, bgibbsUn, fSensGibbs, sgibbs) with Gibbs samples from GJAM model fitted with soil sand content, average annual temperature, temperature seasonality, and precipitation seasonality and mean relative abundance
-   processed_silt_aat_tpr_prsd.RData: Dataframes for each type of parameter (bFacGibbs, bgibbs, bgibbsUn, fSensGibbs, sgibbs) with Gibbs samples from GJAM model fitted with soil silt content, average annual temperature, temperature seasonality, and precipitation seasonality and mean relative abundance
-   processed_silt_aat_tsd_prsd.RData: Dataframes for each type of parameter (bFacGibbs, bgibbs, bgibbsUn, fSensGibbs, sgibbs) with Gibbs samples from GJAM model fitted with soil silt content, average annual temperature, temperature seasonality, and precipitation seasonality and mean relative abundance

### posteriors

-   GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP.RData: Output of fitting GJAM 100 times with 100 posterior draws from STEPPS at 1900, 1500, 1100, 700, and 300 YBP time steps with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates. The output is in a list of length 100, with each object in the list one output from GJAM
-   GJAM_STEPPS_post_sand_aat_tpr_prsd.RData: Output of fitting GJAM 100 times with 100 posterior draws from STEPPS at 1900, 1500, 1100, and 700 YBP time steps with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates. The output is in a list of length 100, with each object in the lsit one output from GJAM
-   GJAM_STEPPS_post_sand_aat_tsd_prsd.RData: Output of fitting GJAM 100 times with 100 posterior draws from STEPPS at 1900, 1500, 1100, and 700 YBP time steps with soil sand content, average annual temperature, temperature seasonality, and precipitation seasonality covariates. The output is in a list of length 100, with each object in the list one output from GJAM
-   GJAM_STEPPS_post_silt_aat_tpr_prsd.RData: Output of fitting GJAM 100 times with 100 posterior draws from STEPPS at 1900, 1500, 1100, and 700 YBP time steps with soil silt content, average annual temperature, total annual precipitation, and precipitation seasonality covariates. The output is in a list of length 100, with each object in the list one output from GJAM
-   GJAM_STEPPS_post_silt_aat_tsd_prsd.RData: Output of fitting GJAM 100 times with 100 posterior draws from STEPPS at 1900, 1500, 1100, and 700 YBP time steps with soil silt content, average annual temperature, temperature seasonality, and precipitation seasonality covariates. The output is in a list of length 100, with each object in the list one output from GJAM
-   oos_prediction_conditionaloak_time.RData: GJAM output for predicting the relative abundance of each posterior draw using prediction conditional on oak method. The prediction is in a list of length 100, with each object in the list the prediction of one posterior draw from STEPPS. Prediction is for a subset of locations for the 300 YBP time step
-   oos_prediction_nonconditional_time.RData: GJAM output for predicting the relative abundance of each posterior draw using non-conditional prediction. The prediction is in a list of length 100, with each object in the list the prediction of one posterior draw from STEPPS. Prediction is for a subset of locations for the 300 YBP time step
-   sand_aat_tpr_prsd: processed Gibbs samples for each parameter type from 100 fits of GJAM with 100 posterior draws from STEPPS and the soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates
    -   bFacGibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for beta coefficients standardized for X (independent variables) and W (latent covariance)
    -   bgibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for beta coefficients standardized for X
    -   bgibbsUn.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for unstandardized beta coefficients
    -   fSensGibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for joint sensitivity of all resopnse variables to each independent variable
    -   sgibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for response variable covariance
    -   parameter_summaries.RData: dataframes with parameter summaries (mean, sd, median, 2.5%, 25%, 75%, and 97.5% quantiles) for each parameter type
-   sand_aat_tpr_prsd_300YBP: processed Gibbs samples for each parameter type from 100 fits of GJAM with 100 posterior draws from STEPPS and the soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates. Includes 300 YBP time step for fitting the model
    -   bFacGibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for beta coefficients standardized for X (independent variables) and W (latent covariance)
    -   bgibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for beta coefficients standardized for X
    -   bgibbsUn.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for unstandardized beta coefficients
    -   fSensGibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for joint sensitivity of all resopnse variables to each independent variable
    -   sgibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for response variable covariance
    -   parameter_summaries.RData: dataframes with parameter summaries (mean, sd, median, 2.5%, 25%, 75%, and 97.5% quantiles) for each parameter type
-   sand_aat_tsd_prsd: processed Gibbs samples for each parameter type from 100 fits of GJAM with 100 posterior draws from STEPPS and the soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates
    -   bFacGibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for beta coefficients standardized for X (independent variables) and W (latent covariance)
    -   bgibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for beta coefficients standardized for X
    -   bgibbsUn.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for unstandardized beta coefficients
    -   fSensGibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for joint sensitivity of all resopnse variables to each independent variable
    -   sgibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for response variable covariance
    -   parameter_summaries.RData: dataframes with parameter summaries (mean, sd, median, 2.5%, 25%, 75%, and 97.5% quantiles) for each parameter type
-   silt_aat_tpr_prsd: processed Gibbs samples for each parameter type from 100 fits of GJAM with 100 posterior draws from STEPPS and the soil silt content, average annual temperature, total annual precipitation, and precipitation seasonality covariates
    -   bFacGibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for beta coefficients standardized for X (independent variables) and W (latent covariance)
    -   bgibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for beta coefficients standardized for X
    -   bgibbsUn.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for unstandardized beta coefficients
    -   fSensGibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for joint sensitivity of all resopnse variables to each independent variable
    -   sgibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for response variable covariance
    -   parameter_summaries.RData: dataframes with parameter summaries (mean, sd, median, 2.5%, 25%, 75%, and 97.5% quantiles) for each parameter type
-   silt_aat_tsd_prsd: processed Gibbs samples for each parameter type from 100 fits of GJAM with 100 posterior draws from STEPPS and the soil silt content, average annual tempreature, temperature seasonality, and precipitation seasonality covariates
    -   bFacGibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for beta coefficients standardized for X (independent variables) and W (latent covariance)
    -   bgibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for beta coefficients standardized for X
    -   bgibbsUn.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for unstandardized beta coefficients
    -   fSensGibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for joint sensitivity of all resopnse variables to each independent variable
    -   sgibbs.RDS: dataframe with Gibbs samples across all 100 fits of GJAM for response variable covariance
    -   parameter_summaries.RData: dataframes with parameter summaries (mean, sd, median, 2.5%, 25%, 75%, and 97.5% quantiles) for each parameter type

## R

**Code** for processing data, running models, processing output, and plotting results. Broken into four sections: **1.Format_STEPPS**, **2.Process_climate**, **3.mean_STEPPS_gjam**, **4.posterior_draws_boostrap**. Additionally includes 2 helper files (**climate_dimensions.R** and **funs.R**), a **deprecated** folder, and a directory for determining subsampling scheme (**Testing_subsampling_methods**).

### 1.Format_STEPPS

-   **1.1.Format_STEPPS_mean_arrays.R**: Reading in and formatting STEPPS mean data product. Going from NCDF data structure to arrays
    -   Inputs:
        -   data/input/msb-paleon-2/2Kyrs_Comp_Mean_Level2_v1.0.nc: netCDF file with mean relative abundances from STEPPS model from <https://portal.edirepository.org/nis/mapbrowse?scope=msb-paleon&identifier=22>
    -   Outputs:
        -   data/processed/mean_STEPPS.RData: coordinates, time, and relative abundances in array format. Used in 1.2.Plot_STEPPS.R, 3.1.subsample_stepps.R, and 3.11.format_final_oos.R
-   **1.2.Plot_STEPPS_mean.R**: Plotting mean relative abundances over space and time
    -   Inputs:
        -   data/processed/mean_STEPPS.RData: coordinates, time, and relative abundances in array format
    -   Outputs: none
-   **1.3.Format_STEPPS_draws.R**: Formatting STEPPS posterior draws. Posterior draws are already in array format but there are no associated dimensions, so need to apply those from separate data object
    -   Inputs:
        -   data/input/posterior_stepps/rIts_sub.RDS: Array with relative abundance estimates for 100 posterior samples from STEPPS model. Dimensions: 704 spatial locations x 12 taxa x 20 time steps x 100 posterior samples. Currently available privately, but will be made publicly available upon request
        -   data/input/posterior_stepps/input.rdata: Associated indexing data for the posterior draws. For this project, centers_veg is the data object of interest, which contains coordinates for the centroids of each "location" in the samples array
    -   Outputs:
        -   data/processed/post_STEPPS.RData: array with dimension names and formatting. Used in 1.4.plots_stepps_draws.R, 4.1.subsample_stepps_draws.R, and 4.11.format_final_oos_draws.R
-   **1.4.Plot_STEPPS_draws.R**: Plotting relative abundance estimates from posterior draws of STEPPS over space and time
    -   Inputs:
        -   data/processed/post_STEPPS.RData: relative abundances in array format
    -   Outputs: none

### 2.Process_climate

-   **2.1.process_climate.R**: Formatting downscaled climate reconstructions. Going from .mat data structure to arrays
    -   Inputs:
        -   Each .mat file from /Volumes/FileBackup/Climate_Downscaling_RG/: .mat files are stored separately because of large data volume. Matlab file structure with one climate variable for one month in each file. Files are privately archived and will be made publicly available upon publication of the data product.
    -   Outputs:
        -   each .RData file from /Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/: .RData files are stored separately because of large data volume. Each .RData file contains 2 arrays, one with downscaled estimate and one with bias. Used in 2.2.climate_temporal_summaries.R
-   **2.2.climate_temporal_summaries.R**: Summarize temperature and precipitation downscaled reconstructions over time. Calculating four climate variables for 50-year windows: 1. average annual temperature (for each year, take average of monthly temperature; for all years within window, take average again); 2. total annual precipitation (for each year, take sum of monthly precipitation; for all years within window, take average); 3. temperature seasonality (for each year, take standard deviation of monthly temperature; for all years within window, take average); 4. precipitation seasonality (for each year, take standard deviation of monthly precipitation; for all years within window, take average). Calculated on both biased and bias-corrected climate estimates. Seasonality methods from <https://pubs.usgs.gov/ds/691/ds691.pdf>
    -   Inputs:
        -   Each .RData file from /Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/: .RData files are stored separately because of large data volume. Each .RData file contains 2 arrays, one with downscaled estimate and one with bias
    -   Outputs:
        -   data/intermediate/tas_201.113.1776.12.RData: Intermediate arrays for estimated and de-biased air temperature with dimensions of lat x lon x year x month. Not used, just saved so it does not need to be recomputed
        -   data/intermediate/pr_201.113.1776.12.RData: Intermediate arrays for estimated and de-biased total precipitation with dimensiosn of lat x lon x year x month. Not used, just saved so it does not need to be recomputed
        -   data/intermediate/mean_average_annual_temperature.RData: Intermediate arrays for estimated and de-biased average annual temperature calculated by summarizing over months and then 50-year windows. Used in 2.3.climate_clip_spatial_extent.R
        -   data/intermediate/mean_temperature_seasonality.RData: Intermediate arrays for estimated and de-biased temperature seasonality calculated by summarizing over months and then 50-year windows. Used in 2.3.climate_clip_spatial_extent.R
        -   data/intermediate/mean_total_annual_precipitation.RData: Intermediate arrays for estimated and de-biased total annual precipitation calculated by summarizing over months and then 50-year windows. Used in 2.3.climate_clip_spatial_extent.R
        -   data/intermediate/mean_precipitation_seasonality.RData: Intermediate arrays for estimated and de-biased precipitation seasonality calculated by summarizing over months and then 50-year windows. Used in 2.3.climate_clip_spatial_extent.R
-   **2.3.climate_clip_spatial_extent.R**: Perform checks on spatio-temporal variability of downscaled climate reconstructions. Then clip the extent of the STEPPS vegetation reconstructions. Prepares climate data to be matched to PLS points in the next step.
    -   Inputs:
        -   data/intermediate/mean_average_annual_temperature.RData: Climate summaries in array format from previous step
        -   data/intermediate/mean_total_annual_precipitation.RData: Climate summaries in array format from previous step
        -   data/intermediate/mean_temperature_seasonality.RData: Climate summaries in array format from previous step
        -   data/intermediate/mean_precipitation_seasonality.RData: Climate summaries in array format from previous step
    - Outputs:
        - data/intermediate/clipped_clim_50.RData: Dataframes with cordinates, time, and climate reconstructions for narrower spatial extent and coarser temporal resolution that matches STEPPS data product. Contains data only from the 50 CE timestep. This is useful because all spatial locations are represented once. Used in 2.4.climate_points_to_grid.R
        - data/intermediate/clipped_clim_alltime.RData: Dataframes with coordinates, time, and climate reconstructions for narrow spatial extent and coarser temporal resolution that matches STEPPS data product. Contains data from all time steps and spatial locations are repeated at each time step. Used in 2.5.climate_aggregate_to_grid.R
- **2.4.climate_points_to_grid.R**: Second part of spatially matching climate to STEPPS. Matches climate grid to point-level PLS data. NOTE: This is separate because this part must be run on VM due to memory constraints. Requires approximately 40 GB RAM as written.
    - Inputs:
        - data/intermediate/clipped_clim_50.RData: Climate reconstructions for one period of time, with all spatial locations
        - .RData files in data/input/PLS_point/: Contain dataframes with point-level observations of taxon occurrences from the Public Land Survey (PLS). The STEPPS model was calibrated on the PLS. From a previous project, we have the PLS points that are in each STEPPS grid cell. We can use this relationship for teh climate data as follows: 1. Find which PLS point each climate reconstruction is closest to; 2. identify which grid cell each PLS point falls within; 3. average over all climate reconstructions in the same grid cell according to which PLS point it is closest to.
    - Outputs:
        - data/intermediate/clipped_pls.RData: PLS data frame with clipped spatial extent (removes lower Michigan since it is not within our domain). Only kept so that this step does not need to be run again. Not used in another step
        - data/intermediate/point_matched_clim.RData: Dataframe with contents of clipped_clim_50.RData plus the coordinates of the closest PLS point. Used in 2.5.climate_aggregate_to_grid.R
- **2.5.climate_aggregate_to_grid.R**: Third part of spatially matching climate to PLS. Aggregates climate reconstructions to STEPPS grid cell size. Then estimates "empty" grid cells. Empty grid cells happen because some grid cells have no PLS points. This is because or large Native American reservations and islands. We have climate reconstructions for these locations, though, so we simply find and add the climate reconstructions for this subset of locations. NOTE: This part should be run on local machine. Only step 2-4 should be run on the VM because other steps should use the most recent version of R (4.4.0).
    - Inputs:
        - data/intermediate/point_matched_clim.RData: Matching between points and grid cells in the PLS data. Tells us which climate reconstruction grid cells correspond to which STEPPS grid cells
        - data/intermediate/clipped_clim_alltime.RData: All climate reconstructions for the more limited spatial extent of our region of interest. All time points, which means there are repeated spatial locations
        - data/input/total_matched.RData: Dataframe with the mapping from PLS points to PLS grid cells. Tells us which grid cell each PLS point falls within. We use this to determine which STEPPS grid cell each climate grid cell falls within, based on which PLS point each climate grid cell is closest to.
        - data/intermediate/matching_intermediate.RData: Intermediate data product with match between grid cell ID and lat/lon for PLS. Use this to add grid cell centroid coordinates to climate reconstruction grid cells after aggregating to the STEPPS grid cell
        - data/input/8km.RData: Used to identify which grid cells have no PLS points, so we can manually identify climate reconstruction points that are within those grid cells
    - Outputs:
        - data/processed/climate_gridded.RData: Dataframe with gridded climate reconstructions at the spatial and temporal scales of the STEPPS data product. Used in 3.2.stepps_soil_climate_formatting.R

### 3.mean_STEPPS_gjam

- **3.1.subsample_stepps.R**: Formatting for fitting GJAM with mean STEPPS relative abundances. Requires subsampling in space and time to reduce correlations between adjacent estimates in space and time. The optical subsampling approach was determined using the files in R/Testing_subsampling_methods/
    - Inputs:
        - data/processed/mean_STEPPS.RData: mean estimates of relative abundances from STEPPS
    - Outputs:
        - data/intermediate/stepps_subsampled.RData: mean estimates of STEPPS relative abundances, but for a subset of spatio-temporal locations. Used in 3.2.stepps_soil_climate_formatting.R and 3.11.format_final_oos.R
- **3.2.stepps_soil_climate_formatting.R**: Adding soil and climate reconstructions to STEPPS subsampled mean relative abundances
    - Inputs:
        - data/intermediate/stepps_subsampled.RData: Dataframe with subset of spatio-temporal locations
        - data/input/8km.RData: Dataframe with grid ID and grid centroid lat/lon from PLS data. This is required because the soil varaible estimates were originally sampled using the PLS data
        - data/input/gridded_soil.RData: Soil data that was previously processed for the PLS dataset. In the "input" folder because this was processed for a different project, not processed for this project
        - data/processed/gridded_climate.RData: Downscaled climate reconstructions. In the "processed" folder because this was downscaled and processed for this project
    - Output:
        - data/intermediate/taxon_insample_soil.RData: Dataframe with STEPPS mean relative abundances and soil variables with in-sample data (excluding final time step, 300 YBP). Not used, just an intermediate data step
        - data/intermediate/taxon_oos_soil.RData: Dataframe with STEPPS mean relative abundances and soil variables with out-of-sample data (only 300 YBP time step). Not used, just an intermediate data step
        - data/processed/mean_stepps_soil_clim.RData: Two dataframes, in-sample and out-of-sample, with STEPPS mean relative abundances, soil variables, and climate variables. Used in 3.3.run_gjam.R, 3.6.oos_prediction.R, and 4.2.stepps_draws_soil_climate_formatting.R
- **3.3.run_gjam.R**: Running GJAM for mean STEPPS data. First perform variable selection: choosing to keep silt OR sand, average annual temperature, total annual precipitation OR temperature seasonality, and precipitation seasonality. Based on correlations and VIFs. Then formatting ydata for GJAM. Removing the ash taxon because it never is very abundant and GJAM has an error related to strong multicollinearity when ash is included, but not when it is removed. Then running GJAM
    - Inputs:
        - data/processed/mean_stepps_soil_clim.RData: Dataframes with in-sample and out-of-sample data with co-located reconstructions of 12 taxa's relative abundance, soil variables and climate variables. Only the in sample data is used here
    - Outputs:
        - out/mean/mean_sand_aat_tpr_prsd.RData: output object from fitting GJAM with soil sand content, average annual temperature, total annual precipitation and precipitation seasonality. Used in 3.4.process_out_gjam_mean.R and 3.6.oos_prediction.R
        - out/mean/mean_silt_aat_tpr_prsd.RData: output object from fitting GJAM with soil silt content, average annual temperature, total annual precipitation, and precipitation seasonality. Used in 3.4.process_out_gjam_mean.R and 3.6.oos_prediction.R
        - out/mean/mean_sand_aat_tsd_prsd.RData: output object from fitting GJAM with soil sand content, average annual temperature, temperature seasonality, and precipitation seasonality. Used in 3.4.process_out_gjam_mean.R and 3.6.oos_prediction.R
        - out/mean/mean_silt_aat_tsd_prsd.RData: output object from fitting GJAM with soil silt content, average annual temperature, temperature seasonality, and precipitation seasonality. Used in 3.4.process_out_gjam_mean.R and 3.6.oos_prediction.R
- **3.4.process_out_gjam_mean.R**: Processing output from GJAM model fit with STEPPS mean relative abundances
    - Inputs:
        - out/mean/mean_silt_aat_tpr_prsd.RData: output object from fitting GJAM with mean silt content, average annual temperature, total annual precipitation, and precipitation seasonality
        - out/mean/mean_sand_aat_tpr_prsd.RData: output object from fitting GJAM with mean sand content, average annual temperature, total annual precipitation, and precipitation seasonality
        - out/mean/mean_silt_aat_tsd_prsd.RData: output object from fitting GJAM with mean silt content, average annual temperature, temperature seasonality, and precipitation seasonality
        - out/mean/mean_sand_aat_tsd_prsd.RData: output object from fitting GJAM with mean sand content, average annual temperature, temperature seasonality, and precipitation sesasonality
    - Outputs:
        - out/mean/processed_silt_aat_tpr_prsd.RData: 5 dataframes, one for each parameter type, with full MCMC chains ready for plotting from GJAM fit with soil silt content, average annual temperature, total annual precipitation, and precipitation seasonality. Used in 3.5.plot_output_mean.R
        - out/mean/processed_sand_aat_tpr_prsd.RData: 5 dataframes, one for each parameter type, with full MCMC chains ready for plotting from GJAM fit with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality. Used in 3.5.plot_output_mean.R
        - out/mean/processed_silt_aat_tsd_prsd.RData: 5 dataframes, one for each parameter type, with full MCMC chains ready for plotting from GJAM fit with soil silt content, average annual temperature, temperature seasonality, and precipitation seasonality. Used in 3.5.plot_output_mean.R
        - out/mean/processed_sand_aat_tsd_prsd.RData: 5 dataframes, one for each parameter type, with full MCMC chains ready for plotting from GJAM fit with soil sand content, average annual temperature, temperature seasonality, and preicpitation seasonality. Used in 3.5.plot_output_mean.R
- **3.5.plot_output_mean.R**: Plot output of GJAM fitted with mean relative abundance estimates from STEPPS
    - Inputs:
        - out/mean/processed_silt_aat_tpr_prsd.RData: 5 dataframes, one for each parameter type, with full MCMC chains ready for plotting from GJAM fit with soil silt content, average annual temperature, total annual precipitation, and precipitation seasonality
        - out/mean/processed_sand_aat_tpr_prsd.RData: 5 dataframes, one for each parameter type, with full MCMC chains ready for plotting from GJAM fit with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality
        - out/mean/processed_silt_aat_tsd_prsd.RData: 5 dataframes, one for each parameter type, with full MCMC chains ready for plotting from GJAM fit with soil silt content, average annual temperature, temperature seasonality, and precipitation seasonality
        - out/mean/processed_sand_aat_tsd_prsd.RData: 5 dataframesone for each parameter type, with full MCMC chians ready for plotting from GJAM fit with soil sand content, average annual temperature, temperature seasonality, and precipitation seasonality
    - Outputs: none
- **3.6.oos_prediction.R**: Out-of-sample prediction in time (using last time step, 300 YBP). Using mean STEPPS relative abundances
    - Inputs:
        - out/mean/mean_sand_aat_tpr_prsd.RData: Output from GJAM with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates with mean relative abundance
        - data/processed/mean_stepps_soil_clim.RData: contains out-of-sample data
    - Outputs:
        - out/mean/oos_prediction_time.RData: out-of-sample predictions for non-conditional, conditional, and conditional on oak predictions. Non-conditional and conditional on oak predictions have a single output object, while conditional prediction is stored in a list of length 11 (number of taxa). Out-of-sample predictions are made for the 300 YBP time period across the same spatial locations as used to fit the model
- **3.7.plot_oos_prediction.R**: Plotting out-of-sample prediction in time. Out-of-sample prediction of 300 YBP for same spatial locations as used to fit the model
    - Inputs:
        - out/mean/oos_prediction_time.RData: Out-of-sample predictions for 300 YBP using non-conditional, conditional, and conditional only on oak methods
        - data/processed/mean_stepps_soil_clim.RData: contains dataframe with out-of-sample data
    - Outputs: none
- **3.8.run_gjam_300YBP.R**: Running GJAM with mean STEPPS data including 300 YBP time step. First combining original in-sample and out-of-sample datasets because we already performed out-of-sample validation. Then formatting ydata for GJAM (same as step for 3-3). Then running GJAM with the soil sand content, average annual temperature, total annual precipitation and precipitaton seasonality covariates
    - Inputs:
        - data/processed/mean_stepps_soil_clim.RData: Dataframes with co-located reconstructions of 12 taxa's relative abundances, soil variables, and climate variables. In-sample and out-of-sample dataframes are combined in this script
    - Outputs:
        - out/mean/mean_sand_aat_tpr_prsd_300YBP.RData: output object from GJAM from fitting GJAM with the 300 YBP time step. Used in 3.9.process_out_gjam_300YBP.R and 3.11.oos_prediction_all.R
- **3.9.process_out_gjam_300YBP.R**: Processing output from GJAM model fit with STEPPS mean relative abundances indcluding 300 YBP time step. Also only including the model with the soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates because this was determined to be the most ecologically relevant model in steps 3-3 to 3-7
    - Inputs:
        - out/mean/mean_sand_aat_tpr_prsd_300YBP.RData: Output from GJAM including 300 YBP
    - Outputs:
        - out/mean/processed_sand_aat_tpr_prsd_300YBP.RData: 5 dataframes, one for each parameter type, with full MCMC chains for plotting
- **3.10.plot_output_300YBP.R**: Plot output of GJAM fitted with mean relative abundance estimates from STEPPS including 300 YBP time step. Identical to step 3-5 but including 300 YBP. Also only including the model with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates because this was determined to be the most ecologically relevant model in steps 3-3 to 3-7
    - Inputs:
        - out/mean/processed_sand_aat_tpr_prsd_300YBP.RData: 5 dataframes, one for each parameter type, with full MCMC chains for plotting
    - Outputs: none
- **3.11.format_final_oos.R**: Formatting final out-of-sample data. Taking full spatiotemporal extent of STEPPS mean relative abunances,, soil, and climate reconstructions. Removing in-sample data and saving the out-of-sample prediction domain
    - Inputs:
        - data/processed/mean_STEPPS.RData: mean estimates of relative abundances from STEPPS (all spatiotemporal locations from original data product)
        - data/intermediate/stepps_subsampled.RData: mean estimates of STEPPS relative abundances, but for a subset of spatio-temporal locations used to fit the first two rounds of the model analysis
        - data/input/8km.RData: Dataframe with grid ID and grid centroid lat/lon from PLS data. This is required because the soil variable estimates were originally sampled using PLS data
        - data/input/gridded_soil.RData: Soil data that was previously processed for the PLS dataset. In the "input" folder because this was processed for a different project, not processed for this project
        - data/processed/gridded_climate.RData: Downscaled climate reconstructions. In the "processed" folder because this was downscaled and processed for this project
    - Outputs:
        - data/processed/mean_stepps_full_oos.RData: full out-of-sample dataset with mean STEPPS relative abundances, soil, and climate reconstructions. Used in 3.12.oos_prediction_final.R and 3.13.plot_oos_prediction_final.R
- **3.12.oos_prediction_final.R**: Out-of-sample prediction in space and time (using all spatiotemporal locations not used to fit the model)
    - Inputs:
        - out/mean/mean_sand_aat_tpr_prsd_300YBP.RData: Output from GJAM fitted with all data from original model fitting and first round of validation, including 300 YBP time step
        - data/processed/mean_stepps_full_oos.RData: Dataframe with climate and soil data for all OOS locations in space and time
    - Outputs:
        - out/mean/oos_prediction_all.RData: GJAM output for non-conditional, full conditional, and conditional on oak predictions for all withheld spatiotemporal locations. Outputs for non-conditional prediction and prediction conditional only on oak are single GJAM out objects. The full conditional prediction is stored as a list of length 11 of prediction outputs (one for each taxon). Used in 3.13.plot_oos_prediction.R
- **3.13.plot_oos_prediction.R**: Plotting out-of-sample prediction in space and time. Out-of-sample prediction for all locations and times not used to fit the model
    - Inputs:
        - out/mean/oos_prediction_all.RData: Out-of-sample predictions for all spatiotemporal locations using non-conditional, conditional, and conditional only on oak methods (each in separate out or list object)
        - data/processed/mean_stepps_full_oos.RData: out-of-sample data for all spatiotemporal locations not used for model fitting
    - Outputs: none

### 4.posterior_draws_bootstrap

- **4.1.subsample_stepps_draws.R**: Formatting for fitting GJAM with draws of STEPPS relative abundances. Requires subsampling in space and time to reduce correlations between adjacent estimates in space and time. The optimal subsampling approach was determined using the files in R/Testing_subsampling_methods/
    - Inputs:
        - data/processed/post_STEPPS.RData: estimates of relative abundance from posterior draws of STEPPS
    - Outputs:
        - data/intermediate/stepps_post_subsampled.RData: relative abundance estimates from posterior draws of STEPPS, but for a subset of spatio-temporal locations. Used in 4.2.stepps_draws_soil_climate_formatting.R and 4.11.format_final_oos_draws.R
- **4.2.stepps_draws_soil_climate_formatting.R**: Adding soil and climate reconstructions to STEPPS subsampled relative abundance draws
    - Inputs:
        - data/intermediate/stepps_post_subsampled.RData: Dataframe with subset of spatio-temporal locations
        - data/processed/mean_stepps_soil_clim.RData: Formatted mean STEPPS relative abundances with climate and soil reconstructions
    - Outputs:
        - data/processed/post_stepps_soil_clim.RData: Posterior estimates from STEPPS with co-located climate and soil covariates. Used in 4.3.run_gjam.R, 4.6.oos_prediction_draws.R, 4.7.plot_oos_prediction_draws.R, and 4.8.run_gjam_300YBP.R
- **4.3.run_gjam.R**: Running GJAM for STEPPS posterior draws. Using outcome of variable selection from mean relative abundances from STEPPS. Looping through each posterior draw: 1) formatting xdata, 2) formatting ydata, 3) running GJAM, 4) saving output
    - Inputs:
        - data/processed/post_stepps_soil_clim.RData: dataframe with co-located reconstructions of 12 taxa's relative abundances, soil variables, and climate variables
    - Outputs:
        - /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData: list of length 100 for 100 posterior draws of STEPPS. Each component of the list is the output of fitting GJAM to the relative abundance posterior draw and the soil sand content, average annual temperature, total annual preicpitation, and precipitation seasonality covariates. Used in 4.4.process_out_gjam_draws.R and 4.6.oos_prediction_draws.R
        - /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_silt_aat_tpr_prsd.RData: list of length 100 for 100 posterior draws of STEPPS. Each component of the list is the output of fitting GJAM to the relative abundance posterior draw and the soil silt content, average annual temperature, total annual precipitation, and precipitation seasonality covariates. Used in 4.5.process_out_gjam_draws.R and 4.6.oos_prediction_draws.R
        - /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_sand_aat_tsd_prsd.RData: list of length 100 for 100 posterior draws of STEPPS. Each component of the list is the output of fitting GJAM to the relative abundance posterior draw and the soil sand content, average annual temperature, temperature seasonality, and precipitation seasonality covariates. Used in 4.5.process_out_gjam_draws.R and 4.6.oos_prediction_draws.R
        - /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_silt_aat_tsd_prsd.RData: list of length 100 for 100 posterior draws of STEPPS. Each component of the list is the output of fitting GJAM to the relative abundance posterior draw and the soil silt content, average annual temperature, temperature seasonality, precipitation seasonality covariates. Used in 4.5.process_out_gjam_draws.R and 4.6.oos_prediction_draws.R
- **4.4.process_out_gjam_draws.R**: Processing output from GJAM model fit with STEPPS relative abundance posterior draws. This step must be run on a VM because of memory constraints. Requires ~ 40 GB RAM. NOTE that this step requires you to change the file path according to what machine you're working on. This is because on a local machine, the files must be saved to an external drive because of storage constraints, but on the VM, the files cannot be on an external drive and must be saved somewhere accessible. On local machine, I saved the files at /Volumes/FileBackup/GJAM_STEPPS_output/. On VM, I saved/loaded the files at out/posteriors/. You must change the file path according to what type of machine you're working on
    - Inputs:
        - out/posteriors/silt_aat_tpr_prsd/GJAM_STEPPS_post_silt_aat_tpr_prsd.RData: list of length 100 for 100 posterior draws of STEPPS. Each component of the list is the output of fitting GJAM to the relative abundance posterior draw and the soil silt content, average annual temperature, total annual precipitation, and precipitation seasonality covariates
        - out/posteriors/sand_aat_tpr_prsd/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData: list of length 100 for 100 posterior draws of STEPPS. Each component of the list is the output of fitting GJAM to the relative abundance posterior draw and the soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates
        - out/posteriors/silt_aat_tsd_prsd/GJAM_STEPPS_post_silt_aat_tsd_prsd.RData: list of length 100 for 100 posterior draws of STEPPS. Each component of the list is the output of fitting GJAM to the relative abundance posterior draw and the soil silt content, average annual temperature, temperature seasonality, and precipitation seasonality covariates
        - out/posteriors/sand_aat_tsd_prsd/GJAM_STEPPS_post_sand_aat_tsd_prsd.RData: list of length 100 for 100 posterior draws of STEPPS. Each component of the list is the output of fitting GJAM to the relative abundance posterior draw and the soil sand content, average annual temperature, temperature seasonality, and precipitation seasonality covariates
    - Outputs:
        - out/posteriors/silt_aat_tpr_prsd/: Directory with processed MCMC chains and summaries for plotting made from output of 100 runs of GJAM with soil silt content, average annual temperature, total annual precipitation, and precipitation seasonality covariates
            - bFacGibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X and W (covariance). Used in 4.5.gjam_draws_figures.R
            - bgibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X. Used in 4.5.gjam_draws_figures.R
            - bgibbsUn.RDS: dataframe of full MCMC chains for unstandardized beta coefficients. Used in 4.5.gjam_draws_figures.R
            - fSensGibbs.RDS: dataframe of full MCMC chains for joint sensitivity of response variables to the independent variables. Used in 4.5.gjam_draws_figures.R
            - parameter_summaries.RData: 5 dataframes, one for each parameter type, with summary statistics over full chains. Currently not used but could be used for plotting
            - sgibbs.RDS: dataframe of full MCMC chains for response variable covariance. Used in 4.5.gjam_draws_figures.R
        - out/posteriors/sand_aat_tpr_prsd/: Directory with processed MCMC chains and summaries for plotting made from output of 100 runs of GJAM with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates
            - bFacGibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X and W (covariance). Used in 4.5.gjam_draws_figures.R
            - bgibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X. Used in 4.5.gjam_draws_figures.R
            - bgibbsUn.RDS: dataframe of full MCMC chains for unstandardized beta coefficients. Used in 4.5.gjam_draws_figures.R
            - fSensGibbs.RDS: dataframe of full MCMC chains for joint sensitivity of response variables to the independent variables. Used in 4.5.gjam_draws_figures.R
            - parameter_summaries.RData: 5 dataframes, one for each parameter type, with summary statistics over full chains. Currently not used but could be used for plotting
            - sgibbs.RDS: dataframe of full MCMC chains for response variable covariance. Used in 4.5.gjam_draws_figures.R
        - out/posteriors/silt_aat_tsd_prsd/: Directory with processed MCMC chains and summaries for plotting made from output of 100 runs of GJAM with soil silt content, average annual temperature, temperature seasonality, and precipitation seasonality covariates
            - bFacGibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X and W (covariance). Used in 4.5.gjam_draws_figures.R
            - bgibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X. Used in 4.5.gjam_draws_figures.R
            - bgibbsUn.RDS: dataframe of full MCMC chains for unstandardized beta coefficients. Used in 4.5.gjam_draws_figures.R
            - fSensGibbs.RDS: dataframe of full MCMC chains for joint sensitivity of response variables to the independent variables. Used in 4.5.gjam_draws_figures.R
            - parameter_summaries.RData: 5 dataframes, one for each parameter type, with summary statistics over full chains. Currently not used but could be used for plotting
            - sgibbs.RDS: dataframe of full MCMC chains for response variable covariance. Used in 4.5.gjam_draws_figures.R
        - out/posteriors/sand_aat_tsd_prsd/: Directory with processed MCMC chains and summaries for plotting made from output of 100 runs of GJAM with soil sand content, average annual temperature, temperature seasonality, and precipitation seasonality covariates
            - bFacGibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X and W (covariance). Used in 4.5.gjam_draws_figures.R
            - bgibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X. Used in 4.5.gjam_draws_figures.R
            - bgibbsUn.RDS: dataframe of full MCMC chains for unstandardized beta coefficients. Used in 4.5.gjam_draws_figures.R
            - fSensGibbs.RDS: dataframe of full MCMC chains for joint sensitivity of response variables to the independent variables. Used in 4.5.gjam_draws_figures.R
            - parameter_summaries.RData: 5 dataframes, one for each parameter type, with summary statistics over full chains. Currently not used but could be used for plotting
            - sgibbs.RDS: dataframe of full MCMC chains for response variable covariance. Used in 4.5.gjam_draws_figures.R
- **4.5.gjam_draws_figures.R**: Plotting posterior parameter estimates from STEPPS posterior samples. 1) Trace plots, 2) environment-vegetation relationships, 3) vegetation vegetation correlations
    - Inputs:
        - out/posteriors/silt_aat_tpr_prsd/: Directory with processed MCMC chains and summaries for plotting made from output of 100 runs of GJAM with soil silt content, average annual temperature, total annual precipitation, and precipitation seasonality covariates
            - bFacGibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X and W (covariance)
            - bgibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X
            - bgibbsUn.RDS: dataframe of full MCMC chains for unstandardized beta coefficients
            - fSensGibbs.RDS: dataframe of full MCMC chains for joint sensitivity of response variables to the independent variables
            - sgibbs.RDS: dataframe of full MCMC chains for response variable covariance
        - out/posteriors/sand_aat_tpr_prsd/: Directory with processed MCMC chains and summaries for plotting made from output of 100 runs of GJAM with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates
            - bFacGibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X and W (covariance)
            - bgibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X
            - bgibbsUn.RDS: dataframe of full MCMC chains for unstandardized beta coefficients
            - fSensGibbs.RDS: dataframe of full MCMC chains for joint sensitivity of response variables to the independent variables
            - sgibbs.RDS: dataframe of full MCMC chains for response variable covariance
        - out/posteriors/silt_aat_tsd_prsd/: Directory with processed MCMC chains and summaries for plotting made from output of 100 runs of GJAM with soil silt content, average annual temperature, temperature seasonality, and precipitation sesasonality covariates
            - bFacGibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X and W (covariance)
            - bgibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X
            - bgibbsUn.RDS: dataframe of full MCMC chains for unstandardized beta coefficients
            - fSensGibbs.RDS: dataframe of full MCMC chains for joint sensitivity of response variables to the independent variables
            - sgibbs.RDS: dataframe of full MCMC chains for response variable covariance
        - out/posteriors/sand_aat_tsd_prsd/: Directory with processed MCMC chains and summaries for plotting made from output of 100 runs of GJAM with soil sand content, average annual temperature, temperature seasonality, and precipitation seasonality covariates
            - bFacGibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X and W (covariance)
            - bgibbs.RDS: dataframe of full MCMC chains for beta coefficients standardized for X
            - bgibbsUn.RDS: dataframe of full MCMC chains for unstandardized beta coefficients
            - fSensGibbs.RDS: dataframe of full MCMC chains for joint sensitivity of response variables to the independent variables
            - sgibbs.RDS: dataframe of full MCMC chains for response variable covariance
    - Outputs: none
- **4.6.oos_prediction_draws.R**: Out-of-sample prediction in time (using last time step, 300 YBP). Using STEPPS posterior draws. This step must be run on VM because of memory constraints. Requires ~ 30 GB RAM. NOTE that this step requires you to change the file path according to the machine you're working on. This is because on a local machine, the files must be saved to an externa ldrive because of storage constraints, but on the VM, the files cannot be on an external drive and must be saved somwehere accessible. On local machine, I saved the files at /Volumes/FileBackup/GJAM_STEPPS_output/. On VM, I saved/loaded the files at out/posteriors/. You must change the file path according to what type of machine you're working on
    - Inputs:
        - out/posteriors/sand_aat_tpr_prsd/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData: output from 100 runs of GJAM with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariateswith posterior draws
        - data/processed/post_stepps_soil_clim.RData: dataframes with STEPPS posterior draws and climate and soil reconstructions, including out-of-sample dataframe with data for 300 YBP
    - Outputs:
        - out/posteriors/oos_prediction_nonconditional_time.RData: Out-of-sample predictions for the 300 YBP time step using the non-conditional prediction method. Output is in list of length 100 with each element being one prediction output from GJAM. Used in 4.7.plot_oos_prediction_draws.R
        - out/posteriors/oos_prediction_conditionaloak_time.RData: Out-of-sample prediction for the 300 YBP time step using the prediction method conditional only on oak. Output is in list of length 100 with each element being one prediction output from GJAM. Used in 4.7.plot_oos_prediction_draws.R
- **4.7.plot_oos_prediction_draws.R**: Plotting out-of-sample predictions in time. Out-of-sample prediction of 300 YBP for same spatial locations as used to fit the model
    - Inputs:
        - data/processed/post_stepps_soil_clim.RData: dataframes with STEPPS posterior draws and climate and soil reconstructions, including out-of-sample dataframe with data for 300 YBP
        - /Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_nonconditional_time.RData: Out-of-sample prediction for the 300 YBP time step using the non-conditional prediction method. Output is in list of length 100 with each element being one prediction output from GJAM. Saved on external drive because of large file size (8 GB)
        - /Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_conditionaloak_time.RData: Out-of-sample prediction for the 300 YBP time step using the prediction method conditional only on oak. Output is in list of length 100 with each element being one prediction output from GJAM. Saved on external drive because of large file size (8 GB)
    - Output: none
- **4.8.run_gjam_300YBP.R**: Running GJAM for STEPPS posterior draws including 300 YBP time step. Using outcome of variable selection from mean relative abundances from STEPPS. First combining original in-sample and out-of-sample datasets because we already performed out-of-sample validation. Then running GJAM. Looping through each posterior draw: 1) formatting xdata, 2) formatting ydata, 3) running GJAM, 4) saving output
    - Inputs:
        - data/processed/post_stepps_soil_clim.RData: 2 dataframes, original in-sample and out-of-sample data, with colocated reconstructions of 12 taxa's relative abundances, soil variables and climate variables. Combined here
    - Outputs:
        - /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP.RData: Output from fitting GJAM with 300 YBP data. Output is in a list of length 100 (for 100 posterior draws) with each element being one output from one run of GJAM. Used in 4.9.process_out_gjam_draws_300YBP.R and 4.12.oos_prediction_draws_final.R
- **4.9.process_out_gjam_draws_300YBP.R**: Processing output from GJAM model fit with STEPPS relative abundance posterior draws including 300 YBP time step. Identical to step 4-4 but including 300 YBP. Also only including the model with soil sand content, average annual temperature, total annual precipitation, and precipitation seasonality covariates because this was determined to be the most ecologically relevant model in steps 3-3 to 3-7 and 4-3 to 4-5. This step must be run on a VM because of memory constraints. Requires ~ 40 GB RAM. NOTE that this step requies you to change the file path according to what machine you're working on. This is because on a local machine, the files must be saved to an external drive because of storage constraints, but on the VM, the files cannot be on an external drive and must be saved somewhere accessible. On local machine, I saved the files at /Volumes/FileBackup/GJAM_STEPPS_output/. On VM, I saved/loaded the files at out/posteriors/. You must change the file path according to what type of machine you're working on
    - Inputs:
        - out/posteriors/sand_aat_tpr_prsd_300YBP/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP.RData: output from fitting GJAM with 300 YBP data. Output is in a list of length 100 (for 100 posterior draws) with each element being one output from one run of GJAM
    - Outputs:
        - out/posteriors/sand_aat_tpr_prsd_300YBP/: directory with processed outputs from running GJAM with 300 YBP data
            - bFacGibbs.RDS: dataframe with full MCMC chains for beta coefficients standardized for X and W (covariance). Used in 4.10.plot_stepps_draws_300YBP.R
            - bgibbs.RDS: dataframe with full MCMC chains for beta coefficients standardized for X. Used in 4.10.plot_stepps_draws_300YBP.R
            - bgibbsUn.RDS: dataframe with full MCMC chains for unstandardized beta coefficients. Used in 4.10.plot_stepps_draws_300YBP.R
            - fSensGibbs.RDS: dataframe with full MCMC chains for joint sensitivity of response variables to each indpendent variable. Used in 4.10.plot_stepps_draws_300YBP.R
            - parameter_summaries.RData: 5 dataframes, one for each parameter type, with summary statistics over full chains. Currently not used but could be used for plotting
            - sgibbs.RDS: dataframe with full MCMC chains for response variable covariance. Used in 4.10.plot_stepps_draws_300YBP.R
- **4.10.plot_stepps_draws_300YBP.R**: Plotting posterior parameter estimates from STEPPS posterior samples with 300 YBP used to fit the model. Identical to step 4-5 but using 300 YBP time step. 1) trace plots, 2) environment-vegetation relationships, 3) vegetation-vegetation correlations
    - Inputs:
        - out/posteriors/sand_aat_tpr_prsd_300YBP/: directory with processed outputs from running GJAM with 300 YBP data
            - bFacGibbs.RDS: dataframe with full MCMC chains for beta coefficients standardized for X and W (covariance)
            - bgibbs.RDS: dataframe with full MCMC chains for beta coefficients standardized for X
            - bgibbsUn.RDS: dataframe with full MCMC chains for unstandardized beta coefficients
            - fSensGibbs.RDS: dataframe with full MCMC chains for joint sensitivity of response variables to each indpendent variable
            - sgibbs.RDS: dataframe with full MCMC chains for response variable covariance
    - Outputs: none
- **4.11.format_final_oos_draws.R**: Formatting final out-of-sample data with STEPPS posterior draws. Taking full spatiotemporal extent of STEPPS posterior draws, soil, and climate reconstructions. Removing in-sample data and saving the out-of-sample prediction domain
    - Inputs:
        - data/processed/post_STEPPS.RData: posterior draws for entire spatiotemporal domain from STEPPS
        - data/intermediate/stepps_post_subsampled.RData: posterior draws of relative abundances, but for a subset of spatiotemporal locations
        - data/processed/mean_stepps_full_oos.RData: Dataframe with climate and soil data for full spatiotemporal domain from doing the same procedure with the mean relative abundances
    - Outputs:
        - data/processed/post_stepps_full_oos.RData: Dataframe with posterior draws from STEPPS for all out-of-sample spatiotemporal locations, as well as corresponding climate and soil reconstructions. Used in 4.12.oos_prediction_draws_final.R and 4.13.plot_oos_prediction_draws_final.R
- **4.12.oos_prediction_draws_final.R**: Out-of-sample prediction in space and time (using all spatiotemporal locations not used to fit the model). This step must be run on VM because of memory constraints. Requires ~ 50 GB RAM. NOTE that this step requires you to change the file path according to what machine you're working on. This is because on a local machine, the files must be saved to an external drive because of storage constraints, but on the VM, the files cannot be on an external drive and must be saved somwhere accessible. On local machine, I saved the files at /Volumes/FileBackup/GJAM_STEPPS_output/. On VM, I saved/loaded the files at out/posteriors/. You must change the file path according to what type of machine you're working on
    - Inputs:
        - out/posteriors/sand_aat_tpr_prsd_300YBP/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP.RData: list of length 100 with 100 outputs of GJAM fitted to each relative abundance posterior draw and the soil and content, average annual temperature, total annual precipitation, and precipitation seasonality covariates and including the data from 300 YBP
        - data/processed/post_stepps_full_oos.RData: out-of-sample data for everything not used to fit the model
    - Outputs:
        - out/posteriors/oos_prediction_nonconditional_final.RData: list of length 100 of prediction outputs using the nonconditional prediction method. Used in 4.13.plot_oos_prediction_draws_final.R
        - out/posteriors/oos_prediction_conditionaloak_final.RData: list of length 100 of prediction outputs using the prediction method conditional only on oak. Used in 4.13.plot_oos_prediction_draws_final.R
- **4.13.plot_oos_prediction_draws_final.R**

### Other Code

- **climate_dimensions.R**: helper file with the maximum extent of climate domain for use in 2.1.process_climate.R
- **funs.R**: helper functions for formatting STEPPS arrays (melt_array) and mapping study region (map_states)
- **deprecated**: contains a small number of deprecated scripts that are ultimately not used
- **Testing_subsampling_methods**: contains three Rmd (and three corresponding pdf) files detailing how the sampling density and specific sampling scheme were chosen in space and time
    - **Semivariogram_spatial_subsampling.Rmd**: fitting semivariograms to the mean relative abundance reconstructions to justify the spatial sampling density (every three grid cells)
    - **Testing_spatial_subsampling.Rmd**: using exploratory analyses to determine optimal sampling density (along with semivariogram analyses) and to determine the best subset of grid cells to choose
    - **Testing_temporal_subsampling.Rmd**: fitting autocorrelation functions to justify the temporal sampling density (every four time steps)