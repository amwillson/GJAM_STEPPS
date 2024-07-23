### STEP 2-1

## Formatting downscaled climate reconstructions
## Going from .mat data structure to arrays

## Input: each .mat file from /Volumes/FileBackup/Climate_Downscaling_RG/
## .mat files are stored separately because of large data volume
## Matlab file structure with one climate variable for one month in each file
## Files are privately archived and will be made publicly available upon publication of the data product

## Output: each .RData file from /Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/
## .RData files are stored separately because of large data volume
## Each .RData file contains 2 arrays, one with downscaled estimate and one with bias
## Used in 2.2.climate_temporal_summaries.R

rm(list = ls())

#### Format temperature files ####

### January ###

# Reduced dimensions
source('R/climate_dimensions.R')

# Load monthly temperature
tas1 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=1_alyssa.mat')
# Array for estimate
tas1_ey.hat <- tas1$EY.hat
# Array for bias
tas1_e <- tas1$e
# Vector for longitude
tas1_lon <- tas1$lon
# Vector for latitude
tas1_lat <- tas1$lat
# Vector for time
tas1_time <- seq(from = 0, to = 2014, by = 1)

# Dimensions to keep
keep_lon <- which(tas1_lon >= min_lon & tas1_lon <= max_lon)
keep_lat <- which(tas1_lat >= min_lat & tas1_lat <= max_lat)
keep_time <- which(tas1_time >= min_time & tas1_time <= max_time)

# Subset
tas1_ey.hat <- tas1_ey.hat[keep_lon, keep_lat, keep_time]
tas1_e <- tas1_e[keep_lon, keep_lat]
tas1_lon <- tas1_lon[keep_lon]
tas1_lat <- tas1_lat[keep_lat]
tas1_time <- tas1_time[keep_time]

# Apply dimension names
dimnames(tas1_ey.hat) <- list(tas1_lon,
                              tas1_lat,
                              tas1_time)
dimnames(tas1_e) <- list(tas1_lon,
                         tas1_lat)

# Save
save(tas1_ey.hat, tas1_e, 
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas1.RData')

### February ###

rm(list = ls())

source('R/climate_dimensions.R')

tas2 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=2_alyssa.mat')
tas2_ey.hat <- tas2$EY.hat
tas2_e <- tas2$e
tas2_lon <- tas2$lon
tas2_lat <- tas2$lat
tas2_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas2_lon >= min_lon & tas2_lon <= max_lon)
keep_lat <- which(tas2_lat >= min_lat & tas2_lat <= max_lat)
keep_time <- which(tas2_time >= min_time & tas2_time <= max_time)

tas2_ey.hat <- tas2_ey.hat[keep_lon, keep_lat, keep_time]
tas2_e <- tas2_e[keep_lon, keep_lat]
tas2_lon <- tas2_lon[keep_lon]
tas2_lat <- tas2_lat[keep_lat]
tas2_time <- tas2_time[keep_time]

dimnames(tas2_ey.hat) <- list(tas2_lon,
                              tas2_lat,
                              tas2_time)
dimnames(tas2_e) <- list(tas2_lon,
                         tas2_lat)

save(tas2_ey.hat, tas2_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas2.RData')

### March ###

rm(list = ls())

source('R/climate_dimensions.R')

tas3 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=3_alyssa.mat')
tas3_ey.hat <- tas3$EY.hat
tas3_e <- tas3$e
tas3_lon <- tas3$lon
tas3_lat <- tas3$lat
tas3_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas3_lon >= min_lon & tas3_lon <= max_lon)
keep_lat <- which(tas3_lat >= min_lat & tas3_lat <= max_lat)
keep_time <- which(tas3_time >= min_time & tas3_time <= max_time)

tas3_ey.hat <- tas3_ey.hat[keep_lon, keep_lat, keep_time]
tas3_e <- tas3_e[keep_lon, keep_lat]
tas3_lon <- tas3_lon[keep_lon]
tas3_lat <- tas3_lat[keep_lat]
tas3_time <- tas3_time[keep_time]

dimnames(tas3_ey.hat) <- list(tas3_lon,
                              tas3_lat,
                              tas3_time)
dimnames(tas3_e) <- list(tas3_lon,
                         tas3_lat)

save(tas3_ey.hat, tas3_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas3.RData')

### April ###

rm(list = ls())

source('R/climate_dimensions.R')

tas4 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=4_alyssa.mat')
tas4_ey.hat <- tas4$EY.hat
tas4_e <- tas4$e
tas4_lon <- tas4$lon
tas4_lat <- tas4$lat
tas4_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas4_lon >= min_lon & tas4_lon <= max_lon)
keep_lat <- which(tas4_lat >= min_lat & tas4_lat <= max_lat)
keep_time <- which(tas4_time >= min_time & tas4_time <= max_time)

tas4_ey.hat <- tas4_ey.hat[keep_lon, keep_lat, keep_time]
tas4_e <- tas4_e[keep_lon, keep_lat]
tas4_lon <- tas4_lon[keep_lon]
tas4_lat <- tas4_lat[keep_lat]
tas4_time <- tas4_time[keep_time]

dimnames(tas4_ey.hat) <- list(tas4_lon,
                              tas4_lat,
                              tas4_time)
dimnames(tas4_e) <- list(tas4_lon,
                         tas4_lat)

save(tas4_ey.hat, tas4_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas4.RData')

### May ###

rm(list = ls())

source('R/climate_dimensions.R')

tas5 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=5_alyssa.mat')
tas5_ey.hat <- tas5$EY.hat
tas5_e <- tas5$e
tas5_lon <- tas5$lon
tas5_lat <- tas5$lat
tas5_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas5_lon >= min_lon & tas5_lon <= max_lon)
keep_lat <- which(tas5_lat >= min_lat & tas5_lat <= max_lat)
keep_time <- which(tas5_time >= min_time & tas5_time <= max_time)

tas5_ey.hat <- tas5_ey.hat[keep_lon, keep_lat, keep_time]
tas5_e <- tas5_e[keep_lon, keep_lat]
tas5_lon <- tas5_lon[keep_lon]
tas5_lat <- tas5_lat[keep_lat]
tas5_time <- tas5_time[keep_time]

dimnames(tas5_ey.hat) <- list(tas5_lon,
                              tas5_lat,
                              tas5_time)
dimnames(tas5_e) <- list(tas5_lon,
                         tas5_lat)

save(tas5_ey.hat, tas5_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas5.RData')

### June ###

rm(list = ls())

source('R/climate_dimensions.R')

tas6 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=6_alyssa.mat')
tas6_ey.hat <- tas6$EY.hat
tas6_e <- tas6$e
tas6_lon <- tas6$lon
tas6_lat <- tas6$lat
tas6_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas6_lon >= min_lon & tas6_lon <= max_lon)
keep_lat <- which(tas6_lat >= min_lat & tas6_lat <= max_lat)
keep_time <- which(tas6_time >= min_time & tas6_time <= max_time)

tas6_ey.hat <- tas6_ey.hat[keep_lon, keep_lat, keep_time]
tas6_e <- tas6_e[keep_lon, keep_lat]
tas6_lon <- tas6_lon[keep_lon]
tas6_lat <- tas6_lat[keep_lat]
tas6_time <- tas6_time[keep_time]

dimnames(tas6_ey.hat) <- list(tas6_lon,
                              tas6_lat,
                              tas6_time)
dimnames(tas6_e) <- list(tas6_lon,
                         tas6_lat)

save(tas6_ey.hat, tas6_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas6.RData')

### July ###

rm(list = ls())

source('R/climate_dimensions.R')

tas7 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=7_alyssa.mat')
tas7_ey.hat <- tas7$EY.hat
tas7_e <- tas7$e
tas7_lon <- tas7$lon
tas7_lat <- tas7$lat
tas7_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas7_lon >= min_lon & tas7_lon <= max_lon)
keep_lat <- which(tas7_lat >= min_lat & tas7_lat <= max_lat)
keep_time <- which(tas7_time >= min_time & tas7_time <= max_time)

tas7_ey.hat <- tas7_ey.hat[keep_lon, keep_lat, keep_time]
tas7_e <- tas7_e[keep_lon, keep_lat]
tas7_lon <- tas7_lon[keep_lon]
tas7_lat <- tas7_lat[keep_lat]
tas7_time <- tas7_time[keep_time]

dimnames(tas7_ey.hat) <- list(tas7_lon,
                              tas7_lat,
                              tas7_time)
dimnames(tas7_e) <- list(tas7_lon,
                         tas7_lat)

save(tas7_ey.hat, tas7_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas7.RData')

### August ###

rm(list = ls())

source('R/climate_dimensions.R')

tas8 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=8_alyssa.mat')
tas8_ey.hat <- tas8$EY.hat
tas8_e <- tas8$e
tas8_lon <- tas8$lon
tas8_lat <- tas8$lat
tas8_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas8_lon >= min_lon & tas8_lon <= max_lon)
keep_lat <- which(tas8_lat >= min_lat & tas8_lat <= max_lat)
keep_time <- which(tas8_time >= min_time & tas8_time <= max_time)

tas8_ey.hat <- tas8_ey.hat[keep_lon, keep_lat, keep_time]
tas8_e <- tas8_e[keep_lon, keep_lat]
tas8_lon <- tas8_lon[keep_lon]
tas8_lat <- tas8_lat[keep_lat]
tas8_time <- tas8_time[keep_time]

dimnames(tas8_ey.hat) <- list(tas8_lon,
                              tas8_lat,
                              tas8_time)
dimnames(tas8_e) <- list(tas8_lon,
                         tas8_lat)

save(tas8_ey.hat, tas8_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas8.RData')

### September ###

rm(list = ls())

source('R/climate_dimensions.R')

tas9 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=9_alyssa.mat')
tas9_ey.hat <- tas9$EY.hat
tas9_e <- tas9$e
tas9_lon <- tas9$lon
tas9_lat <- tas9$lat
tas9_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas9_lon >= min_lon & tas9_lon <= max_lon)
keep_lat <- which(tas9_lat >= min_lat & tas9_lat <= max_lat)
keep_time <- which(tas9_time >= min_time & tas9_time <= max_time)

tas9_ey.hat <- tas9_ey.hat[keep_lon, keep_lat, keep_time]
tas9_e <- tas9_e[keep_lon, keep_lat]
tas9_lon <- tas9_lon[keep_lon]
tas9_lat <- tas9_lat[keep_lat]
tas9_time <- tas9_time[keep_time]

dimnames(tas9_ey.hat) <- list(tas9_lon,
                              tas9_lat,
                              tas9_time)
dimnames(tas9_e) <- list(tas9_lon,
                         tas9_lat)

save(tas9_ey.hat, tas9_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas9.RData')

### October ###

rm(list = ls())

source('R/climate_dimensions.R')

tas10 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=10_alyssa.mat')
tas10_ey.hat <- tas10$EY.hat
tas10_e <- tas10$e
tas10_lon <- tas10$lon
tas10_lat <- tas10$lat
tas10_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas10_lon >= min_lon & tas10_lon <= max_lon)
keep_lat <- which(tas10_lat >= min_lat & tas10_lat <= max_lat)
keep_time <- which(tas10_time >= min_time & tas10_time <= max_time)

tas10_ey.hat <- tas10_ey.hat[keep_lon, keep_lat, keep_time]
tas10_e <- tas10_e[keep_lon, keep_lat]
tas10_lon <- tas10_lon[keep_lon]
tas10_lat <- tas10_lat[keep_lat]
tas10_time <- tas10_time[keep_time]

dimnames(tas10_ey.hat) <- list(tas10_lon,
                               tas10_lat,
                               tas10_time)
dimnames(tas10_e) <- list(tas10_lon,
                          tas10_lat)

save(tas10_ey.hat, tas10_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas10.RData')

### November ###

rm(list = ls())

source('R/climate_dimensions.R')

tas11 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=11_alyssa.mat')
tas11_ey.hat <- tas11$EY.hat
tas11_e <- tas11$e
tas11_lon <- tas11$lon
tas11_lat <- tas11$lat
tas11_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas11_lon >= min_lon & tas11_lon <= max_lon)
keep_lat <- which(tas11_lat >= min_lat & tas11_lat <= max_lat)
keep_time <- which(tas11_time >= min_time & tas11_time <= max_time)

tas11_ey.hat <- tas11_ey.hat[keep_lon, keep_lat, keep_time]
tas11_e <- tas11_e[keep_lon, keep_lat]
tas11_lon <- tas11_lon[keep_lon]
tas11_lat <- tas11_lat[keep_lat]
tas11_time <- tas11_time[keep_time]

dimnames(tas11_ey.hat) <- list(tas11_lon,
                               tas11_lat,
                               tas11_time)
dimnames(tas11_e) <- list(tas11_lon,
                          tas11_lat)

save(tas11_ey.hat, tas11_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas11.RData')

### December ###

rm(list = ls())

source('R/climate_dimensions.R')

tas12 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/tas_PCRdownscaled_mth=12_alyssa.mat')
tas12_ey.hat <- tas12$EY.hat
tas12_e <- tas12$e
tas12_lon <- tas12$lon
tas12_lat <- tas12$lat
tas12_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(tas12_lon >= min_lon & tas12_lon <= max_lon)
keep_lat <- which(tas12_lat >= min_lat & tas12_lat <= max_lat)
keep_time <- which(tas12_time >= min_time & tas12_time <= max_time)

tas12_ey.hat <- tas12_ey.hat[keep_lon, keep_lat, keep_time]
tas12_e <- tas12_e[keep_lon, keep_lat]
tas12_lon <- tas12_lon[keep_lon]
tas12_lat <- tas12_lat[keep_lat]
tas12_time <- tas12_time[keep_time]

dimnames(tas12_ey.hat) <- list(tas12_lon,
                               tas12_lat,
                               tas12_time)
dimnames(tas12_e) <- list(tas12_lon,
                          tas12_lat)

save(tas12_ey.hat, tas12_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas12.RData')

#### Format precipitation files ####

### January ###

rm(list = ls())

source('R/climate_dimensions.R')

# Load monthly precipitation
pr1 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=1_alyssa.mat')
pr1_ey.hat <- pr1$EY.hat
pr1_e <- pr1$e
pr1_lon <- pr1$lon
pr1_lat <- pr1$lat
pr1_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr1_lon >= min_lon & pr1_lon <= max_lon)
keep_lat <- which(pr1_lat >= min_lat & pr1_lat <= max_lat)
keep_time <- which(pr1_time >= min_time & pr1_time <= max_time)

pr1_ey.hat <- pr1_ey.hat[keep_lon, keep_lat, keep_time]
pr1_e <- pr1_e[keep_lon, keep_lat]
pr1_lon <- pr1_lon[keep_lon]
pr1_lat <- pr1_lat[keep_lat]
pr1_time <- pr1_time[keep_time]

dimnames(pr1_ey.hat) <- list(pr1_lon,
                             pr1_lat,
                             pr1_time)
dimnames(pr1_e) <- list(pr1_lon,
                        pr1_lat)

save(pr1_ey.hat, pr1_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr1.RData')

### February ###

rm(list = ls())

source('R/climate_dimensions.R')

pr2 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=2_alyssa.mat')
pr2_ey.hat <- pr2$EY.hat
pr2_e <- pr2$e
pr2_lon <- pr2$lon
pr2_lat <- pr2$lat
pr2_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr2_lon >= min_lon & pr2_lon <= max_lon)
keep_lat <- which(pr2_lat >= min_lat & pr2_lat <= max_lat)
keep_time <- which(pr2_time >= min_time & pr2_time <= max_time)

pr2_ey.hat <- pr2_ey.hat[keep_lon, keep_lat, keep_time]
pr2_e <- pr2_e[keep_lon, keep_lat]
pr2_lon <- pr2_lon[keep_lon]
pr2_lat <- pr2_lat[keep_lat]
pr2_time <- pr2_time[keep_time]

dimnames(pr2_ey.hat) <- list(pr2_lon,
                             pr2_lat,
                             pr2_time)
dimnames(pr2_e) <- list(pr2_lon,
                        pr2_lat)

save(pr2_ey.hat, pr2_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr2.RData')

### March ###

rm(list = ls())

source('R/climate_dimensions.R')

pr3 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=3_alyssa.mat')
pr3_ey.hat <- pr3$EY.hat
pr3_e <- pr3$e
pr3_lon <- pr3$lon
pr3_lat <- pr3$lat
pr3_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr3_lon >= min_lon & pr3_lon <= max_lon)
keep_lat <- which(pr3_lat >= min_lat & pr3_lat <= max_lat)
keep_time <- which(pr3_time >= min_time & pr3_time <= max_time)

pr3_ey.hat <- pr3_ey.hat[keep_lon, keep_lat, keep_time]
pr3_e <- pr3_e[keep_lon, keep_lat]
pr3_lon <- pr3_lon[keep_lon]
pr3_lat <- pr3_lat[keep_lat]
pr3_time <- pr3_time[keep_time]

dimnames(pr3_ey.hat) <- list(pr3_lon,
                             pr3_lat,
                             pr3_time)
dimnames(pr3_e) <- list(pr3_lon,
                        pr3_lat)

save(pr3_ey.hat, pr3_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr3.RData')

### April ###

rm(list = ls())

source('R/climate_dimensions.R')

pr4 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=4_alyssa.mat')
pr4_ey.hat <- pr4$EY.hat
pr4_e <- pr4$e
pr4_lon <- pr4$lon
pr4_lat <- pr4$lat
pr4_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr4_lon >= min_lon & pr4_lon <= max_lon)
keep_lat <- which(pr4_lat >= min_lat & pr4_lat <= max_lat)
keep_time <- which(pr4_time >= min_time & pr4_time <= max_time)

pr4_ey.hat <- pr4_ey.hat[keep_lon, keep_lat, keep_time]
pr4_e <- pr4_e[keep_lon, keep_lat]
pr4_lon <- pr4_lon[keep_lon]
pr4_lat <- pr4_lat[keep_lat]
pr4_time <- pr4_time[keep_time]

dimnames(pr4_ey.hat) <- list(pr4_lon,
                             pr4_lat,
                             pr4_time)
dimnames(pr4_e) <- list(pr4_lon,
                        pr4_lat)

save(pr4_ey.hat, pr4_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr4.RData')

### May ###

rm(list = ls())

source('R/climate_dimensions.R')

pr5 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=5_alyssa.mat')
pr5_ey.hat <- pr5$EY.hat
pr5_e <- pr5$e
pr5_lon <- pr5$lon
pr5_lat <- pr5$lat
pr5_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr5_lon >= min_lon & pr5_lon <= max_lon)
keep_lat <- which(pr5_lat >= min_lat & pr5_lat <= max_lat)
keep_time <- which(pr5_time >= min_time & pr5_time <= max_time)

pr5_ey.hat <- pr5_ey.hat[keep_lon, keep_lat, keep_time]
pr5_e <- pr5_e[keep_lon, keep_lat]
pr5_lon <- pr5_lon[keep_lon]
pr5_lat <- pr5_lat[keep_lat]
pr5_time <- pr5_time[keep_time]

dimnames(pr5_ey.hat) <- list(pr5_lon,
                             pr5_lat,
                             pr5_time)
dimnames(pr5_e) <- list(pr5_lon,
                        pr5_lat)

save(pr5_ey.hat, pr5_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr5.RData')

### June ###

rm(list = ls())

source('R/climate_dimensions.R')

pr6 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=6_alyssa.mat')
pr6_ey.hat <- pr6$EY.hat
pr6_e <- pr6$e
pr6_lon <- pr6$lon
pr6_lat <- pr6$lat
pr6_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr6_lon >= min_lon & pr6_lon <= max_lon)
keep_lat <- which(pr6_lat >= min_lat & pr6_lat <= max_lat)
keep_time <- which(pr6_time >= min_time & pr6_time <= max_time)

pr6_ey.hat <- pr6_ey.hat[keep_lon, keep_lat, keep_time]
pr6_e <- pr6_e[keep_lon, keep_lat]
pr6_lon <- pr6_lon[keep_lon]
pr6_lat <- pr6_lat[keep_lat]
pr6_time <- pr6_time[keep_time]

dimnames(pr6_ey.hat) <- list(pr6_lon,
                             pr6_lat,
                             pr6_time)
dimnames(pr6_e) <- list(pr6_lon,
                        pr6_lat)

save(pr6_ey.hat, pr6_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr6.RData')

### July ###

rm(list = ls())

source('R/climate_dimensions.R')

pr7 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=7_alyssa.mat')
pr7_ey.hat <- pr7$EY.hat
pr7_e <- pr7$e
pr7_lon <- pr7$lon
pr7_lat <- pr7$lat
pr7_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr7_lon >= min_lon & pr7_lon <= max_lon)
keep_lat <- which(pr7_lat >= min_lat & pr7_lat <= max_lat)
keep_time <- which(pr7_time >= min_time & pr7_time <= max_time)

pr7_ey.hat <- pr7_ey.hat[keep_lon, keep_lat, keep_time]
pr7_e <- pr7_e[keep_lon, keep_lat]
pr7_lon <- pr7_lon[keep_lon]
pr7_lat <- pr7_lat[keep_lat]
pr7_time <- pr7_time[keep_time]

dimnames(pr7_ey.hat) <- list(pr7_lon,
                             pr7_lat,
                             pr7_time)
dimnames(pr7_e) <- list(pr7_lon,
                        pr7_lat)

save(pr7_ey.hat, pr7_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr7.RData')

### August ###

rm(list = ls())

source('R/climate_dimensions.R')

pr8 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=8_alyssa.mat')
pr8_ey.hat <- pr8$EY.hat
pr8_e <- pr8$e
pr8_lon <- pr8$lon
pr8_lat <- pr8$lat
pr8_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr8_lon >= min_lon & pr8_lon <= max_lon)
keep_lat <- which(pr8_lat >= min_lat & pr8_lat <= max_lat)
keep_time <- which(pr8_time >= min_time & pr8_time <= max_time)

pr8_ey.hat <- pr8_ey.hat[keep_lon, keep_lat, keep_time]
pr8_e <- pr8_e[keep_lon, keep_lat]
pr8_lon <- pr8_lon[keep_lon]
pr8_lat <- pr8_lat[keep_lat]
pr8_time <- pr8_time[keep_time]

dimnames(pr8_ey.hat) <- list(pr8_lon,
                             pr8_lat,
                             pr8_time)
dimnames(pr8_e) <- list(pr8_lon,
                        pr8_lat)

save(pr8_ey.hat, pr8_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr8.RData')

### September ###

rm(list = ls())

source('R/climate_dimensions.R')

pr9 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=9_alyssa.mat')
pr9_ey.hat <- pr9$EY.hat
pr9_e <- pr9$e
pr9_lon <- pr9$lon
pr9_lat <- pr9$lat
pr9_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr9_lon >= min_lon & pr9_lon <= max_lon)
keep_lat <- which(pr9_lat >= min_lat & pr9_lat <= max_lat)
keep_time <- which(pr9_time >= min_time & pr9_time <= max_time)

pr9_ey.hat <- pr9_ey.hat[keep_lon, keep_lat, keep_time]
pr9_e <- pr9_e[keep_lon, keep_lat]
pr9_lon <- pr9_lon[keep_lon]
pr9_lat <- pr9_lat[keep_lat]
pr9_time <- pr9_time[keep_time]

dimnames(pr9_ey.hat) <- list(pr9_lon,
                             pr9_lat,
                             pr9_time)
dimnames(pr9_e) <- list(pr9_lon,
                        pr9_lat)

save(pr9_ey.hat, pr9_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr9.RData')

### October ###

rm(list = ls())

source('R/climate_dimensions.R')

pr10 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=10_alyssa.mat')
pr10_ey.hat <- pr10$EY.hat
pr10_e <- pr10$e
pr10_lon <- pr10$lon
pr10_lat <- pr10$lat
pr10_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr10_lon >= min_lon & pr10_lon <= max_lon)
keep_lat <- which(pr10_lat >= min_lat & pr10_lat <= max_lat)
keep_time <- which(pr10_time >= min_time & pr10_time <= max_time)

pr10_ey.hat <- pr10_ey.hat[keep_lon, keep_lat, keep_time]
pr10_e <- pr10_e[keep_lon, keep_lat]
pr10_lon <- pr10_lon[keep_lon]
pr10_lat <- pr10_lat[keep_lat]
pr10_time <- pr10_time[keep_time]

dimnames(pr10_ey.hat) <- list(pr10_lon,
                              pr10_lat,
                              pr10_time)
dimnames(pr10_e) <- list(pr10_lon,
                         pr10_lat)

save(pr10_ey.hat, pr10_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr10.RData')

### November ###

rm(list = ls())

source('R/climate_dimensions.R')

pr11 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=11_alyssa.mat')
pr11_ey.hat <- pr11$EY.hat
pr11_e <- pr11$e
pr11_lon <- pr11$lon
pr11_lat <- pr11$lat
pr11_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr11_lon >= min_lon & pr11_lon <= max_lon)
keep_lat <- which(pr11_lat >= min_lat & pr11_lat <= max_lat)
keep_time <- which(pr11_time >= min_time & pr11_time <= max_time)

pr11_ey.hat <- pr11_ey.hat[keep_lon, keep_lat, keep_time]
pr11_e <- pr11_e[keep_lon, keep_lat]
pr11_lon <- pr11_lon[keep_lon]
pr11_lat <- pr11_lat[keep_lat]
pr11_time <- pr11_time[keep_time]

dimnames(pr11_ey.hat) <- list(pr11_lon,
                              pr11_lat,
                              pr11_time)
dimnames(pr11_e) <- list(pr11_lon,
                         pr11_lat)

save(pr11_ey.hat, pr11_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr11.RData')

### December ###

rm(list = ls())

source('R/climate_dimensions.R')

pr12 <- R.matlab::readMat('/Volumes/FileBackup/Climate_Downscaling_RG/pr_PCRdownscaled_mth=12_alyssa.mat')
pr12_ey.hat <- pr12$EY.hat
pr12_e <- pr12$e
pr12_lon <- pr12$lon
pr12_lat <- pr12$lat
pr12_time <- seq(from = 0, to = 2014, by = 1)

keep_lon <- which(pr12_lon >= min_lon & pr12_lon <= max_lon)
keep_lat <- which(pr12_lat >= min_lat & pr12_lat <= max_lat)
keep_time <- which(pr12_time >= min_time & pr12_time <= max_time)

pr12_ey.hat <- pr12_ey.hat[keep_lon, keep_lat, keep_time]
pr12_e <- pr12_e[keep_lon, keep_lat]
pr12_lon <- pr12_lon[keep_lon]
pr12_lat <- pr12_lat[keep_lat]
pr12_time <- pr12_time[keep_time]

dimnames(pr12_ey.hat) <- list(pr12_lon,
                              pr12_lat,
                              pr12_time)
dimnames(pr12_e) <- list(pr12_lon,
                         pr12_lat)

save(pr12_ey.hat, pr12_e,
     file = '/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr12.RData')
