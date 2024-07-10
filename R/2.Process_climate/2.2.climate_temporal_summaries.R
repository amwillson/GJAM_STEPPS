### STEP 2-2

## Summarize temperature and precipitation downscaled reconstructions over time

## Calculating four climate variables for 50 year windows:
## 1. average annual temperature
##    - for each year, take average of monthly temperature
##    - for all years within window, take average again
## 2. total annual precipitation
##    - for each year, take sum of monthly precipitation
##    - for all years within window, take average
## 3. temperature seasonality
##    - for each year, take standard deviation of monthly temperature
##    - for all years within window, take average
## 4. precipitation seasonality
##    - for each year, take standard deviation of monthly precipitation
##    - for all years within window, take average

## Calculated on both biased and bias-corrected climate estimates

## Input: each .RData file from /Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/
## .RData files are stored separately because of large data volume
## Each .RData file contains 2 arrays, one with downscaled estimate and one with bias

## Output: data/intermediate/tas_201.113.1776.12.RData, data/intermediate/pr_201.113.1776.12.RData
## Intermediate arrays for estimated and de-biased air temperature and total precipitation
## with dimensions of lat x lon x year x month
## Not used, just saved so it does not need to be recomputed

## Output: data/intermediate/mean_average_annual_temperature.RData, data/intermediate/mean_temperature_seasonality.RData,
## data/intermediate/mean_total_annual_precipitation.RData, data/intermediate/mean_precipitation_seasonality.RData
## Intermediate arrays for estimated and de-based average annual temperature, temperature seasonality,
## total annual precipitation, and precipitation seasonality
## calculated by summarizing over months and then 50-year windows
## Used in 2.3.climate_clip_spatial_extent.R

rm(list = ls())

#### Temperature ####

### Loading and processing ###

# Load monthly temperature
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas1.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas2.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas3.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas4.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas5.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas6.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas7.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas8.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas9.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas10.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas11.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/tas12.RData')

# Bias correct
tas1_unbias <- array(, dim = dim(tas1_ey.hat))
tas2_unbias <- array(, dim = dim(tas1_unbias))
tas3_unbias <- array(, dim = dim(tas1_unbias))
tas4_unbias <- array(, dim = dim(tas1_unbias))
tas5_unbias <- array(, dim = dim(tas1_unbias))
tas6_unbias <- array(, dim = dim(tas1_unbias))
tas7_unbias <- array(, dim = dim(tas1_unbias))
tas8_unbias <- array(, dim = dim(tas1_unbias))
tas9_unbias <- array(, dim = dim(tas1_unbias))
tas10_unbias <- array(, dim = dim(tas1_unbias))
tas11_unbias <- array(, dim = dim(tas1_unbias))
tas12_unbias <- array(, dim = dim(tas1_unbias))

# Apply dimension names
dimnames(tas1_unbias) <- dimnames(tas1_ey.hat)
dimnames(tas2_unbias) <- dimnames(tas1_unbias)
dimnames(tas3_unbias) <- dimnames(tas1_unbias)
dimnames(tas4_unbias) <- dimnames(tas1_unbias)
dimnames(tas5_unbias) <- dimnames(tas1_unbias)
dimnames(tas6_unbias) <- dimnames(tas1_unbias)
dimnames(tas7_unbias) <- dimnames(tas1_unbias)
dimnames(tas8_unbias) <- dimnames(tas1_unbias)
dimnames(tas9_unbias) <- dimnames(tas1_unbias)
dimnames(tas10_unbias) <- dimnames(tas1_unbias)
dimnames(tas11_unbias) <- dimnames(tas1_unbias)
dimnames(tas12_unbias) <- dimnames(tas1_unbias)

# time
time <- dimnames(tas1_ey.hat)[[3]]

# Subtract bias for each year
for(i in 1:length(time)){
  temp <- tas1_ey.hat[,,i]
  tas1_unbias[,,i] <- temp + tas1_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas2_ey.hat[,,i]
  tas2_unbias[,,i] <- temp + tas2_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas3_ey.hat[,,i]
  tas3_unbias[,,i] <- temp + tas3_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas4_ey.hat[,,i]
  tas4_unbias[,,i] <- temp + tas4_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas5_ey.hat[,,i]
  tas5_unbias[,,i] <- temp + tas5_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas6_ey.hat[,,i]
  tas6_unbias[,,i] <- temp + tas6_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas7_ey.hat[,,i]
  tas7_unbias[,,i] <- temp + tas7_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas8_ey.hat[,,i]
  tas8_unbias[,,i] <- temp + tas8_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas9_ey.hat[,,i]
  tas9_unbias[,,i] <- temp + tas9_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas10_ey.hat[,,i]
  tas10_unbias[,,i] <- temp + tas10_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas11_ey.hat[,,i]
  tas11_unbias[,,i] <- temp + tas11_e
  print(i)
}
for(i in 1:length(time)){
  temp <- tas12_ey.hat[,,i]
  tas12_unbias[,,i] <- temp + tas12_e
  print(i)
}

# Combine into larger array
tas_ey.hat <- array(, dim = list(dim(tas1_ey.hat)[[1]],
                                 dim(tas1_ey.hat)[[2]],
                                 dim(tas1_ey.hat)[[3]],
                                 12))
tas_ey.hat[,,,1] <- tas1_ey.hat
tas_ey.hat[,,,2] <- tas2_ey.hat
tas_ey.hat[,,,3] <- tas3_ey.hat
tas_ey.hat[,,,4] <- tas4_ey.hat
tas_ey.hat[,,,5] <- tas5_ey.hat
tas_ey.hat[,,,6] <- tas6_ey.hat
tas_ey.hat[,,,7] <- tas7_ey.hat
tas_ey.hat[,,,8] <- tas8_ey.hat
tas_ey.hat[,,,9] <- tas9_ey.hat
tas_ey.hat[,,,10] <- tas10_ey.hat
tas_ey.hat[,,,11] <- tas11_ey.hat
tas_ey.hat[,,,12] <- tas12_ey.hat

# Remove large monthly objects
rm(tas1_ey.hat, tas2_ey.hat, tas3_ey.hat, tas4_ey.hat,
   tas5_ey.hat, tas6_ey.hat, tas7_ey.hat, tas8_ey.hat,
   tas9_ey.hat, tas10_ey.hat, tas11_ey.hat, tas12_ey.hat)

tas_unbias <- array(, dim = dim(tas_ey.hat))

tas_unbias[,,,1] <- tas1_unbias
tas_unbias[,,,2] <- tas2_unbias
tas_unbias[,,,3] <- tas3_unbias
tas_unbias[,,,4] <- tas4_unbias
tas_unbias[,,,5] <- tas5_unbias
tas_unbias[,,,6] <- tas6_unbias
tas_unbias[,,,7] <- tas7_unbias
tas_unbias[,,,8] <- tas8_unbias
tas_unbias[,,,9] <- tas9_unbias
tas_unbias[,,,10] <- tas10_unbias
tas_unbias[,,,11] <- tas11_unbias
tas_unbias[,,,12] <- tas12_unbias

dimnames(tas_ey.hat) <- list(dimnames(tas1_unbias)[[1]],
                             dimnames(tas1_unbias)[[2]],
                             dimnames(tas1_unbias)[[3]],
                             1:12)
dimnames(tas_unbias) <- dimnames(tas_ey.hat)

rm(tas1_unbias, tas2_unbias, tas3_unbias, tas4_unbias,
   tas5_unbias, tas6_unbias, tas7_unbias, tas8_unbias,
   tas9_unbias, tas10_unbias, tas11_unbias, tas12_unbias)

rm(tas1_e, tas2_e, tas3_e, tas4_e, tas5_e, tas6_e,
   tas7_e, tas8_e, tas9_e, tas10_e, tas11_e, tas12_e)

# Save
save(tas_ey.hat, tas_unbias,
     file = 'data/intermediate/tas_201.113.1776.12.RData')

### Average annual temperature ###

# Average over months for each year
aat_ey.hat <- apply(tas_ey.hat, 1:3, mean)
aat_unbias <- apply(tas_unbias, 1:3, mean)

dimnames(aat_ey.hat) <- list(dimnames(tas_unbias)[[1]],
                             dimnames(tas_unbias)[[2]],
                             dimnames(tas_unbias)[[3]])
dimnames(aat_unbias) <- dimnames(aat_ey.hat)

# Load STEPPS data
# for time steps
load('data/processed/mean_STEPPS.RData')

# Convert to calendar years
time <- time * 100
time <- 1950 - time

# Make time intervals centered on STEPPS time steps
time_intervals <- matrix(, nrow = 18, ncol = 2)
time_intervals[,1] <- seq(from = 25, to = 1725, by = 100)
time_intervals[,2] <- seq(from = 75, to = 1775, by = 100)
time_intervals <- as.data.frame(time_intervals)
colnames(time_intervals) <- c('start', 'stop')

# Average over 50-year intervals
aat_ey.hat_mean <- array(, dim = list(dim(aat_ey.hat)[[1]],
                               dim(aat_ey.hat)[[2]],
                               nrow(time_intervals)))
aat_unbias_mean <- array(, dim = dim(aat_ey.hat_mean))

# Dim names
dimnames(aat_ey.hat_mean) <- list(dimnames(aat_ey.hat)[[1]],
                                  dimnames(aat_ey.hat)[[2]],
                                  rev(time[1:18]))
dimnames(aat_unbias_mean) <- dimnames(aat_ey.hat_mean)

# Loop over time intervals
for(i in 1:nrow(time_intervals)){
  # years included in given time interval
  tt <- as.character(seq(from = time_intervals[i,1],
                         to = time_intervals[i,2],
                         by = 1))
  # subset ey.hat for given time interval
  temp <- aat_ey.hat[,,dimnames(aat_ey.hat)[[3]] %in% tt]
  # average again
  aat_ey.hat_mean[,,i] <- apply(temp, 1:2, mean)
  
  # repeat for bias-corrected values
  temp <- aat_unbias[,,dimnames(aat_unbias)[[3]] %in% tt]
  aat_unbias_mean[,,i] <- apply(temp, 1:2, mean)
  
  print(i)
}

# Save
save(aat_ey.hat_mean, aat_unbias_mean,
     file = 'data/intermediate/mean_average_annual_temperature.RData')

### Temperature seasonality ###

# Find standard deviation of temperature across months for each year
tsd_ey.hat <- apply(tas_ey.hat, 1:3, sd)
tsd_unbias <- apply(tas_unbias, 1:3, sd)

# Dim names
dimnames(tsd_ey.hat) <- dimnames(aat_ey.hat)
dimnames(tsd_unbias) <- dimnames(aat_unbias)

# Average over 50-year intervals
tsd_ey.hat_mean <- array(, dim = dim(aat_ey.hat_mean))
tsd_unbias_mean <- array(, dim = dim(aat_unbias_mean))

# Dim names
dimnames(tsd_ey.hat_mean) <- dimnames(aat_ey.hat_mean)
dimnames(tsd_unbias_mean) <- dimnames(aat_unbias_mean)

# Loop over time intervals
for(i in 1:nrow(time_intervals)){
  # years included in given time interval
  tt <- as.character(seq(from = time_intervals[i,1],
                         to = time_intervals[i,2],
                         by = 1))
  # subset ey.hat for given time interval
  temp <- tsd_ey.hat[,,dimnames(tsd_ey.hat)[[3]] %in% tt]
  # average
  tsd_ey.hat_mean[,,i] <- apply(temp, 1:2, mean)
  
  # repeat for bias-corrected values
  temp <- tsd_unbias[,,dimnames(tsd_unbias)[[3]] %in% tt]
  tsd_unbias_mean[,,i] <- apply(temp, 1:2, mean)
  
  print(i)
}

# Save
save(tsd_ey.hat_mean, tsd_unbias_mean,
     file = 'data/intermediate/mean_temperature_seasonality.RData')

#### Precipitation ####

rm(list = ls())

### Loading and processing ###

# Load monthly precipitation
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr1.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr2.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr3.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr4.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr5.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr6.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr7.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr8.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr9.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr10.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr11.RData')
load('/Volumes/FileBackup/Climate_Downscaling_RG/processed_monthly_Rdata/pr12.RData')

# Bias correct
pr1_unbias <- array(, dim = dim(pr1_ey.hat))
pr2_unbias <- array(, dim = dim(pr2_ey.hat))
pr3_unbias <- array(, dim = dim(pr3_ey.hat))
pr4_unbias <- array(, dim = dim(pr4_ey.hat))
pr5_unbias <- array(, dim = dim(pr5_ey.hat))
pr6_unbias <- array(, dim = dim(pr6_ey.hat))
pr7_unbias <- array(, dim = dim(pr7_ey.hat))
pr8_unbias <- array(, dim = dim(pr8_ey.hat))
pr9_unbias <- array(, dim = dim(pr9_ey.hat))
pr10_unbias <- array(, dim = dim(pr10_ey.hat))
pr11_unbias <- array(, dim = dim(pr11_ey.hat))
pr12_unbias <- array(, dim = dim(pr12_ey.hat))

# Apply dimension names
dimnames(pr1_unbias) <- dimnames(pr1_ey.hat)
dimnames(pr2_unbias) <- dimnames(pr1_unbias)
dimnames(pr3_unbias) <- dimnames(pr1_unbias)
dimnames(pr4_unbias) <- dimnames(pr1_unbias)
dimnames(pr5_unbias) <- dimnames(pr1_unbias)
dimnames(pr6_unbias) <- dimnames(pr1_unbias)
dimnames(pr7_unbias) <- dimnames(pr1_unbias)
dimnames(pr8_unbias) <- dimnames(pr1_unbias)
dimnames(pr9_unbias) <- dimnames(pr1_unbias)
dimnames(pr10_unbias) <- dimnames(pr1_unbias)
dimnames(pr11_unbias) <- dimnames(pr1_unbias)
dimnames(pr12_unbias) <- dimnames(pr1_unbias)

# time
time <- dimnames(pr1_ey.hat)[[3]]

# Subtract bias for each year
for(i in 1:length(time)){
  temp <- pr1_ey.hat[,,i]
  pr1_unbias[,,i] <- temp + pr1_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr2_ey.hat[,,i]
  pr2_unbias[,,i] <- temp + pr2_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr3_ey.hat[,,i]
  pr3_unbias[,,i] <- temp + pr3_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr4_ey.hat[,,i]
  pr4_unbias[,,i] <- temp + pr4_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr5_ey.hat[,,i]
  pr5_unbias[,,i] <- temp + pr5_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr6_ey.hat[,,i]
  pr6_unbias[,,i] <- temp + pr6_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr7_ey.hat[,,i]
  pr7_unbias[,,i] <- temp + pr7_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr8_ey.hat[,,i]
  pr8_unbias[,,i] <- temp + pr8_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr9_ey.hat[,,i]
  pr9_unbias[,,i] <- temp + pr9_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr10_ey.hat[,,i]
  pr10_unbias[,,i] <- temp + pr10_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr11_ey.hat[,,i]
  pr11_unbias[,,i] <- temp + pr11_e
  print(i)
}
for(i in 1:length(time)){
  temp <- pr12_ey.hat[,,i]
  pr12_unbias[,,i] <- temp + pr12_e
  print(i)
}

# Combine into larger array
pr_ey.hat <- array(, dim = list(dim(pr1_ey.hat)[[1]],
                                dim(pr1_ey.hat)[[2]],
                                dim(pr1_ey.hat)[[3]],
                                12))
pr_ey.hat[,,,1] <- pr1_ey.hat
pr_ey.hat[,,,2] <- pr2_ey.hat
pr_ey.hat[,,,3] <- pr3_ey.hat
pr_ey.hat[,,,4] <- pr4_ey.hat
pr_ey.hat[,,,5] <- pr5_ey.hat
pr_ey.hat[,,,6] <- pr6_ey.hat
pr_ey.hat[,,,7] <- pr7_ey.hat
pr_ey.hat[,,,8] <- pr8_ey.hat
pr_ey.hat[,,,9] <- pr9_ey.hat
pr_ey.hat[,,,10] <- pr10_ey.hat
pr_ey.hat[,,,11] <- pr11_ey.hat
pr_ey.hat[,,,12] <- pr12_ey.hat

# Remove large monthly objects
rm(pr1_ey.hat, pr2_ey.hat, pr3_ey.hat, pr4_ey.hat,
   pr5_ey.hat, pr6_ey.hat, pr7_ey.hat, pr8_ey.hat,
   pr9_ey.hat, pr10_ey.hat, pr11_ey.hat, pr12_ey.hat)

pr_unbias <- array(, dim = dim(pr_ey.hat))

pr_unbias[,,,1] <- pr1_unbias
pr_unbias[,,,2] <- pr2_unbias
pr_unbias[,,,3] <- pr3_unbias
pr_unbias[,,,4] <- pr4_unbias
pr_unbias[,,,5] <- pr5_unbias
pr_unbias[,,,6] <- pr6_unbias
pr_unbias[,,,7] <- pr7_unbias
pr_unbias[,,,8] <- pr8_unbias
pr_unbias[,,,9] <- pr9_unbias
pr_unbias[,,,10] <- pr10_unbias
pr_unbias[,,,11] <- pr11_unbias
pr_unbias[,,,12] <- pr12_unbias

dimnames(pr_ey.hat) <- list(dimnames(pr1_unbias)[[1]],
                            dimnames(pr1_unbias)[[2]],
                            dimnames(pr1_unbias)[[3]],
                            1:12)
dimnames(pr_unbias) <- dimnames(pr_ey.hat)

rm(pr1_unbias, pr2_unbias, pr3_unbias, pr4_unbias,
   pr5_unbias, pr6_unbias, pr7_unbias, pr8_unbias,
   pr9_unbias, pr10_unbias, pr11_unbias, pr12_unbias)

rm(pr1_e, pr2_e, pr3_e, pr4_e, pr5_e, pr6_e,
   pr7_e, pr8_e, pr9_e, pr10_e, pr11_e, pr12_e)

# Save
save(pr_ey.hat, pr_unbias,
     file = 'data/intermediate/pr_201.113.1776.12.RData')

### Total annual precipitation ###

# Sum over months for each year
tpr_ey.hat <- apply(pr_ey.hat, 1:3, sum)
tpr_unbias <- apply(pr_unbias, 1:3, sum)

dimnames(tpr_ey.hat) <- list(dimnames(pr_unbias)[[1]],
                             dimnames(pr_unbias)[[2]],
                             dimnames(pr_unbias)[[3]])
dimnames(tpr_unbias) <- dimnames(tpr_ey.hat)

# Load STEPPS data
# for time steps
load('data/processed/mean_STEPPS.RData')

# Convert to calendar years
time <- time * 100
time <- 1950 - time

# Make time intervals centered on STEPPS time steps
time_intervals <- matrix(, nrow = 18, ncol = 2)
time_intervals[,1] <- seq(from = 25, to = 1725, by = 100)
time_intervals[,2] <- seq(from = 75, to = 1775, by = 100)
time_intervals <- as.data.frame(time_intervals)
colnames(time_intervals) <- c('start', 'stop')

# Average over 50-year intervals
tpr_ey.hat_mean <- array(, dim = list(dim(tpr_ey.hat)[[1]],
                                      dim(tpr_ey.hat)[[2]],
                                      nrow(time_intervals)))
tpr_unbias_mean <- array(, dim = dim(tpr_ey.hat_mean))

# Dim names
dimnames(tpr_ey.hat_mean) <- list(dimnames(tpr_ey.hat)[[1]],
                                  dimnames(tpr_ey.hat)[[2]],
                                  rev(time[1:18]))
dimnames(tpr_unbias_mean) <- dimnames(tpr_ey.hat_mean)

# Loop over time intervals
for(i in 1:nrow(time_intervals)){
  # years included in given time interval
  tt <- as.character(seq(from = time_intervals[i,1],
                         to = time_intervals[i,2],
                         by = 1))
  # subset ey.hat for given time interval
  temp <- tpr_ey.hat[,,dimnames(tpr_ey.hat)[[3]] %in% tt]
  # average
  tpr_ey.hat_mean[,,i] <- apply(temp, 1:2, mean)
  
  # repeat for bias-corrected values
  temp <- tpr_unbias[,,dimnames(tpr_unbias)[[3]] %in% tt]
  tpr_unbias_mean[,,i] <- apply(temp, 1:2, mean)
  
  print(i)
}

# Save
save(tpr_ey.hat_mean, tpr_unbias_mean,
     file = 'data/intermediate/mean_total_annual_precipitation.RData')

### Precipitation seasonality ###

# Find standard deviation of precipitation across months for each year
prsd_ey.hat <- apply(pr_ey.hat, 1:3, sd)
prsd_unbias <- apply(pr_unbias, 1:3, sd)

# Mean precipitation across year
prmean_ey.hat <- apply(pr_ey.hat, 1:3, mean)
prmean_unbias <- apply(pr_unbias, 1:3, mean)

# Coefficient of variation
prcv_ey.hat <- prsd_ey.hat / prmean_ey.hat
prcv_unbias <- prsd_unbias / prmean_unbias

# Dim names
dimnames(prsd_ey.hat) <- dimnames(tpr_ey.hat)
dimnames(prsd_unbias) <- dimnames(tpr_unbias)
dimnames(prcv_ey.hat) <- dimnames(tpr_ey.hat)
dimnames(prcv_unbias) <- dimnames(tpr_unbias)

# Average over 50-year intervals
prsd_ey.hat_mean <- array(, dim = dim(tpr_ey.hat_mean))
prsd_unbias_mean <- array(, dim = dim(tpr_unbias_mean))
prcv_ey.hat_mean <- array(, dim = dim(tpr_ey.hat_mean))
prcv_unbias_mean <- array(, dim = dim(tpr_unbias_mean))

# Dim names
dimnames(prsd_ey.hat_mean) <- dimnames(tpr_ey.hat_mean)
dimnames(prsd_unbias_mean) <- dimnames(tpr_unbias_mean)
dimnames(prcv_ey.hat_mean) <- dimnames(tpr_ey.hat_mean)
dimnames(prcv_unbias_mean) <- dimnames(tpr_unbias_mean)

# Loop over time intervals
for(i in 1:nrow(time_intervals)){
  # years included in given time interval
  tt <- as.character(seq(from = time_intervals[i,1],
                         to = time_intervals[i,2],
                         by = 1))
  # subset ey.hat for given time interval
  temp <- prsd_ey.hat[,,dimnames(prsd_ey.hat)[[3]] %in% tt]
  # average
  prsd_ey.hat_mean[,,i] <- apply(temp, 1:2, mean)
  
  # repeat for bias-corrected values
  temp <- prsd_unbias[,,dimnames(prsd_unbias)[[3]] %in% tt]
  prsd_unbias_mean[,,i] <- apply(temp, 1:2, mean)
  
  # repeat for cv
  temp <- prcv_ey.hat[,,dimnames(prcv_ey.hat)[[3]] %in% tt]
  prcv_ey.hat_mean[,,i] <- apply(temp, 1:2, mean)
  
  temp <- prcv_unbias[,,dimnames(prcv_unbias)[[3]] %in% tt]
  prcv_unbias_mean[,,i] <- apply(temp, 1:2, mean)
  
  print(i)
}

# Save
save(prsd_ey.hat_mean, prsd_unbias_mean,
     prcv_ey.hat_mean, prcv_unbias_mean,
     file = 'data/intermediate/mean_precipitation_seasonality.RData')
