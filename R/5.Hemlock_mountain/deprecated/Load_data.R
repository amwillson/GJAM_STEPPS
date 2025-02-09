## Building simple dynamic linear model for testing
## temporal depedence in STEPPS record for a subset
## of locations

## Beta regression with standardized relative abundance
## estimated in the model

rm(list = ls())

# Helper funs
source('R/funs.R')

#### Format all data ####

# Load original in-sample and oos data
load('data/processed/mean_stepps_soil_clim.RData')

# Combine
taxon_insample <- rbind(taxon_insample_all, taxon_oos_all)

# Load all the rest of the data
load('data/processed/mean_stepps_full_oos.RData')

# Combine
taxon_all <- rbind(taxon_insample, taxon_oos_all)

# Format covariates (xdata)
xdata <- taxon_all |>
  # Remove NAs where grid cells are empty
  tidyr::drop_na() |>
  # Add location index
  dplyr::mutate(loc = paste0(stepps_x, '_', stepps_y)) |>
  # Take only covariates and indexing
  dplyr::select(time, stepps_x, stepps_y, loc, 
                sand, aat, tpr, prsd)

# Format response variable (ydata)
# Note that currently, we're only interested in hemlock
# because only looking at Hemlock Mountain
ydata <- taxon_all |>
  # Remove NAs where grid cells are empty
  tidyr::drop_na() |>
  # Add location index
  dplyr::mutate(loc = paste0(stepps_x, '_', stepps_y)) |>
  # Take hemlock and indexing
  dplyr::select(time, stepps_x, stepps_y, loc, 
                hemlock)

# Need to change the direction time moves
# Time is currently in YBP so runs backward
# Index time to go from 1 to 17 instead
times <- as.data.frame(unique(xdata$time)[rev(order(unique(xdata$time)))])
colnames(times) <- 'time'
times$time_ind <- seq(from = 1, length.out = nrow(times))

# Add new times to xdata
xdata <- xdata |>
  dplyr::left_join(y = times,
                   by = 'time') |>
  dplyr::select(-time) |>
  dplyr::rename(time = time_ind)
# Add new times to ydata
ydata <- ydata |>
  dplyr::left_join(y = times,
                   by = 'time') |>
  dplyr::select(-time) |>
  dplyr::rename(time = time_ind)
