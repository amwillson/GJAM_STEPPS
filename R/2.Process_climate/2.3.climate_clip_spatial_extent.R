### STEP 2-3

## Perform checks on spatio-temporal variability of downscaled climate reconstructions
## Then clip the extent to the extent of the STEPPS vegetation reconstructions
## Prepares climate data to be matched to PLS points in the next step

## Input: data/intermediate/mean_average_annual_temperature.RData,
## Input: data/intermediate/mean_total_annual_precipitation.RData
## Input: data/intermediate/mean_temperature_seasonality.RData
## Input: data/intermediate/mean_precipitation_seasonality.RData
## Climate summaries in array format from previous step

## Output: data/intermediate/clipped_clim_50.RData
## Output: data/intermediate/clipped_clim_alltime.RData
## Dataframes with coordinates, time, and climate reconstructions for narrower spatial
## extent and coarser temporal resolution that matches STEPPS data product
## clipped_clim_50 contains data only from the 50 CE timestep. This is useful because
## all spatial locations are represented once.
## clipped_clim_alltime contains data from all time steps and spatial locations are
## repeated at each time step
## clipped_clim_50 used in 2.4.climate_points_to_grid.R
## clipped_clim_alltime used in 2.5.climate_aggregate_to_grid.R

rm(list = ls())

# Source helper funs
source('R/funs.R')

#### Load data ####

# Load temporally scaled data
load('data/intermediate/mean_average_annual_temperature.RData')
load('data/intermediate/mean_total_annual_precipitation.RData')
load('data/intermediate/mean_temperature_seasonality.RData')
load('data/intermediate/mean_precipitation_seasonality.RData')

# Melt
aat_ey.hat <- reshape2::melt(aat_ey.hat_mean)
aat_unbias <- reshape2::melt(aat_unbias_mean)
tpr_ey.hat <- reshape2::melt(tpr_ey.hat_mean)
tpr_unbias <- reshape2::melt(tpr_unbias_mean)
tsd_ey.hat <- reshape2::melt(tsd_ey.hat_mean)
tsd_unbias <- reshape2::melt(tsd_unbias_mean)
prsd_ey.hat <- reshape2::melt(prsd_ey.hat_mean)
prsd_unbias <- reshape2::melt(prsd_unbias_mean)

# Add column names
colnames(aat_ey.hat) <- c('x', 'y', 'time', 'aat')
colnames(aat_unbias) <- c('x', 'y', 'time', 'aat')
colnames(tpr_ey.hat) <- c('x', 'y', 'time', 'tpr')
colnames(tpr_unbias) <- c('x', 'y', 'time', 'tpr')
colnames(tsd_ey.hat) <- c('x', 'y', 'time', 'tsd')
colnames(tsd_unbias) <- c('x', 'y', 'time', 'tsd')
colnames(prsd_ey.hat) <- c('x', 'y', 'time', 'prsd')
colnames(prsd_unbias) <- c('x', 'y', 'time', 'prsd')

# Make map of study region
states <- map_states()

states <- sf::st_transform(states, crs = 'EPSG:4326')

# Order of facets
time_order <- c('50 CE', '150 CE', '250 CE', '350 CE', '450 CE', '550 CE',
                '650 CE', '750 CE', '850 CE', '950 CE', '1050 CE', '1150 CE',
                '1250 CE', '1350 CE', '1450 CE', '1550 CE', '1650 CE', '1750 CE')

# Initial plots to check things out
aat_ey.hat |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'A', name = 'Average annual\ntemperature') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Original model estimate') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))
aat_unbias |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'A', name = 'Average annual\ntemperature') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Debiased estimate') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))
tpr_ey.hat |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'G', name = 'Total annual\nprecipitation') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Original model estimate') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))
tpr_unbias |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'G', name = 'Total annual\nprecipitation') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Debiased estimate') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))
tsd_ey.hat |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'D', name = 'Temperature\nseasonality') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Original model estimate') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))
tsd_unbias |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'D', name = 'Temperature\nseasonality') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Debiased estimate') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))
prsd_ey.hat |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'E', name = 'Precipitation\nseasonality') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Original model estimate') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))
prsd_unbias |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'E', name = 'Precipitation\nseasonality') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Debiased estimate') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

# Check magnitude of bias
bias <- aat_ey.hat_mean - aat_unbias_mean
bias <- reshape2::melt(bias)
colnames(bias) <- c('x', 'y', 'time', 'bias')

bias |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bias)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Bias in average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

bias <- tpr_ey.hat_mean - tpr_unbias_mean
bias <- reshape2::melt(bias)
colnames(bias) <- c('x', 'y', 'time', 'bias')

bias |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bias)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Bias in total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

bias <- tsd_ey.hat_mean - tsd_unbias_mean
bias <- reshape2::melt(bias)
colnames(bias) <- c('x', 'y', 'time', 'bias')

bias |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bias)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Bias in temperature seasonality') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

bias <- prsd_ey.hat_mean - prsd_unbias_mean
bias <- reshape2::melt(bias)
colnames(bias) <- c('x', 'y', 'time', 'bias')

bias |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, ' CE')) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bias)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Bias in precipitation seasonality') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

# Combine variables
ey.hat <- aat_ey.hat |>
  dplyr::full_join(y = tpr_ey.hat, by = c('x', 'y', 'time')) |>
  dplyr::full_join(y = tsd_ey.hat, by = c('x', 'y', 'time')) |>
  dplyr::full_join(y = prsd_ey.hat, by = c('x', 'y', 'time'))
unbias <- aat_unbias |>
  dplyr::full_join(y = tpr_unbias, by = c('x', 'y', 'time')) |>
  dplyr::full_join(y = tsd_unbias, by = c('x', 'y', 'time')) |>
  dplyr::full_join(y = prsd_unbias, by = c('x', 'y', 'time'))

# Add spatial ID column because of multiple time steps
ey.hat <- dplyr::mutate(ey.hat, spatID = paste0(x, '_', y))
unbias <- dplyr::mutate(unbias, spatID = paste0(x, '_', y))

# Check that that spatIDs are the same in both dataframes
identical(ey.hat$spatID, unbias$spatID)

# Check that there are the exact same number of spatial locations in each timestep
nrow(ey.hat) / length(unique(ey.hat$spatID)) == length(unique(ey.hat$time))

#### Convert coordinate system ####

ey.hat <- sf::st_as_sf(ey.hat,
                       coords = c('x', 'y'),
                       crs = 'EPSG:4326')
unbias <- sf::st_as_sf(unbias,
                       coords = c('x', 'y'),
                       crs = 'EPSG:4326')

ey.hat <- sf::st_transform(ey.hat,
                           crs = 'EPSG:3175')
unbias <- sf::st_transform(unbias,
                           crs = 'EPSG:3175')

#### Clip to state extents ####

# Take one time period
# use the spatID to apply to other time periods
ey.hat_50 <- dplyr::filter(ey.hat, time == 50)
unbias_50 <- dplyr::filter(unbias, time == 50)

# Re-convert map
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Clip climate reconstructions to extent of region of interest
ey.hat_50 <- sf::st_intersection(ey.hat_50, states)
unbias_50 <- sf::st_intersection(unbias_50, states)

# Convert back to data frame
ey.hat_50 <- sfheaders::sf_to_df(ey.hat_50,
                              fill = TRUE)
unbias_50 <- sfheaders::sf_to_df(unbias_50,
                              fill = TRUE)

# Remove unnecessary columns
ey.hat_50 <- dplyr::select(ey.hat_50, c(time:spatID, x, y))
unbias_50 <- dplyr::select(unbias_50, c(time:spatID, x, y))

# List of unique spatID
unique_spatID <- ey.hat_50$spatID

# Filter the full dataset for these spatIDs
ey.hat <- dplyr::filter(ey.hat, spatID %in% unique_spatID)
unbias <- dplyr::filter(unbias, spatID %in% unique_spatID)

# Convert back to dataframe
ey.hat <- sfheaders::sf_to_df(ey.hat,
                              fill = TRUE)
unbias <- sfheaders::sf_to_df(unbias,
                              fill = TRUE)

# Remove unnecessary columns
ey.hat <- dplyr::select(ey.hat, -sfg_id, -point_id)
unbias <- dplyr::select(unbias, -sfg_id, -point_id)

# Plot again
ey.hat |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
unbias |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) + 
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
ey.hat |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
unbias |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
ey.hat |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
unbias |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
ey.hat |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
unbias |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()

# Save full climate with all time steps
save(ey.hat, unbias, file = 'data/intermediate/clipped_clim_alltime.RData')

# Save climate with one time step
save(ey.hat_50, unbias_50, file = 'data/intermediate/clipped_clim_50.RData')