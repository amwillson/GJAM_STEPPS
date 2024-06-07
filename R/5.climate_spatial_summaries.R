## Spatially matching climate to STEPPS

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

# Initial plots to check things out
aat_ey.hat |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
aat_unbias |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
tpr_ey.hat |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
tpr_unbias |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
tsd_ey.hat |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
tsd_unbias |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
prsd_ey.hat |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
prsd_unbias |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()

# Check magnitude of bias
bias <- aat_ey.hat_mean - aat_unbias_mean
bias <- reshape2::melt(bias)
colnames(bias) <- c('x', 'y', 'time', 'bias')

bias |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bias)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()

bias <- tpr_ey.hat_mean - tpr_unbias_mean
bias <- reshape2::melt(bias)
colnames(bias) <- c('x', 'y', 'time', 'bias')

bias |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bias)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()

bias <- tsd_ey.hat_mean - tsd_unbias_mean
bias <- reshape2::melt(bias)
colnames(bias) <- c('x', 'y', 'time', 'bias')

bias |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bias)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()

bias <- prsd_ey.hat_mean - prsd_unbias_mean
bias <- reshape2::melt(bias)
colnames(bias) <- c('x', 'y', 'time', 'bias')

bias |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = bias)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()

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

### Convert coordinate system ###

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

### Clip to state extents ###

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

# Load if on the VM
load('data/intermediate/clipped_clim_50.RData')

#### Match points to PLS points ####

## This section must be run on VM due to memory constraints

# Load PLS point level data for area of interest
load('data/processed/PLS_point/minnesota_process.RData')
load('data/processed/PLS_point/upmichigan_process.RData')
load('data/processed/PLS_point/wisconsin_process.RData')

# Combine all PLS data
pls <- rbind(minnesota, upmichigan, wisconsin)

# Transform so that each point has one line
pls <- pls |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x),
                keep_x = x,
                keep_y = y)

# Define coordinate system
pls <- sf::st_as_sf(pls,
                    coords = c('x', 'y'),
                    crs = 'EPSG:4326')

# Convert coordinate system (because of planar assumption)
pls <- sf::st_transform(pls, crs = 'EPSG:3175')

# Keep only what's in our study region (removes part of lower peninsula)
pls <- sf::st_intersection(pls, states)
# Convert back to data frame
pls <- sfheaders::sf_to_df(pls, fill = TRUE)
# Keep only necessary columns
pls <- dplyr::select(pls, keep_x, keep_y, uniqueID)

save(pls, file = 'data/intermediate/clipped_pls.RData')

# Load if on the VM
load('data/intermediate/clipped_pls.RData')

# Define coordinate system of climate data
# (we haad transformed earlier)
ey.hat_50 <- sf::st_as_sf(ey.hat_50,
                          coords = c('x', 'y'),
                          crs = 'EPSG:3175')
unbias_50 <- sf::st_as_sf(unbias_50,
                          coords = c('x', 'y'),
                          crs = 'EPSG:3175')
# Transform back to keep things consistent across workflows
ey.hat_50 <- sf::st_transform(ey.hat_50,
                              crs = 'EPSG:4326')
unbias_50 <- sf::st_transform(unbias_50,
                              crs = 'EPSG:4326')
# Convert back to data frame
ey.hat_50 <- sfheaders::sf_to_df(ey.hat_50, fill = TRUE)
unbias_50 <- sfheaders::sf_to_df(unbias_50, fill = TRUE)
# Remove unnecessary columns
ey.hat_50 <- dplyr::select(ey.hat_50, -sfg_id, -point_id)
unbias_50 <- dplyr::select(unbias_50, -sfg_id, -point_id)

# Change name of pls coordinates
pls <- dplyr::rename(pls, x = keep_x, y = keep_y)

# Make two subsets of pls
pls1 <- pls[1:253536,]
pls2 <- pls[253537:nrow(pls),]

# Select coordinates
coords_pls1 <- dplyr::select(pls1, y, x)
coords_pls2 <- dplyr::select(pls2, y, x)
coords_ey.hat <- dplyr::select(ey.hat_50, y, x)
coords_unbias <- dplyr::select(unbias_50, y, x)

# Check that the 2 climate dataframes are identical
identical(coords_ey.hat$y, coords_unbias$y)
identical(coords_ey.hat$x, coords_unbias$x)

### Subset 1 ###

# Find the distance between each pair of points in the pls (1) and climate (2) dataframes
dists <- fields::rdist(coords_pls1, coords_ey.hat)
# Find the closest climate point to each PLS point
# (should have length = nrow(pls subset))
closest_point <- apply(dists, 1, which.min)
# Immediately remove dists because it's huge
rm(dists)

select_ey.hat <- dplyr::slice(ey.hat_50, closest_point)

clim_pls <- cbind(select_ey.hat, pls1)

rm(closest_point, coords_pls1, select_ey.hat, pls1)

### Subset 2 ###

dists <- fields::rdist(coords_pls2, coords_ey.hat)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- dplyr::slice(ey.hat_50, closest_point)

temp <- cbind(select_ey.hat, pls2)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_pls2, select_ey.hat, pls2)

# Update column names
colnames(clim_pls) <- c('time', 'aat', 'tpr', 'tsd', 'prsd', 'spatID', 'clim_x', 'clim_y', 'pls_x', 'pls_y', 'uniqueID')

# Check that coordinates match up
clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = clim_x)) +
  ggplot2::geom_abline()

clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_y, y = clim_y)) +
  ggplot2::geom_abline()

# Check that spatial patterns make sense
clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = aat)) +
  ggplot2::facet_wrap(~time)

clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = tpr)) +
  ggplot2::facet_wrap(~time)

clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = tsd)) +
  ggplot2::facet_wrap(~time)

clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = prsd)) +
  ggplot2::facet_wrap(~time)

# Save
save(clim_pls, file = 'data/intermediate/point_matched_clim.RData')
