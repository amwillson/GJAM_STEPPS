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

# Re-convert map
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Clip climate reconstructions to extent of region of interest
ey.hat <- sf::st_intersection(ey.hat, states)
unbias <- sf::st_intersection(unbias, states)

# Convert back to data frame
ey.hat <- sfheaders::sf_to_df(ey.hat,
                              fill = TRUE)
unbias <- sfheaders::sf_to_df(unbias,
                              fill = TRUE)

# Remove unnecessary columns
ey.hat <- dplyr::select(ey.hat, c(time:prsd, x, y))
unbias <- dplyr::select(unbias, c(time:prsd, x, y))

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

#### Match points to STEPPS grid ####

# Load PLS point level data for area of interest
load('data/processed/PLS_point/minnesota_process.RData')
load('data/processed/PLS_point/upmichigan_process.RData')
load('data/processed/PLS_point/wisconsin_process.RData')

# Combine all PLS data
pls <- rbind(minnesota, upmichigan, wisconsin)

# Transform so that each point has one line
pls <- pls |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Define coordinate system
pls <- sf::st_as_sf(pls,
                    coords = c('x', 'y'),
                    crs = 'EPSG:4326')

# Convert coordinate system of states
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Keep only what's in our study region (removes part of lower peninsula)
pls <- sf::st_intersection(pls, states)
# Convert back to data frame
pls <- sfheaders::sf_to_df(pls, fill = TRUE)
# Keep only necessary columns
pls <- dplyr::select(pls, x, y, uniqueID)

# Define coordinate system of climate data
# (we haad transformed earlier)
ey.hat <- sf::st_as_sf(ey.hat,
                       coords = c('x', 'y'),
                       crs = 'EPSG:3175')
unbias <- sf::st_as_sf(unbias,
                       coords = c('x', 'y'),
                       crs = 'EPSG:3175')
# Transform back to keep things consistent across workflows
ey.hat <- sf::st_transform(ey.hat,
                           crs = 'EPSG:4326')
unbias <- sf::st_transform(unbias,
                           crs = 'EPSG:4326')
# Convert back to data frame
ey.hat <- sfheaders::sf_to_df(ey.hat, fill = TRUE)
unbias <- sfheaders::sf_to_df(unbias, fill = TRUE)
# Remove unnecessary columns
ey.hat <- dplyr::select(ey.hat, -sfg_id, -point_id)
unbias <- dplyr::select(unbias, -sfg_id, -point_id)

# Select coordinates
coords_pls <- dplyr::select(pls, y, x)
coords_ey.hat <- dplyr::select(ey.hat, y, x)
coords_unbias <- dplyr::select(unbias, y, x)

dists <- fields::rdist(coords_pls, coords_ey.hat)
