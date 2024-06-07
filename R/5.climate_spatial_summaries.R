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

## This section must be run on VM due to memory contraints

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

# Change name of pls coordinates
pls <- dplyr::rename(pls, x = keep_x, y = keep_y)

# Divide pls data into sections
pls1 <- pls[1:25000,]
pls2 <- pls[25001:50000,]
pls3 <- pls[50001:75000,]
pls4 <- pls[75001:100000,]
pls5 <- pls[100001:125000,]
pls6 <- pls[125001:150000,]
pls7 <- pls[150001:175000,]
pls8 <- pls[175001:200000,]
pls9 <- pls[200001:225000,]
pls10 <- pls[225001:250000,]
pls11 <- pls[250001:275000,]
pls12 <- pls[275001:300000,]
pls13 <- pls[300001:325000,]
pls14 <- pls[325001:350000,]
pls15 <- pls[350001:375000,]
pls16 <- pls[375001:400000,]
pls17 <- pls[400001:425000,]
pls18 <- pls[425001:450000,]
pls19 <- pls[450001:475000,]
pls20 <- pls[475001:500000,]
pls21 <- pls[500001:nrow(pls),]

# Select coordinates
coords_pls1 <- dplyr::select(pls1, y, x)
coords_pls2 <- dplyr::select(pls2, y, x)
coords_pls3 <- dplyr::select(pls3, y, x)
coords_pls4 <- dplyr::select(pls4, y, x)
coords_pls5 <- dplyr::select(pls5, y, x)
coords_pls6 <- dplyr::select(pls6, y, x)
coords_pls7 <- dplyr::select(pls7, y, x)
coords_pls8 <- dplyr::select(pls8, y, x)
coords_pls9 <- dplyr::select(pls9, y, x)
coords_pls10 <- dplyr::select(pls10, y, x)
coords_pls11 <- dplyr::select(pls11, y, x)
coords_pls12 <- dplyr::select(pls12, y, x)
coords_pls13 <- dplyr::select(pls13, y, x)
coords_pls14 <- dplyr::select(pls14, y, x)
coords_pls15 <- dplyr::select(pls15, y, x)
coords_pls16 <- dplyr::select(pls16, y, x)
coords_pls17 <- dplyr::select(pls17, y, x)
coords_pls18 <- dplyr::select(pls18, y, x)
coords_pls19 <- dplyr::select(pls19, y, x)
coords_pls20 <- dplyr::select(pls20, y, x)
coords_pls21 <- dplyr::select(pls21, y, x)

coords_ey.hat <- dplyr::select(ey.hat, y, x)
coords_unbias <- dplyr::select(unbias, y, x)

# Check that the 2 climate dataframes are identical
identical(coords_ey.hat$y, coords_unbias$y)
identical(coords_ey.hat$x, coords_unbias$x)

### Subset 1 ###
# Subset coords of climate to reduce computation time
coords_ey.hat1 <- dplyr::filter(coords_ey.hat,
                                x >= min(coords_pls1$x) - 0.1 & x <= max(coords_pls1$x) + 0.1 &
                                  y >= min(coords_pls1$y) - 0.1 & y <= max(coords_pls1$y) + 0.1)

# Find the distance between each pair of points in the pls (1) and climate (2) dataframes
dists <- fields::rdist(coords_pls1, coords_ey.hat1)
# Find the closest climate point to each PLS point
# (should have length = nrow(pls subset))
closest_point <- apply(dists, 1, which.min)
# Immediately remove dists because it's huge
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls1$x) - 0.1 & x <= max(coords_pls1$x) + 0.1 &
                  y >= min(coords_pls1$y) - 0.1 & y <= max(coords_pls1$y) + 0.1) |>
  dplyr::slice(closest_point)

clim_pls <- cbind(select_ey.hat, pls1)

rm(closest_point, coords_ey.hat1, coords_pls1, select_ey.hat, pls1)

### Subset 2 ###

coords_ey.hat2 <- dplyr::filter(coords_ey.hat,
                                x >= min(coords_pls2$x) - 0.1 & x <= max(coords_pls2$x) + 0.1 &
                                  y >= min(coords_pls2$y) - 0.1 & y <= max(coords_pls2$y) + 0.1)


dists <- fields::rdist(coords_pls2, coords_ey.hat2)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls2$x) - 0.1 & x <= max(coords_pls2$x) + 0.1 &
                  y >= min(coords_pls2$y) - 0.1 & y <= max(coords_pls2$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls2)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat2, coords_pls2, select_ey.hat, pls2)

### Subset 3 ###

coords_ey.hat3 <- dplyr::filter(coords_ey.hat,
                                x >= min(coords_pls3$x) - 0.1 & x <= max(coords_pls3$x) + 0.1 &
                                  y >= min(coords_pls3$y) - 0.1 & y <= max(coords_pls3$y) + 0.1)

dists <- fields::rdist(coords_pls3, coords_ey.hat3)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls3$x) - 0.1 & x <= max(coords_pls3$x) + 0.1 &
                  y >= min(coords_pls3$y) - 0.1 & y <= max(coords_pls3$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls3)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat3, coords_pls3, select_ey.hat, pls3)

### Subset 4 ###

coords_ey.hat4 <- dplyr::filter(coords_ey.hat,
                                x >= min(coords_pls4$x) - 0.1 & x <= max(coords_pls4$x) + 0.1 &
                                  y >= min(coords_pls4$y) - 0.1 & y <= max(coords_pls4$y) + 0.1)

dists <- fields::rdist(coords_pls4, coords_ey.hat4)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls4$x) - 0.1 & x <= max(coords_pls4$x) + 0.1 &
                  y >= min(coords_pls4$y) - 0.1 & y <= max(coords_pls4$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls4)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat4, coords_pls4, select_ey.hat, pls4)

### Subset 5 ###

coords_ey.hat5 <- dplyr::filter(coords_ey.hat,
                                x >= min(coords_pls5$x) - 0.1 & x <= max(coords_pls5$x) + 0.1 &
                                  y >= min(coords_pls5$y) - 0.1 & y <= max(coords_pls5$y) + 0.1)

dists <- fields::rdist(coords_pls5, coords_ey.hat5)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls5$x) - 0.1 & x <= max(coords_pls5$x) + 0.1 &
                  y >= min(coords_pls5$y) - 0.1 & y <= max(coords_pls5$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls5)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat5, coords_pls5, select_ey.hat, pls5)

### Subset 6 ###

coords_ey.hat6 <- dplyr::filter(coords_ey.hat,
                                x >= min(coords_pls6$x) - 0.1 & x <= max(coords_pls6$x) + 0.1 &
                                  y >= min(coords_pls6$y) - 0.1 & y <= max(coords_pls6$y) + 0.1)

dists <- fields::rdist(coords_pls6, coords_ey.hat6)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls6$x) - 0.1 & x <= max(coords_pls6$x) + 0.1 &
                  y >= min(coords_pls6$y) - 0.1 & y <= max(coords_pls6$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls6)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat6, coords_pls6, select_ey.hat, pls6)

### Subset 7 ###

coords_ey.hat7 <- dplyr::filter(coords_ey.hat,
                                x >= min(coords_pls7$x) - 0.1 & x <= max(coords_pls7$x) + 0.1 &
                                  y >= min(coords_pls7$y) - 0.1 & y <= max(coords_pls7$y) + 0.1)

dists <- fields::rdist(coords_pls7, coords_ey.hat7)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls7$x) - 0.1 & x <= max(coords_pls7$x) + 0.1 &
                  y >= min(coords_pls7$y) - 0.1 & y <= max(coords_pls7$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls7)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat7, coords_pls7, select_ey.hat, pls7)

### Subset 8 ###

coords_ey.hat8 <- dplyr::filter(coords_ey.hat,
                                x >= min(coords_pls8$x) - 0.1 & x <= max(coords_pls8$x) + 0.1 &
                                  y >= min(coords_pls8$y) - 0.1 & y <= max(coords_pls8$y) + 0.1)

dists <- fields::rdist(coords_pls8, coords_ey.hat8)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls8$x) - 0.1 & x <= max(coords_pls8$x) + 0.1 &
                  y >= min(coords_pls8$y) - 0.1 & y <= max(coords_pls8$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls8)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat8, coords_pls8, select_ey.hat, pls8)

### Subset 9 ###

coords_ey.hat9 <- dplyr::filter(coords_ey.hat,
                                x >= min(coords_pls9$x) - 0.1 & x <= max(coords_pls9$x) + 0.1 &
                                  y >= min(coords_pls9$y) - 0.1 & y <= max(coords_pls9$y) + 0.1)

dists <- fields::rdist(coords_pls9, coords_ey.hat9)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls9$x) - 0.1 & x <= max(coords_pls9$x) + 0.1 &
                  y >= min(coords_pls9$y) - 0.1 & y <= max(coords_pls9$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls9)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat9, coords_pls9, select_ey.hat, pls9)

### Subset 10 ###

coords_ey.hat10 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls10$x) - 0.1 & x <= max(coords_pls10$x) + 0.1 &
                                   y >= min(coords_pls10$y) - 0.1 & y <= max(coords_pls10$y) + 0.1)

dists <- fields::rdist(coords_pls10, coords_ey.hat10)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls10$x) - 0.1 & x <= max(coords_pls10$x) + 0.1 &
                  y >= min(coords_pls10$y) - 0.1 & y <= max(coords_pls10$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls10)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat10, coords_pls10, select_ey.hat, pls10)

### Subset 11 ###

coords_ey.hat11 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls11$x) - 0.1 & x <= max(coords_pls11$x) + 0.1 &
                                   y >= min(coords_pls11$y) - 0.1 & y <= max(coords_pls11$y) + 0.1)

dists <- fields::rdist(coords_pls11, coords_ey.hat11)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls11$x) - 0.1 & x <= max(coords_pls11$x) + 0.1 &
                  y >= min(coords_pls11$y) - 0.1 & y <= max(coords_pls11$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls11)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat11, coords_pls11, select_ey.hat, pls11)

### Subset 12 ###

coords_ey.hat12 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls12$x) - 0.1 & x <= max(coords_pls12$x) + 0.1 &
                                   y >= min(coords_pls12$y) - 0.1 & y <= max(coords_pls12$y) + 0.1)

dists <- fields::rdist(coords_pls12, coords_ey.hat12)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls12$x) - 0.1 & x <= max(coords_pls12$x) + 0.1 &
                  y >= min(coords_pls12$y) - 0.1 & y <= max(coords_pls12$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls12)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat12, coords_pls12, select_ey.hat, pls12)

### Subset 13 ###

coords_ey.hat13 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls13$x) - 0.1 & x <= max(coords_pls13$x) + 0.1 &
                                   y >= min(coords_pls13$y) - 0.1 & y <= max(coords_pls13$y) + 0.1)

dists <- fields::rdist(coords_pls13, coords_ey.hat13)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls13$x) - 0.1 & x <= max(coords_pls13$x) + 0.1 &
                  y >= min(coords_pls13$y) - 0.1 & y <= max(coords_pls13$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls13)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat13, coords_pls13, select_ey.hat, pls13)

### Subset 14 ###

coords_ey.hat14 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls14$x) - 0.1 & x <= max(coords_pls14$x) + 0.1 &
                                   y >= min(coords_pls14$y) - 0.1 & y <= max(coords_pls14$y) + 0.1)

dists <- fields::rdist(coords_pls14, coords_ey.hat14)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls14$x) - 0.1 & x <= max(coords_pls14$x) + 0.1 &
                  y >= min(coords_pls14$y) - 0.1 & y <= max(coords_pls14$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls14)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat14, coords_pls14, select_ey.hat, pls14)

### Subset 15 ###

coords_ey.hat15 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls15$x) - 0.1 & x <= max(coords_pls15$x) + 0.1 &
                                   y >= min(coords_pls15$y) - 0.1 & y <= max(coords_pls15$y) + 0.1)

dists <- fields::rdist(coords_pls15, coords_ey.hat15)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls15$x) - 0.1 & x <= max(coords_pls15$x) + 0.1 &
                  y >= min(coords_pls15$y) - 0.1 & y <= max(coords_pls15$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls15)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat15, coords_pls15, select_ey.hat, pls15)

### Subset 16 ###

coords_ey.hat16 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls16$x) - 0.1 & x <= max(coords_pls16$x) + 0.1 &
                                   y >= min(coords_pls16$y) - 0.1 & y <= max(coords_pls16$y) + 0.1)

dists <- fields::rdist(coords_pls16, coords_ey.hat16)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls16$x) - 0.1 & x <= max(coords_pls16$x) + 0.1 &
                  y >= min(coords_pls16$y) - 0.1 & y <= max(coords_pls16$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls16)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat16, coords_pls16, select_ey.hat, pls16)

### Subset 17 ###

coords_ey.hat17 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls17$x) - 0.1 & x <= max(coords_pls17$x) + 0.1 &
                                   y >= min(coords_pls17$y) - 0.1 & y <= max(coords_pls17$y) + 0.1)

dists <- fields::rdist(coords_pls17, coords_ey.hat17)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls17$x) - 0.1 & x <= max(coords_pls17$x) + 0.1 &
                  y >= min(coords_pls17$y) - 0.1 & y <= max(coords_pls17$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls17)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat17, coords_pls17, select_ey.hat, pls17)

### Subset 18 ###

coords_ey.hat18 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls18$x) - 0.1 & x <= max(coords_pls18$x) + 0.1 &
                                   y >= min(coords_pls18$y) - 0.1 & y <= max(coords_pls18$y) + 0.1)

dists <- fields::rdist(coords_pls18, coords_ey.hat18)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls18$x) - 0.1 & x <= max(coords_pls18$x) + 0.1 &
                  y >= min(coords_pls18$y) - 0.1 & y <= max(coords_pls18$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls18)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat18, coords_pls18, select_ey.hat, pls18)

### Subset 19 ###

coords_ey.hat19 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls19$x) - 0.1 & x <= max(coords_pls19$x) + 0.1 &
                                   y >= min(coords_pls19$y) - 0.1 & y <= max(coords_pls19$y) + 0.1)

dists <- fields::rdist(coords_pls19, coords_ey.hat19)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls19$x) - 0.1 & x <= max(coords_pls19$x) + 0.1 &
                  y >= min(coords_pls19$y) - 0.1 & y <= max(coords_pls19$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls19)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat19, coords_pls19, select_ey.hat, pls19)

### Subset 20 ###

coords_ey.hat20 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls20$x) - 0.1 & x <= max(coords_pls20$x) + 0.1 &
                                   y >= min(coords_pls20$y) - 0.1 & y <= max(coords_pls20$y) + 0.1)

dists <- fields::rdist(coords_pls20, coords_ey.hat20)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls20$x) - 0.1 & x <= max(coords_pls20$x) + 0.1 &
                  y >= min(coords_pls20$y) - 0.1 & y <= max(coords_pls20$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls20)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat20, coords_pls20, select_ey.hat, pls20)

### Subset 21 ###

coords_ey.hat21 <- dplyr::filter(coords_ey.hat,
                                 x >= min(coords_pls21$x) - 0.1 & x <= max(coords_pls21$x) + 0.1 &
                                   y >= min(coords_pls21$y) - 0.1 & y <= max(coords_pls21$y) + 0.1)

dists <- fields::rdist(coords_pls21, coords_ey.hat21)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- ey.hat |>
  dplyr::filter(x >= min(coords_pls21$x) - 0.1 & x <= max(coords_pls21$x) + 0.1 &
                  y >= min(coords_pls21$y) - 0.1 & y <= max(coords_pls21$y) + 0.1) |>
  dplyr::slice(closest_point)

temp <- cbind(select_ey.hat, pls21)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_ey.hat21, coords_pls21, select_ey.hat, pls21)

# Update column names
colnames(clim_pls) <- c('time', 'aat', 'tpr', 'tsd', 'prsd', 'clim_x', 'clim_y', 'pls_x', 'pls_y', 'uniqueID')

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