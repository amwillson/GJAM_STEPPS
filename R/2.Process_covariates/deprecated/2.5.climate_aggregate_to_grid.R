### STEP 2-5

## Third part of spatially matching climate to PLS
## Aggregates climate reconstructions to STEPPS grid cell size
## Then estimates "empty" grid cells. Empty grid cells happen
## because some grid cells have no PLS points. This is because of large
## Native American reservations and islands. We have climate reconstructions for
## these locations, though, so we simply find and add the climate reconstructions
## for this subsest of locations.
## NOTE: This part should be run on local machine
## Only step 2-4 should be run on the VM
## because other steps should use the most recent version of R (4.4.0)

## Input: data/intermediate/point_matched_clim.RData
## Matching between points and grid cells in the PLS data. Tells us which
## climate reconstruction grid cells correspond to which STEPPS grid cells

## Input: data/intermediate/clipped_clim_alltime.RData
## All climate reconstructions for the more limited spatial extent of our region
## of interest. All time points, which means there are repeated spatial locations.

## Input: data/input/total_matched.RData
## Dataframe with the mapping from PLS points to PLS grid cells
## Tells us which grid cell each PLS point falls within. We use this to
## determine which STEPPS grid cell each climate grid cell falls within, based
## on which PLS point each climate grid cell is closest to

## Input: data/intermediate/matching_intermediate.RData
## Intermediate data product with match between grid cell ID and lat/lon for PLS
## Use this to add grid cell centroid coordinates to climate reconstruction
## grid cells after aggregating to the STEPPS grid cell

## Input: data/input/8km.RData
## Used to identify which grid cells have no PLS points, so we can
## manually identify climate reconstruction points that are within those grid cells

## Output: data/processed/climate_gridded.RData
## data frame with gridded climate reconstructions at the spatial and temporal
## scales of the STEPPS data product
## Used in 3.2.stepps_soil_climate_formatting.R

rm(list = ls())

# Load helper functions
source('R/funs.R')

# Load intermediate step
load('data/intermediate/point_matched_clim.RData')
load('data/intermediate/clipped_clim_alltime.RData')

# Unique time steps
time <- unique(ey.hat$time)

# Make dataframe of just spatial mapping info
spat_map <- dplyr::select(clim_pls, spatID:uniqueID)

# Repeat dataframe for each time step
# We know the spatial information is the exact same for each
# time step because we verified this in an earlier step
spat_map <- do.call('rbind', replicate(n = length(time), spat_map, simplify = FALSE))

# Add time to dataframe
spat_map$time <- rep(time, each = nrow(clim_pls))

# Add climate data using spatial mapping
ey.hat_pls_full <- dplyr::right_join(x = ey.hat,
                                     y = spat_map,
                                     by = c('time', 'spatID'))
unbias_pls_full <- dplyr::right_join(x = unbias,
                                     y = spat_map,
                                     by = c('time', 'spatID'))

# Map of study region
states <- map_states()

# Plot to make sure it looks consistent
# We want to see the correct spatial extent and 
# correct spatial patterns of climate
p1 <- ey.hat_pls_full |>
  dplyr::filter(time == 850) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
p2 <- unbias_pls_full |>
  dplyr::filter(time == 850) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(p1, p2, nrow = 1)

# Transform CRS for state maps
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Make sure that using the other set of coordinates also looks good
# Arbitrarily plotting a different variable to make sure spatial
# patterns still look good
p1 <- ey.hat_pls_full |>
  dplyr::filter(time == 250) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clim_x, y = clim_y, color = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
p2 <- unbias_pls_full |>
  dplyr::filter(time == 250) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clim_x, y = clim_y, color = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(p1, p2, nrow = 1)

# One more random variable to check final coordinate set
p1 <- ey.hat_pls_full |>
  dplyr::filter(time == 1550) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
p2 <- unbias_pls_full |>
  dplyr::filter(time == 1550) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(p1, p2, nrow = 1)

#### Aggregate points to PLS grid ####

# Load point and grid matched PLS data
# Maps from points to grid
load('data/input/total_matched.RData')

# Add grid ID to climate data
ey.hat_matched <- ecosystem_matched |>
  dplyr::select(grid_id, x, y) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y  = ey.hat_pls_full, by = c('pls_x', 'pls_y'))
unbias_matched <- ecosystem_matched |>
  dplyr::select(grid_id, x, y) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = unbias_pls_full, by = c('pls_x', 'pls_y'))

# Summarize for each grid cell at each time
# Averaging over all points within a grid cell
ey.hat_grid <- ey.hat_matched |>
  dplyr::group_by(time, grid_id) |>
  dplyr::summarize(aat = mean(aat),
                   tpr = mean(tpr),
                   tsd = mean(tsd),
                   prsd = mean(prsd),
                   prcv = mean(prcv))
unbias_grid <- unbias_matched |>
  dplyr::group_by(time, grid_id) |>
  dplyr::summarize(aat = mean(aat),
                   tpr = mean(tpr),
                   tsd = mean(tsd),
                   prsd = mean(prsd),
                   prcv = mean(prcv))

# Load intermediate grid matching
load('data/intermediate/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to climate data frame
ey.hat_grid <- ey.hat_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')
unbias_grid <- unbias_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Transform CRS for state map
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Plot gridded and point-level climate
# Make sure patterns look the same
grid <- ey.hat_grid |>
  dplyr::filter(time == 850) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- ey.hat_pls_full |>
  dplyr::filter(time == 850) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

# Same for debiased estimates
grid <- unbias_grid |>
  dplyr::filter(time == 850) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = grid_x, y = grid_y, color = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- unbias_pls_full |>
  dplyr::filter(time == 850) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

#### Estimate empty grid cells ####

# All grid cells across domain
grid <- unique(ey.hat_grid$grid_id)

# Gridded PLS data products
load('data/input/8km.RData')

# Make spatial object
comp_dens <- sf::st_as_sf(comp_dens,
                          coords = c('x', 'y'),
                          crs = 'EPSG:3175')

# Clip to extent of region of interest
comp_dens <- sf::st_intersection(comp_dens, states)

# Convert back to data frame
comp_dens <- sfheaders::sf_to_df(comp_dens, fill = TRUE)

# Remove unnecessary columns
comp_dens <- dplyr::select(comp_dens, id, x, y)

# Which grid cells have no points in them?
nopoints <- comp_dens[!comp_dens$id %in% grid,]
nopoints <- dplyr::distinct(nopoints)

# Plot locations of grid cells with no points
# Want to verify that grid cells with no points
# are in locations where there were no PLS data
nopoints |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Find extent of each grid cell with no points
nopoints <- nopoints |>
  dplyr::mutate(x_min = x - 4000,
                x_max = x + 4000,
                y_min = y - 4000,
                y_max = y + 4000)

# Loop over grid cells that don't have points
for(i in 1:nrow(nopoints)){
  # Grid cell
  p <- nopoints[i,]
  # Climate data points that are within that grid cell
  sub_ey.hat <- ey.hat |>
    dplyr::filter(x >= p$x_min & x <= p$x_max &
                    y >= p$y_min & y <= p$y_max)
  sub_unbias <- unbias |>
    dplyr::filter(x >= p$x_min & x <= p$x_max &
                    y >= p$y_min & y <= p$y_max)
  
  # Average over all points within grid cell
  summ_ey.hat <- sub_ey.hat |>
    dplyr::group_by(time) |>
    dplyr::summarize(aat = mean(aat),
                     tpr = mean(tpr),
                     tsd = mean(tsd),
                     prsd = mean(prsd),
                     prcv = mean(prcv))
  summ_unbias <- sub_unbias |>
    dplyr::group_by(time) |>
    dplyr::summarize(aat = mean(aat),
                     tpr = mean(tpr),
                     tsd = mean(tsd),
                     prsd = mean(prsd),
                     prcv = mean(prcv))
  
  # Save
  if(i == 1){
    summ_xy_ey.hat <- cbind(rep(p$x, times = nrow(summ_ey.hat)), 
                            rep(p$y, times = nrow(summ_ey.hat)),
                            rep(p$id, times = nrow(summ_ey.hat)),
                            summ_ey.hat)
    summ_xy_unbias <- cbind(rep(p$x, times = nrow(summ_unbias)),
                            rep(p$y, times = nrow(summ_unbias)),
                            rep(p$id, times = nrow(summ_unbias)),
                            summ_unbias)
  }else{
    temp_ey.hat <- cbind(rep(p$x, times = nrow(summ_ey.hat)),
                         rep(p$y, times = nrow(summ_ey.hat)),
                         rep(p$id, times = nrow(summ_ey.hat)),
                         summ_ey.hat)
    temp_unbias <- cbind(rep(p$x, times = nrow(summ_unbias)),
                         rep(p$y, times = nrow(summ_unbias)),
                         rep(p$id, times = nrow(summ_unbias)),
                         summ_unbias)
    
    summ_xy_ey.hat <- rbind(summ_xy_ey.hat, temp_ey.hat)
    summ_xy_unbias <- rbind(summ_xy_unbias, temp_unbias)
  }
  # Progress
  print(i)
}

# Change column names
colnames(summ_xy_ey.hat) <- c('x', 'y', 'id', 'time', 'aat', 'tpr', 'tsd', 'prsd', 'prcv')
colnames(summ_xy_unbias) <- colnames(summ_xy_ey.hat)

# Identify grid cells still missing
nopoints <- nopoints[which(!(nopoints$id %in% summ_xy_ey.hat$id)),]

# Loop over still missing grid cells
for(i in 1:nrow(nopoints)){
  # Grid cell
  p <- nopoints[i,]
  # Grid cell x and y coordinates
  p_xy <- c(p$x, p$y)
  # Newly estimated grid cells
  ey.hat_xy <- summ_xy_ey.hat |>
    dplyr::filter(time == 50) |>
    dplyr::select(x, y)
  unbias_xy <- summ_xy_unbias |>
    dplyr::filter(time == 50) |>
    dplyr::select(x, y)

  # Find distance between the grid cell of interest and the grid cells we just estimated
  closest_gridcells_ey.hat <- fields::rdist(p_xy, ey.hat_xy)
  closest_gridcells_unbias <- fields::rdist(p_xy, unbias_xy)
  
  # Find the closest grid cell to the one lacking data
  closest_gridcell_ey.hat <- apply(closest_gridcells_ey.hat, 1, which.min)
  closest_gridcell_unbias <- apply(closest_gridcells_unbias, 1, which.min)
  
  # Take first element
  # Manually verified that taking the first is correct
  closest_gridcell_ey.hat <- closest_gridcell_ey.hat[1]
  closest_gridcell_unbias <- closest_gridcell_unbias[1]
  
  # Grid id
  closest_gridid_ey.hat <- summ_xy_ey.hat$id[closest_gridcell_ey.hat]
  closest_gridid_unbias <- summ_xy_unbias$id[closest_gridcell_unbias]
  
  # Get climate data for closest grid cell
  closest_ey.hat <- dplyr::filter(summ_xy_ey.hat, id == closest_gridid_ey.hat)
  closest_unbias <- dplyr::filter(summ_xy_unbias, id == closest_gridid_unbias)

  # Format
  temp_ey.hat <- cbind(rep(p$x, times = nrow(closest_ey.hat)),
                       rep(p$y, times = nrow(closest_ey.hat)),
                       rep(p$id, times = nrow(closest_ey.hat)),
                       dplyr::select(closest_ey.hat, time:prcv))
  temp_unbias <- cbind(rep(p$x, times = nrow(closest_unbias)),
                       rep(p$y, times = nrow(closest_unbias)),
                       rep(p$id, times = nrow(closest_unbias)),
                       dplyr::select(closest_unbias, time:prcv))

  colnames(temp_ey.hat) <- c('x', 'y', 'id', 'time', 'aat', 'tpr', 'tsd', 'prsd', 'prcv')
  colnames(temp_unbias) <- colnames(temp_ey.hat)
  
  # Combine with previous data frames
  summ_xy_ey.hat <- rbind(summ_xy_ey.hat, temp_ey.hat)
  summ_xy_unbias <- rbind(summ_xy_unbias, temp_unbias)
}

# Reformat
summ_xy_ey.hat <- summ_xy_ey.hat |>
  dplyr::rename(grid_id = id,
                grid_x = x,
                grid_y = y) |>
  dplyr::select(colnames(ey.hat_grid))
summ_xy_unbias <- summ_xy_unbias |>
  dplyr::rename(grid_id = id,
                grid_x = x,
                grid_y = y) |>
  dplyr::select(colnames(unbias_grid))

# Combine
ey.hat_grid <- rbind(ey.hat_grid, summ_xy_ey.hat)
unbias_grid <- rbind(unbias_grid, summ_xy_unbias)

# Save
save(ey.hat_grid, unbias_grid,
     file = 'data/processed/gridded_climate.RData')
