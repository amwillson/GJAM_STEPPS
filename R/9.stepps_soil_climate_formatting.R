## Adding soil and climate  data to STEPPS subsampled data
rm(list = ls())

source('R/funs.R')

#### Soils ####

# Load STEPPS subsampled data from step 8
load('data/intermediate/stepps_subsampled.RData')

# Load PLS data because that's how we matched the soil grid
load('data/processed/8km.RData')

# Take only coordinates and ID column
comp_dens <- dplyr::select(comp_dens, x, y, id)

# Add ID column from PLS data to STEPPS data
# We can then join the soil & STEPPS data by the PLS grid ID
taxon_insample_id <- taxon_insample |>
  dplyr::left_join(y = comp_dens, by = c('x', 'y'))
taxon_oos_id <- taxon_oos |>
  dplyr::left_join(y = comp_dens, by = c('x', 'y'))

# Load soil data
load('data/processed/gridded_soil.RData')

# Rename coordinate columns to amke sure we're on target later
soil_grid <- dplyr::rename(soil_grid, 
                           soil_x = x,
                           soil_y = y)

# Join
taxon_insample_soil <- taxon_insample_id |>
  dplyr::rename(grid_id = id,
                stepps_x = x,
                stepps_y = y) |>
  dplyr::left_join(y = soil_grid, by = 'grid_id')
taxon_oos_soil <- taxon_oos_id |>
  dplyr::rename(grid_id = id,
                stepps_x = x,
                stepps_y = y) |>
  dplyr::left_join(y = soil_grid, by = 'grid_id')

states <- map_states()

# Transform state map
states2 <- sf::st_transform(states, crs = 'EPSG:4326')

# Check to make sure the plots look the same regardless of coordinate system
# and make sense
p1 <- taxon_insample_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = stepps_x, y = stepps_y, color = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
p1

p2 <- taxon_insample_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = soil_x, y = soil_y, color = sand)) +
  ggplot2::geom_sf(data = states2, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
p2

cowplot::plot_grid(p1, p2)

p1 <- taxon_oos_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = stepps_x, y = stepps_y, color = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
p1

p2 <- taxon_oos_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = soil_x, y = soil_y, color = sand)) +
  ggplot2::geom_sf(data = states2, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
p2

cowplot::plot_grid(p1, p2)

# Now convert one set of coordinates and make sure it still matches up
taxon_insample_soil <- sf::st_as_sf(taxon_insample_soil, coords = c('soil_x', 'soil_y'), crs = 'EPSG:4326')
taxon_insample_soil <- sf::st_transform(taxon_insample_soil, crs = 'EPSG:3175')
taxon_insample_soil <- sfheaders::sf_to_df(taxon_insample_soil, fill = TRUE)
taxon_insample_soil <- taxon_insample_soil |>
  dplyr::rename(soil_x = x,
                soil_y = y) |>
  dplyr::select(-sfg_id, -point_id)

taxon_oos_soil <- sf::st_as_sf(taxon_oos_soil, coords = c('soil_x', 'soil_y'), crs = 'EPSG:4326')
taxon_oos_soil <- sf::st_transform(taxon_oos_soil, crs = 'EPSG:3175')
taxon_oos_soil <- sfheaders::sf_to_df(taxon_oos_soil, fill = TRUE)
taxon_oos_soil <- taxon_oos_soil |>
  dplyr::rename(soil_x = x,
                soil_y = y) |>
  dplyr::select(-sfg_id, -point_id)

# Plot x against x
taxon_insample_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = soil_x, y = stepps_x)) +
  ggplot2::geom_abline()
taxon_oos_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = soil_x, y = stepps_x)) +
  ggplot2::geom_abline()

# same with y
taxon_insample_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = soil_y , stepps_y)) +
  ggplot2::geom_abline()
taxon_oos_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = soil_y, stepps_y)) +
  ggplot2::geom_abline()

# Remove & rearrange columns
taxon_insample_soil <- dplyr::select(taxon_insample_soil,
                                     stepps_x, stepps_y, time, grid_id,
                                     ash:tamarack,
                                     clay:awc)
taxon_oos_soil <- dplyr::select(taxon_oos_soil,
                                stepps_x, stepps_y, time, grid_id,
                                ash:tamarack,
                                clay:awc)

# Save
save(taxon_insample_soil, file = 'data/intermediate/taxon_insample_soil.RData')
save(taxon_oos_soil, file = 'data/intermediate/taxon_oos_soil.RData')

#### Climate ####

# Load climate data
load('data/processed/gridded_climate.RData')

# Rename coordinate columns to make sure we're on target later
unbias_grid <- dplyr::rename(unbias_grid,
                             clim_x = grid_x,
                             clim_y = grid_y)

# Convert time in climate dataset to YBP
unbias_grid <- unbias_grid |>
  dplyr::mutate(time = 1950 - time,
                time = time / 100)

# Join
taxon_insample_all <- taxon_insample_soil |>
  dplyr::left_join(y = unbias_grid, by = c('grid_id', 'time'))
taxon_oos_all <- taxon_oos_soil |>
  dplyr::left_join(y = unbias_grid, by = c('grid_id', 'time'))

states <- map_states()

# Check to make sure the plots look the same regardless of coordinate set
# and make sense
p1 <- taxon_insample_all |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = stepps_x, y = stepps_y, color = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
p1

p2 <- taxon_insample_all |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clim_x, y = clim_y, color = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
p2

cowplot::plot_grid(p1, p2)

p1 <- taxon_oos_all |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = stepps_x, y = stepps_y, color = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
p1

p2 <- taxon_oos_all |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clim_x, y = clim_y, color = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
p2

cowplot::plot_grid(p1, p2)

# Plot x against x
taxon_insample_all |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clim_x, y = stepps_x)) +
  ggplot2::geom_abline()
taxon_oos_all |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clim_x, y = stepps_x)) +
  ggplot2::geom_abline()

# Same with y
taxon_insample_all |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clim_y, y = stepps_y)) +
  ggplot2::geom_abline()
taxon_oos_all |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = clim_y, y = stepps_y)) +
  ggplot2::geom_abline()

# Remove and rearrange columns
taxon_insample_all <- dplyr::select(taxon_insample_all,
                                    -grid_id, -clim_x, -clim_y)
taxon_oos_all <- dplyr::select(taxon_oos_all,
                               -grid_id, -clim_x, -clim_y)

# Save
save(taxon_insample_all, taxon_oos_all,
     file = 'data/processed/mean_stepps_soil_clim.RData')
