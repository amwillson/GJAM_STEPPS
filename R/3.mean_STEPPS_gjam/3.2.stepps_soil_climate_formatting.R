### STEP 3-2

## Adding soil and climate reconstructions to STEPPS subsampled mean relative abundances

## Input: data/intermediate/stepps_subsampled.RData
## Dataframe with subset of spatio-temporal locations

## Input: data/input/8km.RData
## Dataframe with grid ID and grid centroid lat/lon from PLS data
## This is required because the soil variable estimates were originally
## sampled using the PLS data

## Input: data/input/gridded_soil.RData
## Soil data that was previously processed for the PLS dataset
## In the "input" folder because this was processed for a different project,
## not processed for this project

## Input: data/processed/gridded_climate.RData
## Downscaled climate reconstructions
## In the "processed" folder because this was downscaled and processed
## for this project

## Output: data/intermediate/taxon_insample_soil.RData
## Output: data/intermediate/taxon_oos_soil.RData
## Dataframes with STEPPS mean relative abundances and soil variables with
## in-sample and out-of-sample data saved separately.
## Not used, just an intermediate data step

## Output: data/processed/mean_stepps_soil_clim.RData
## Dataframes with STEPPS mean relative abundances, soil variables, and climate variables
## in-sample and out-of-sample data saved in separate data frames
## Used in 3.3.run_gjam.R and 4.4.stepps_draws_soil_climate_formatting.R

rm(list = ls())

source('R/funs.R')

#### Soils ####

# Load STEPPS subsampled data from step 8
load('data/intermediate/stepps_subsampled.RData')

# Load PLS data because that's how we matched the soil grid
load('data/input/8km.RData')

# Take only coordinates and ID column
comp_dens <- dplyr::select(comp_dens, x, y, id)

# Add ID column from PLS data to STEPPS data
# We can then join the soil & STEPPS data by the PLS grid ID
taxon_insample_id <- taxon_insample |>
  dplyr::left_join(y = comp_dens, by = c('x', 'y'))
taxon_oos_id <- taxon_oos |>
  dplyr::left_join(y = comp_dens, by = c('x', 'y'))

# Load soil data
load('data/input/gridded_soil.RData')

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

#### Plots ####

# Order of facets
time_order <- c('1900 YBP', '1500 YBP', '1100 YBP', '700 YBP')

### Relative abundance ###

## ASH 

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = ash)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## BEECH

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## BIRCH

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## ELM

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## HEMLOCK

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## MAPLE

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## OAK

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER CONIFER

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER HARDWOOD

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## PINE

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## SPRUCE

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## TAMARACK

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, name = 'Relative\nabundance') +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

### Soil covariates ###

## CLAY

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = clay)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, name = 'Soil % clay') +
  ggplot2::ggtitle('Soil % Clay') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## SAND

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, name = 'Soil % sand') +
  ggplot2::ggtitle('Soil % Sand') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## SILT

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = silt)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, name = 'Soil % silt') +
  ggplot2::ggtitle('Soil % Silt') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## CACO3

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = caco3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, name = '[CaCO3]') +
  ggplot2::ggtitle('[CaCO3]') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## AWC

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = awc)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, name = 'AWC') +
  ggplot2::ggtitle('Available Water Content') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

### Climate covariates ###

## AAT

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'F', name = 'AAT') +
  ggplot2::ggtitle('Average Annual Temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## TPR

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'G', name = 'TPR') +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## TSD

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'D', name = 'TSD') +
  ggplot2::ggtitle('Temperature seasonality') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## PRSD

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'E', name = 'PRSD') +
  ggplot2::ggtitle('Precipitation seasonality') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

# Save
save(taxon_insample_all, taxon_oos_all,
     file = 'data/processed/mean_stepps_soil_clim.RData')
