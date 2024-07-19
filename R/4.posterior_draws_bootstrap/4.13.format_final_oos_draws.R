### 4-13

## Formatting final out-of-sample data with STEPPS posterior draws
## Taking full spatiotemporal extent of STEPPS posterior draws, soil, and
## climate reconstructions
## Removing in-sample data and saving the out-of-sample prediction domain

## Input: data/processed/post_STEPPS.RData
## Posterior draws from STEPPS

## Input: data/intermediate/stepps_post_subsampled.RData
## Posterior draws of relative abundances, but for a subset of spatio-
## temporal locations

rm(list = ls())

source('R/funs.R')

#### STEPPS data ####

# Load STEPPS data
load('data/processed/post_STEPPS.RData')

# Melt to dataframe
post_df <- reshape2::melt(post)

# Column names
colnames(post_df) <- c('ind', 'taxon', 'time', 'draw', 'val')

# Rescale factor
rescale <- 1e6

# Format
post_df <- post_df |>
  # Add coordinates
  dplyr::full_join(y = centers_veg, by =  'ind') |>
  # Fix coords
  dplyr::mutate(x = x * rescale,
                y = y * rescale) |>
  # Remove indexing
  dplyr::select(-ind) |>
  # Pivot wider
  tidyr::pivot_wider(names_from = 'taxon', values_from = 'val') |>
  # make coordinates characters to avoid problem with joining
  dplyr::mutate(x = as.character(x),
                y = as.character(y))

# Load formatted subsampled data
load('data/intermediate/stepps_post_subsampled.RData')

# Combine original in-sample and oos
post_insample <- rbind(post_insample, post_oos)

post_insample <- post_insample |>
  # add column to mark in-sample locations
  dplyr::mutate(insample = 'yes',
  # format x and y to avoid problem with joining
                x = as.character(x),
                y = as.character(y))

# Add locations that were in-sample to full dataframe
post_oos <- post_insample |>
  # keep only columns indexing spatiotemporal locations
  dplyr::select(x, y, time, draw, insample) |>
  # join with full data frame
  dplyr::full_join(y = post_df, by = c('x', 'y', 'time', 'draw')) |>
  # where insample is not yes, make it no
  dplyr::mutate(insample = dplyr::if_else(is.na(insample), 'no', insample)) |>
  # keep only locations not included in model fitting
  dplyr::filter(insample == 'no') |>
  # translate to point-type data by dropping NAs
  # (where no data exist)
  tidyr::drop_na() |>
  # remove insample indexing column
  dplyr::select(-insample)

#### Soil data ####

# Load PLS data because that's how we matched the soil grid
load('data/input/8km.RData')

# Take only coordinates and ID column
comp_dens <- dplyr::select(comp_dens, x, y, id)

# Add ID column from PLS dat to STEPPS data
# We can then join the soil and STEPPS data by the PLS grid ID
post_oos_id <- post_oos |>
  dplyr::mutate(x = as.integer(x),
                y = as.integer(y)) |>
  dplyr::left_join(y = comp_dens, by = c('x', 'y'))

# Load soil data
load('data/input/gridded_soil.RData')

# Rename coordinate columns to make sure we're on target later
soil_grid <- dplyr::rename(soil_grid,
                           soil_x = x,
                           soil_y = y)

# Join
post_oos_soil <- post_oos_id |>
  dplyr::rename(grid_id = id,
                stepps_x = x,
                stepps_y = y) |>
  dplyr::left_join(y = soil_grid, by = 'grid_id') |>
  # drop NAs in southwest corner of Minnesota
  tidyr::drop_na()

# Map of states
states <- map_states()

# Transform state map
states2 <- sf::st_transform(states, crs = 'EPSG:4326')

# Check and make sure the plots look the same regardless of coordinate system
p1 <- post_oos_soil |>
  dplyr::group_by(stepps_x, stepps_y, time) |>
  dplyr::summarize(sand = mean(sand)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = stepps_x, y = stepps_y, color = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
p1

p2 <- post_oos_soil |>
  dplyr::group_by(soil_x, soil_y, time) |>
  dplyr::summarize(sand = mean(sand)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = soil_x, y = soil_y, color = sand)) +
  ggplot2::geom_sf(data = states2, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
p2

cowplot::plot_grid(p1, p2)

# Now convert one set of coordinate sand make sure it still matches up
post_oos_soil <- sf::st_as_sf(post_oos_soil, coords = c('soil_x', 'soil_y'), crs = 'EPSG:4326')
post_oos_soil <- sf::st_transform(post_oos_soil, crs = 'EPSG:3175')
post_oos_soil <- sfheaders::sf_to_df(post_oos_soil, fill = TRUE)
post_oos_soil <- post_oos_soil |>
  dplyr::rename(soil_x = x,
                soil_y = y) |>
  dplyr::select(-sfg_id, -point_id)

# Plot x against x
post_oos_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = soil_x, y = stepps_x)) +
  ggplot2::geom_abline()

# same with y
post_oos_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = soil_y, y = stepps_y)) +
  ggplot2::geom_abline()

#### Climate data ####

# Load climate data
load('data/processed/gridded_climate.RData')

unbias_grid <- unbias_grid |> 
  dplyr::rename(clim_x = grid_x,
                clim_y = grid_y) |>
  dplyr::mutate(time = 1950 - time,
                time = time / 100)

# Join
post_oos_all <- post_oos_soil |>
  # remove time steps where we don't have climate data
  # and where we know there is anthropogenic influence
  dplyr::filter(!(time %in% c(20:21, 2))) |>
  dplyr::left_join(y = unbias_grid, by = c('grid_id', 'time'))
