## Formatting for mean GJAM run
## This serves as a check to make sure all steps are correct
## before automating to iterate over posterior samples

rm(list = ls())

# Load data
load('data/processed/mean_STEPPS.RData')

# Load helper functions
source('R/funs.R')

# Melt arrays to data frames
ash_melt <- melt_array(taxon_mat = ash, x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'ash'))
beech_melt <- melt_array(taxon_mat = beech, x = x, y = y, time = time,
                         col_names = c('x', 'y', 'time', 'beech'))
birch_melt <- melt_array(taxon_mat = birch, x = x, y = y, time = time,
                         col_names = c('x', 'y', 'time', 'birch'))
elm_melt <- melt_array(taxon_mat = elm, x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'elm'))
hemlock_melt <- melt_array(taxon_mat = hemlock, x = x, y = y, time = time,
                           col_names = c('x', 'y', 'time', 'hemlock'))
maple_melt <- melt_array(taxon_mat = maple, x = x, y = y, time = time,
                         col_names = c('x', 'y', 'time', 'maple'))
oak_melt <- melt_array(taxon_mat = oak, x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'oak'))
other_conifer_melt <- melt_array(taxon_mat = other_conifer, x = x, y = y, time = time,
                                 col_names = c('x', 'y', 'time', 'other_conifer'))
other_hardwood_melt <- melt_array(taxon_mat = other_hardwood, x = x, y = y, time = time,
                                  col_names = c('x', 'y', 'time', 'other_hardwood'))
pine_melt <- melt_array(taxon_mat = pine, x = x, y = y, time = time,
                        col_names = c('x', 'y', 'time', 'pine'))
spruce_melt <- melt_array(taxon_mat = spruce, x = x, y = y, time = time,
                          col_names = c('x', 'y', 'time', 'spruce'))
tamarack_melt <- melt_array(taxon_mat = tamarack, x = x, y = y, time = time,
                            col_names = c('x', 'y', 'time', 'tamarack'))

# Combine taxa  into one dataframe
taxon_melt <- ash_melt |>
  dplyr::left_join(y = beech_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = birch_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = elm_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = hemlock_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = maple_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = oak_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = other_conifer_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = other_hardwood_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = pine_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = spruce_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = tamarack_melt, by = c('x', 'y', 'time'))

# Make map of study region
states <- map_states()

# Sample every 3 grid cells over all time periods
# Start at x = 1, y = 3 for best coverage

# Sample every 3rd x and y grid cell index
x_ind <- seq(from = 1, to = length(x), by = 3)
y_ind <- seq(from = 3, to = length(y), by = 3)

# Storage matrix
locs <- matrix(, nrow = length(x), ncol = length(y))

# Set cells we are keeping to TRUE
locs[x_ind, y_ind] <- TRUE

# Check to make sure we have the correct number
# of cells set to true
length(which(locs == TRUE)) == length(x_ind) * length(y_ind)

# Add x and y coordinates as dimension names
dimnames(locs) <- list(x, y)
# Melt to dataframe
locs_melt <- reshape2::melt(locs)
# Add column names
colnames(locs_melt) <- c('x', 'y', 'keep')
# Format
locs_melt <- dplyr::mutate(locs_melt,
                           keep = dplyr::if_else(is.na(keep), FALSE, keep))

# Plot locations we are going to keep to make sure it looks good
locs_melt |>
  dplyr::filter(keep == TRUE) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y), color = 'darkblue') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Subset the total taxon dataframe
taxon_filter <- taxon_melt |>
  dplyr::left_join(y = locs_melt, by = c('x', 'y')) |>
  dplyr::filter(keep == TRUE)

# Plot each taxon with filtered grid
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = ash)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Ash',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Beech',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Birch',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Elm',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Hemlock',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Maple',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Oak',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Other conifer',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Other hardwood',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Pine',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Spruce',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()
taxon_filter |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Tamarack',
                                na.value = 'white') +
  ggplot2::facet_wrap(~time) +
  ggplot2::theme_void()

## Now sample in time

# Sample times 4 apart
keep_times <- seq(from = 19, to = 3, by = -4)

# Subsample taxon data
taxon_filter <- taxon_filter |>
  dplyr::filter(time %in% keep_times)

# Separate in sample and out of sample data
taxon_insample <- taxon_filter |>
  dplyr::select(-keep) |>
  dplyr::filter(time %in% keep_times[1:4])
taxon_oos <- taxon_filter |>
  dplyr::select(-keep) |>
  dplyr::filter(time == keep_times[5])

# Order for facets
facet_order <- c('1900 YBP', '1500 YBP', 
                 '1100 YBP', '700 YBP')

# Plot each taxon with reduced spatiotemporal domain
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = ash)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Ash',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Beech',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Birch',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Elm',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Hemlock',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Maple',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Oak',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Other conifer',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Other hardwood',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Pine',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Spruce',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()
taxon_insample |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Tamarack',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, facet_order)) +
  ggplot2::theme_void()

# Translate to point-type data by dropping NAs
# (where no data exist)
taxon_insample <- taxon_insample |>
  tidyr::drop_na()
taxon_oos <- taxon_oos |>
  tidyr::drop_na()

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
soil_grid <- dplyr::rename(soil_grid, soil_x = x,
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

## Everything is correct
## We wait for climate data now to do the same

# Save
save(taxon_insample_soil, file = 'data/processed/taxon_insample_soil.RData')
save(taxon_oos_soil, file = 'data/processed/taxon_oos_soil.RData')
