### STEP 4-3

## Formatting for fitting GJAM with mean STEPPS relative abundances
## Requires subsampling in space and time to reduce correlations between
## adjacent estimates in space and time
## The optimal subsampling approach was determined using the files in 
## R/Testing_subsampling_methods/

## Input: data/processed/post_STEPPS.RData
## estimates of relative abundance from posterior draws of STEPPS

## Output: data/intermediate/stepps_post_subsampled.RData
## relative abundances estimates from posterior draws of STEPPS, but for a subset
## of spatio-temporal locations
## Used in 4.4.stepps_draws_soil_climate_formatting.R

rm(list = ls())

#### Subsampling in space ####

# Load data
load('data/processed/post_STEPPS.RData')

# Load helper functions
source('R/funs.R')

# Melt to dataframe
post_df <- reshape2::melt(post)

# Column names
colnames(post_df) <- c('ind', 'taxon', 'time', 'draw', 'val')

# Rescale factor
rescale <- 1e6

# Format
post_df <- post_df |>
  # Add coordinates
  dplyr::full_join(y = centers_veg, by = 'ind') |>
  # Fix coords
  dplyr::mutate(x = x * rescale,
                y = y * rescale) |>
  # Remove indexing
  dplyr::select(-ind) |>
  # Pivot wider
  tidyr::pivot_wider(names_from = 'taxon', values_from = 'val')

# Make map of study region
states <- map_states()

# Sample every 3 grid cells over all time periods
x_ind <- seq(from = 1, to = length(x), by = 3)
y_ind <- seq(from = 3, to = length(y), by = 3)

# Storage matrix
locs <- matrix(, nrow = length(x), ncol = length(y))

# Set cells we are keeping to TRUE
locs[x_ind, y_ind] <- TRUE

# Check to make sure we have the correct number
# of cells set to true
length(which(locs == TRUE)) == length(x_ind) * length(y_ind)

# Reorder y
y <- sort(y, decreasing = FALSE)

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

locs_melt_keep <- locs_melt |>
  dplyr::filter(keep == TRUE) |>
  dplyr::mutate(loc = paste0(x, '_', y))

# Subset the total taxon dataframe
post_filter <- post_df |>
  dplyr::mutate(loc = paste0(x, '_', y)) |>
  dplyr::select(-x, -y) |>
  dplyr::left_join(y = locs_melt_keep, by = 'loc') |>
  dplyr::filter(keep == TRUE)

#### Plotting spatial subsample ####

# Order of facets
time_order <- c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP', '1700 YBP',
                '1600 YBP', '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                '1100 YBP', '1000 YBP', '900 YBP', '800 YBP', '700 YBP',
                '600 YBP', '500 YBP', '400 YBP', '300 YBP', '200 YBP')

### ASH ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(ash = median(ASH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ash)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(ASH, probs = 0.025),
                   `25%` = quantile(ASH, probs = 0.25),
                   `50%` = median(ASH),
                   `75%` = quantile(ASH, probs = 0.75),
                   `97.5%` = quantile(ASH, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### BEECH ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(beech = median(BEECH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(BEECH, probs = 0.025),
                   `25%` = quantile(BEECH, probs = 0.25),
                   `50%` = median(BEECH),
                   `75%` = quantile(BEECH, probs = 0.75),
                   `97.5%` = quantile(BEECH, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### BIRCH ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(birch = median(BIRCH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(BIRCH, probs = 0.025),
                   `25%` = quantile(BIRCH, probs = 0.25),
                   `50%` = median(BIRCH),
                   `75%` = quantile(BIRCH, probs = 0.75),
                   `97.5%` = quantile(BIRCH, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### ELM ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(elm = median(ELM)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(ELM, probs = 0.025),
                   `25%` = quantile(ELM, probs = 0.25),
                   `50%` = median(ELM),
                   `75%` = quantile(ELM, probs = 0.75),
                   `97.5%` = quantile(ELM, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### HEMLOCK ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(hemlock = median(HEMLOCK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(HEMLOCK, probs = 0.025),
                   `25%` = quantile(HEMLOCK, probs = 0.25),
                   `50%` = median(HEMLOCK),
                   `75%` = quantile(HEMLOCK, probs = 0.75),
                   `97.5%` = quantile(HEMLOCK, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### MAPLE ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(maple = median(MAPLE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(MAPLE, probs = 0.025),
                   `25%` = quantile(MAPLE, probs = 0.25),
                   `50%` = median(MAPLE),
                   `75%` = quantile(MAPLE, probs = 0.75),
                   `97.5%` = quantile(MAPLE, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OAK ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(oak = median(OAK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(OAK, probs = 0.025),
                   `25%` = quantile(OAK, probs = 0.25),
                   `50%` = median(OAK),
                   `75%` = quantile(OAK, probs = 0.75),
                   `97.5%` = quantile(OAK, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OTHER CONIFER ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_conifer = median(OTHER.CONIFER)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(OTHER.CONIFER, probs = 0.025),
                   `25%` = quantile(OTHER.CONIFER, probs = 0.25),
                   `50%` = median(OTHER.CONIFER),
                   `75%` = quantile(OTHER.CONIFER, probs = 0.75),
                   `97.5%` = quantile(OTHER.CONIFER, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OTHER HARDWOOD ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_hardwood = median(OTHER.HARDWOOD)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(OTHER.HARDWOOD, probs = 0.025),
                   `25%` = quantile(OTHER.HARDWOOD, probs = 0.25),
                   `50%` = median(OTHER.HARDWOOD),
                   `75%` = quantile(OTHER.HARDWOOD, probs = 0.75),
                   `97.5%` = quantile(OTHER.HARDWOOD, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### PINE ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(pine = median(PINE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(PINE, probs = 0.025),
                   `25%` = quantile(PINE, probs = 0.25),
                   `50%` = median(PINE),
                   `75%` = quantile(PINE, probs = 0.75),
                   `97.5%` = quantile(PINE, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### SPRUCE ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(spruce = median(SPRUCE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(SPRUCE, probs = 0.025),
                   `25%` = quantile(SPRUCE, probs = 0.25),
                   `50%` = median(SPRUCE),
                   `75%` = quantile(SPRUCE, probs = 0.75),
                   `97.5%` = quantile(SPRUCE, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### TAMARACK ###

# Plot median over time
post_filter |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tamarack = median(TAMARACK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '2', '200 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '20', '2000 YBP', time),
                time = dplyr::if_else(time == '21', '2100 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot different quantiles at one time point
post_filter |>
  dplyr::filter(time == 11) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(TAMARACK, probs = 0.025),
                   `25%` = quantile(TAMARACK, probs = 0.25),
                   `50%` = median(TAMARACK),
                   `75%` = quantile(TAMARACK, probs = 0.75),
                   `97.5%` = quantile(TAMARACK, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'metric', values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', na.value = 'white',
                                direction = 1, name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### Temporal subsampling ####

## Now sample in time

# Sample times 4 apart
keep_times <- seq(from = 19, to = 3, by = -4)

# Subsample taxon data
post_filter <- post_filter |>
  dplyr::filter(time %in% keep_times)

# Separate in sample and out of sample data
post_insample <- post_filter |>
  dplyr::select(-keep, -loc) |>
  dplyr::filter(time %in% keep_times[1:4])
post_oos <- post_filter |>
  dplyr::select(-keep, -loc) |>
  dplyr::filter(time == keep_times[5])

#### Plotting temporal subsample ####

# Order for  facets
facet_order <- c('1900 YBP', '1500 YBP',
                 '1100 YBP', '700 YBP')

### ASH ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(ash = median(ASH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ash)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### BEECH ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(beech = median(BEECH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### BIRCH ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(birch = median(BIRCH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### ELM ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(elm = median(ELM)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### HEMLOCK ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(hemlock = median(HEMLOCK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### MAPLE ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(maple = median(MAPLE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('MAPLE') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OAK ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(oak = median(OAK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OTHER CONIFER ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_conifer = median(OTHER.CONIFER)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OTHER HARDWOOD ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_hardwood = median(OTHER.HARDWOOD)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### PINE ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(pine = median(PINE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### SPRUCE ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(spruce = median(SPRUCE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### TAMARACK ###

# Plot median of each taxon with reduced spatiotemporal domain
post_insample |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tamarack = median(TAMARACK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                name = 'Relative\nabundance',
                                na.value = 'white') +
  ggplot2::facet_wrap(~factor(time, levels = facet_order)) +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Save
save(post_insample, post_oos,
     file = 'data/intermediate/stepps_post_subsampled.RData')
