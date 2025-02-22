### STEP 4-11

## Formatting final out-of-sample data with STEPPS posterior draws
## Taking full spatiotemporal extent of STEPPS posterior draws, soil, and
## climate reconstructions
## Removing in-sample data and saving the out-of-sample prediction domain

## Input: data/processed/post_STEPPS.RData
## Posterior draws from STEPPS

## Input: data/intermediate/stepps_post_subsampled.RData
## Posterior draws of relative abundances, but for a subset of spatio-
## temporal locations

## Input: data/processed/mean_stepps_full_oos.RData
## Dataframe with climate and soil data for full spatiotemporal domain
## from doing the same procedure with the mean relative abundances

## Output: data/processed/post_stepps_full_oos.RData
## Dataframe with posterior draws from STEPPS for all out-of-sample
## spatiotemporal locations, as well as corresponding climate and soil
## reconstructions
## Used in 4.12.oos_prediction_draws_final.R and 4.13.plot_oos_prediction_draws_final.R

rm(list = ls())

# Load helper functions
source('R/funs.R')

#### STEPPS data ####

# Load STEPPS data
load('data/processed/post_STEPPS.RData')

# Melt to dataframe
post_df <- reshape2::melt(post)

# Column names
colnames(post_df) <- c('ind', 'taxon', 'time', 'draw', 'val')

# Rescale coordinates
# Just a scalar to multiply cooridnates by to get
# actual coordinates
rescale <- 1e6

# Format
post_df <- post_df |>
  # Add coordinates by joining with centers_veg
  # We know that the grid cells are in the same order
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

# Remove missing data (these are out-of-sample locations)
post_insample <- tidyr::drop_na(post_insample)

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
  # remove insample indexing column
  dplyr::select(-insample)

#### Combine data ####

# Load formatted mean with climate and soil data
load('data/processed/mean_stepps_full_oos.RData')

soil_clim_oos <- dplyr::select(taxon_oos_all, c(x:time, clay:prsd))

# Remove mean dfs
rm(taxon_oos_all)

# Combine by time and coordinates
post_oos_all <- post_oos |>
  dplyr::mutate(x = as.integer(x),
                y = as.integer(y)) |>
  dplyr::full_join(y = soil_clim_oos,
                   by = c('x', 'y', 'time')) |>
  # remove time steps where we don't have climate data
  # and where we know there is anthropogenic influence
  dplyr::filter(!(time %in% c(20:21, 2))) |>
  # drop NAs in southwest corner of Minnesota
  tidyr::drop_na()

#### Plotting checks ####

# Map of study region
states <- map_states()

# Order of facets
time_order <- c('1900 YBP', '1800 YBP', '1700 YBP', '1600 YBP',
                '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                '1100 YBP', '1000 YBP', '900 YBP', '800 YBP',
                '700 YBP', '600 YBP', '500 YBP', '400 YBP', '300 YBP')

## Plot covariates

# These are more production-level plots of response variables
# and covariates, rather than ones meant for checks

### Soil covariates ###

### CLAY ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(clay = median(clay)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = clay)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, 
                                name = '% clay',
                                na.value = '#00000000',
                                limits = c(0, 100)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil % clay') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### SAND ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(sand = median(sand)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, 
                                name = '% sand',
                                na.value = '#00000000',
                                limits = c(0, 100)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil % sand') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### SILT ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(silt = median(silt)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = silt)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, 
                                name = '% silt',
                                na.value = '#00000000',
                                limits = c(0, 100)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil % silt') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### CACO3 ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(caco3 = median(caco3)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = caco3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1,
                                name = expression(paste('% CaC', O[3])),
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil calcium carbonate concentration') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### AWC ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(awc = median(awc)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = awc)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1,
                                 name = 'cm/cm',
                                 na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil available water content') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### Climate covariates ###

### AAT ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(aat = median(aat)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'F', 
                                name = '°C',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### TPR ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tpr = median(tpr)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'G', 
                                name = 'mm/year',
                                na.value = '#00000000',
                                direction = -1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### TSD ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tsd = median(tsd)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'D', 
                                name = '°C',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Temperature seasonality (standard deviation)') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### PRSD ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(prsd = median(prsd)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'E', 
                                name = 'mm/year',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Precipitation seasonality (standard deviation)') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### Relative abundance ###

### ASH ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(ash = median(ASH)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ash)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Ash (', italic('Fraxinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

pdf(file = 'figures/data/full_oos_posterior_abundances.pdf',
    width = 8, height = 10.5)

### BEECH ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(beech = median(BEECH)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### BIRCH ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(birch = median(BIRCH)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### ELM ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(elm = median(ELM)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### HEMLOCK ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(hemlock = median(HEMLOCK)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### MAPLE ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(maple = median(MAPLE)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OAK ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(oak = median(OAK)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OTHER CONIFER ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_conifer = median(OTHER.CONIFER)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5))

### OTHER HARDWOOD ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_hardwood = median(OTHER.HARDWOOD)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5))

### PINE ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(pine = median(PINE)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic ('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### SPRUCE ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(spruce = median(SPRUCE)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### TAMARACK ###

post_oos_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tamarack = median(TAMARACK)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Fraction total\nstems',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

dev.off()

# Save
save(post_oos_all,
     file = 'data/processed/post_stepps_full_oos.RData')
