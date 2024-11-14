### STEP 1-4

## Plotting relative abundance estimates from posterior draws of STEPPS over space and time

## Input: data/processed/post_STEPPS.RData
## relative abundances in array format

## Output: none

rm(list = ls())

# Load helper functions
source('R/funs.R')

# Load posterior draws
load('data/processed/post_STEPPS.RData')

# Melt to dataframe
post_df <- reshape2::melt(post)

# Column names
colnames(post_df) <- c('ind', 'taxon', 'time', 'draw', 'val')

# Rescale factor
# Just a scalar to multiply coordinates by to get actual coordinates
# The x and y vectors are already scaled, but if we're joining
# using centers_veg, that x and y is not scaled
rescale <- 1e6

# Format
post_df <- post_df |>
  # Add coordinates by joining with centers_veg
  # We know that the grid cells are in the same order
  dplyr::full_join(y = centers_veg, by = 'ind') |>
  # Fix coords
  dplyr::mutate(x = x * rescale,
                y = y * rescale) |>
  # Remove indexing
  dplyr::select(-ind) |>
  # Pivot wider
  tidyr::pivot_wider(names_from = 'taxon', values_from = 'val')

# Map of study region
states <- map_states()

#### Spatiotemporal plots ####

# Order of facets
time_order <- c('1900 YBP', '1800 YBP', '1700 YBP',
                '1600 YBP', '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                '1100 YBP', '1000 YBP', '900 YBP', '800 YBP', '700 YBP',
                '600 YBP', '500 YBP', '400 YBP', '300 YBP')

### ASH ###

# Plot median of posterior draws over entire spatiotemporal domain
# Analogous to plots in 1.2.Plot_STEPPS_mean.R
# This is just to look at median of the posterior draws and see what 
# the data look like
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(ash = median(ASH)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ash)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Ash (', italic('Fraxinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_ash.png',
                width = 7, height = 7, units = 'in')

### BEECH ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(beech = median(BEECH)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = 'grey85',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_beech.png',
                width = 7, height = 7, units = 'in')

### BIRCH ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(birch = median(BIRCH)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_birch.png',
                width = 7, height = 7, units = 'in')

### ELM ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(elm = median(ELM)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_elm.png',
                width = 7, height = 7, units = 'in')

### HEMLOCK ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(hemlock = median(HEMLOCK)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_hemlock.png',
                width = 7, height = 7, units = 'in')

### MAPLE ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(maple = median(MAPLE)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_maple.png',
                width = 7, height = 7, units = 'in')

### OAK ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(oak = median(OAK)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_oak.png',
                width = 7, height = 7, units = 'in')

### OTHER CONIFER ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_conifer = median(OTHER.CONIFER)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_oc.png',
                width = 7, height = 7, units = 'in')

### OTHER HARDWOOD ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_hardwood = median(OTHER.HARDWOOD)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_oh.png',
                width = 7, height = 7, units = 'in')

### PINE ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(pine = median(PINE)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_pine.png',
                width = 7, height = 7, units = 'in')

### SPRUCE ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(spruce = median(SPRUCE)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_spruce.png',
                width = 7, height = 7, units = 'in')

### TAMARACK ###

# Plot median over time
post_df |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tamarack = median(TAMARACK)) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/estimated_posterior_median_abundance_tamarack.png',
                width = 7, height = 7, units = 'in')

#### Spatial uncertainty plots ####

pdf(file = 'figures/data/estimated_posterior_uncertainty.pdf',
    width = 8, height = 10.5)

### ASH ###

# Plot different quantiles at one time point
# This is to look at the uncertainty in relative abundance estimates
# but simplifying the plots by only looking at one representative time point
# Arbitrarily chose 1100 YBP because it's in the middle of our temporal domain
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Ash (', italic('Fraxinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### BEECH ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### BIRCH ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### ELM ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### HEMLOCK ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1,
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### MAPLE ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OAK ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OTHER CONIFER ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OTHER HARDWOOD ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### PINE ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### SPRUCE ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### TAMARACK ###

# Plot minimum, median, maximum at one time point
post_df |>
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
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                na.value = '#00000000',
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(metric)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

dev.off()
