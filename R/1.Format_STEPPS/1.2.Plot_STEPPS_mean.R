### STEP 1-2

## Plotting mean relative abundances over space and time

## Input: data/processed/mean_STEPPS.RData
## coordinates, time, and relative abundances in array format

## Output: none

rm(list = ls())

# Load helper functions
source('R/funs.R')

# Load means
load('data/processed/mean_STEPPS.RData')

# Format into dataframes
ash_melt <- melt_array(taxon_mat = ash,
                       x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'ash'))
beech_melt <- melt_array(taxon_mat = beech,
                         x = x, y = y, time = time,
                         col_names = c('x', 'y', 'time', 'beech'))
birch_melt <- melt_array(taxon_mat = birch,
                         x = x, y = y, time = time,
                         col_names = c('x', 'y', 'time', 'birch'))
elm_melt <- melt_array(taxon_mat = elm,
                       x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'elm'))
hemlock_melt <- melt_array(taxon_mat = hemlock,
                           x = x, y = y, time = time,
                           col_names = c('x', 'y', 'time', 'hemlock'))
maple_melt <- melt_array(taxon_mat = maple,
                         x = x, y = y, time = time,
                         col_names = c('x', 'y', 'time', 'maple'))
oak_melt <- melt_array(taxon_mat = oak,
                       x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'oak'))
other_conifer_melt <- melt_array(taxon_mat = other_conifer,
                                 x = x, y = y, time = time,
                                 col_names = c('x', 'y', 'time', 'other_conifer'))
other_hardwood_melt <- melt_array(taxon_mat = other_hardwood,
                                  x = x, y = y, time = time,
                                  col_names = c('x', 'y', 'time', 'other_hardwood'))
pine_melt <- melt_array(taxon_mat = pine,
                        x = x, y = y, time = time,
                        col_names = c('x', 'y', 'time', 'pine'))
spruce_melt <- melt_array(taxon_mat = spruce,
                          x = x, y = y, time = time,
                          col_names = c('x', 'y', 'time', 'spruce'))
tamarack_melt <- melt_array(taxon_mat = tamarack,
                            x = x, y = y, time = time,
                            col_names = c('x', 'y', 'time', 'tamarack'))

# Combine individual taxon dataframes into one
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

# Map of study region
states <- map_states()

# Order of facets for plotting
time_order <- c('1900 YBP', '1800 YBP', '1700 YBP',
                '1600 YBP', '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                '1100 YBP', '1000 YBP', '900 YBP', '800 YBP', '700 YBP',
                '600 YBP', '500 YBP', '400 YBP', '300 YBP')

pdf(file = 'figures/data/estimated_mean_abundances.pdf',
    width = 8, height = 10.5)

# Plot the relative abundance of each taxon (individually) over space and time
# We just want to see what the data look like over the entire spatiotemporal domain
taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = ash)) +
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

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                na.value = '#00000000', 
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = birch)) +
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

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = elm)) +
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

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = hemlock)) +
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

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = maple)) +
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

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = oak)) +
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

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                na.value = '#00000000', 
                                direction = 1, 
                                name = 'Fraction total\nstems',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
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

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = pine)) +
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

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = spruce)) +
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

taxon_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tamarack)) +
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

dev.off()
