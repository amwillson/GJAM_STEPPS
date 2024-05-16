## Plot residuals for each taxon

rm(list = ls())

# Load helper functions
source('R/funs.R')

# Load residuals from models using mean relative abundance
load('data/processed/mean_residuals.RData')

# Map of states
states <- map_states()

# Order of facets
time_order <- c('2100 YBP', '2000 YBP', '1900 YBP', '1800 YBP', '1700 YBP',
                '1600 YBP', '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                '1100 YBP', '1000 YBP', '900 YBP', '800 YBP', '700 YBP',
                '600 YBP', '500 YBP', '400 YBP', '300 YBP', '200 YBP')

# Plot each taxon
resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = ash)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Ash') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Beech') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Birch') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Elm') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Maple') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Oak') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Pine') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Spruce') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

resids |>
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
  ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradient2(na.value = 'white', 
                                low = '#8C510A', 
                                high = '#01665E', 
                                midpoint = 0,
                                name = 'Residual\nabundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
