### STEP 2-6

## Plot climate and soil variables across entire spatiotemporal domain
## Intended to be production-level figures of environmental covariates

## Plot climate variables to ensure geographical patterns are consistent
## with the plots from step 2-3

rm(list = ls())

# Helper funs
source('R/funs.R')

# Order of facets
time_order <- c('1900 YBP', '1800 YBP', '1700 YBP', '1600 YBP',
                '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                '1100 YBP', '1000 YBP', '900 YBP', '800 YBP',
                '700 YBP', '600 YBP', '500 YBP', '400 YBP', '300 YBP')

# States outline
states <- map_states()

#### Climate variables ####

# Load climate variables
load('data/processed/gridded_climate.RData')

# Open pdf
pdf(file = 'figures/data/spatiotemporal_climate_variables.pdf',
    width = 8, height = 6)

climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'F',
                                na.value = '#00000000',
                                name = '°C') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'G',
                                na.value = '#00000000',
                                name = 'mm/year',
                                direction = -1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'D',
                                na.value = '#00000000',
                                name = '°C') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Temperature seasonality (standard deviation)') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time, as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'E',
                                na.value = '#00000000',
                                name = 'mm/year') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Precipitation seasonality (standard deviation)') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(prcv = prcv * 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prcv)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'E',
                                na.value = '#00000000',
                                name = '%') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Precipitation seasonality (coefficient of variation)') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

dev.off()

# Open new pdf
pdf(file = 'figures/data/temporal_climate_variables.pdf',
    width = 6, height = 4)

# Plot climate variables over time at each location
climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00'),
                time = as.numeric(time)) |>
  dplyr::group_by(time) |>
  dplyr::summarize(med = median(aat, na.rm = TRUE),
                   low = min(aat, na.rm = TRUE),
                   high = max(aat, na.rm = TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = med), linewidth = 1) +
  ggplot2::geom_ribbon(ggplot2::aes(x = time, ymin = low, ymax = high),
                       alpha = 0.2) +
  ggplot2::scale_x_reverse() +
  ggplot2::xlab('Time (YBP)') + ggplot2::ylab('Average annual temperature (°C)') +
  ggplot2::theme_minimal()

climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00'),
                time = as.numeric(time)) |>
  dplyr::group_by(time) |>
  dplyr::summarize(med = median(tpr, na.rm = TRUE),
                   low = min(tpr, na.rm = TRUE),
                   high = max(tpr, na.rm = TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = med), linewidth = 1) +
  ggplot2::geom_ribbon(ggplot2::aes(x = time, ymin = low, ymax = high),
                       alpha = 0.2) +
  ggplot2::scale_x_reverse() +
  ggplot2::xlab('Time (YBP)') + ggplot2::ylab('Total annual precipitation (mm/year)') +
  ggplot2::theme_minimal()

climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00'),
                time = as.numeric(time)) |>
  dplyr::group_by(time) |>
  dplyr::summarize(med = median(tsd, na.rm = TRUE),
                   low = min(tsd, na.rm = TRUE),
                   high = max(tsd, na.rm = TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = med), linewidth = 1) +
  ggplot2::geom_ribbon(ggplot2::aes(x = time, ymin = low, ymax = high),
                       alpha = 0.2) +
  ggplot2::scale_x_reverse() +
  ggplot2::xlab('Time (YBP)') + ggplot2::ylab('Temperature seasonality (°C)') +
  ggplot2::theme_minimal()

climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00'),
                time = as.numeric(time)) |>
  dplyr::group_by(time) |>
  dplyr::summarize(med = median(prsd, na.rm = TRUE),
                   low = min(prsd, na.rm = TRUE),
                   high = max(prsd, na.rm = TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = med), linewidth = 1) +
  ggplot2::geom_ribbon(ggplot2::aes(x = time, ymin = low, ymax = high),
                       alpha = 0.2) +
  ggplot2::scale_x_reverse() +
  ggplot2::xlab('Time (YBP)') + ggplot2::ylab('Precipitation seasonality (mm/year)') +
  ggplot2::theme_minimal()

climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00'),
                time = as.numeric(time)) |>
  dplyr::group_by(time) |>
  dplyr::summarize(med = median(prcv, na.rm = TRUE),
                   low = min(prcv, na.rm = TRUE),
                   high = max(prcv, na.rm = TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = med), linewidth = 1) +
  ggplot2::geom_ribbon(ggplot2::aes(x = time, ymin = low, ymax = high),
                       alpha = 0.2) +
  ggplot2::scale_x_reverse() +
  ggplot2::xlab('Time (YBP)') + ggplot2::ylab('Precipitation seasonality (%)') +
  ggplot2::theme_minimal()

dev.off()

#### Soil variables ####

# Load soil estimates
load('data/processed/gridded_soil.RData')

## Plot soil variables to ensure geographical patterns are consistent
## with what was found at finer grid in amwillson/historic-modern-environment
## Since soil is the same at all times, just randomly subsetting one

pdf(file = 'figures/data/spatial_soil_variables.pdf',
    width = 5, height = 4)

soil_grid |>
  dplyr::filter(time == 10) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = clay)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                na.value = '#00000000',
                                name = '% clay',
                                limits = c(0, 100)) +
  ggplot2::ggtitle('Soil % clay') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

soil_grid |>
  dplyr::filter(time == 10) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                na.value = '#00000000',
                                name = '% sand',
                                limits = c(0, 100)) +
  ggplot2::ggtitle('Soil % sand') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

soil_grid |>
  dplyr::filter(time == 10) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = silt)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                na.value = '#00000000',
                                name = '% silt',
                                limits = c(0, 100)) +
  ggplot2::ggtitle('Soil % silt') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

soil_grid |>
  dplyr::filter(time == 10) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = caco3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                na.value = '#00000000',
                                name = expression(paste('% CaC', O[3]))) +
  ggplot2::ggtitle('Soil calcium carbonate concentration') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

soil_grid |>
  dplyr::filter(time == 10) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = awc)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                na.value = '#00000000',
                                name = 'cm/cm') +
  ggplot2::ggtitle('Soil available water content') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

dev.off()
