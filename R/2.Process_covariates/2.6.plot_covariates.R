### STEP 2-6

## Plot climate and soil variables across entire spatiotemporal domain
## Intended to be production-level figures of environmental covariates

## Plot climate variables to ensure geographical patterns are consistent
## with the plots from step 2-3

## Input: data/processed/gridded_climate.RData
## Processed and formatted climate covariates

## Input: data/processed/gridded_soil.RData
## Processed and formatted soil covariates

## Output: none

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

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/spatiotemporal_aat.png',
                height = 7, width = 7, units = 'in')

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

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/spatiotemporal_tpr.png',
                height = 7, width = 7, units = 'in')

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

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/spatiotemporal_tsd.png',
                height = 7, width = 7, units = 'in')

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

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                 filename = 'figures/data/spatiotemporal_prsd.png',
                 height = 7, width = 7, units = 'in')

# Plot climate variables over time at each location
p1 <- climate_grid |>
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

p2 <- climate_grid |>
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

p3 <- climate_grid |>
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

p4 <- climate_grid |>
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

# Plot together
cowplot::plot_grid(p1, p2,
                   p3, p4, 
                   nrow = 2)

# Sav3
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/temporal_climate_variables.png',
                height = 10, width = 7, units = 'in')

#### Soil variables ####

# Load soil estimates
load('data/processed/gridded_soil.RData')

## Plot soil variables to ensure geographical patterns are consistent
## with what was found at finer grid in amwillson/historic-modern-environment
## Since soil is the same at all times, just randomly subsetting one

p1 <- soil_grid |>
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

p2 <- soil_grid |>
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

# Plot together
cowplot::plot_grid(p1, p2,
                   nrow = 2)

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/data/spatial_soil_variables.png',
                height = 10, width = 7, units = 'in')
