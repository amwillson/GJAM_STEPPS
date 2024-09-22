### STEP 4-2

## Adding soil and climate reconstructions to STEPPS subsampled relative abundance draws

## Input: data/intermediate/stepps_post_subsampled.RData
## Dataframe with subset of spatio-temporal locations

## Input: data/processed/mean_stepps_soil_clim.RData
## Formatted mean STEPPS relative abundances with climate and soil reconstructions

## Output: data/processed/post_stepps_soil_clim.RData
## Posterior estimates from STEPPS with co-located climate and soil covariates
## Used in 4.3.run_gjam.R, 4.6.oos_prediction_draws.R, 
## 4.7.plot_oos_prediction_draws.R, and 4.8.run_gjam_300YBP.R

rm(list = ls())

# Load helper functions
source('R/funs.R')

#### Combine data ####

# Load formatted posterior draws
load('data/intermediate/stepps_post_subsampled.RData')

# Load formatted mean with climate and soil data
load('data/processed/mean_stepps_soil_clim.RData')

# Select only climate and soil varaibles from mean STEPPS dataframe
# The climate and soil are the same. We only want to change estimates from
# STEPPS
soil_clim_insample <- dplyr::select(taxon_insample_all, c(stepps_x:time, clay:prsd))
soil_clim_oos <- dplyr::select(taxon_oos_all, c(stepps_x:time, clay:prsd))

# Remove mean dfs
rm(taxon_insample_all, taxon_oos_all)

# Combine climate/soil and posterior draws by time and coordinates
post_insample_all <- post_insample |>
  dplyr::rename(stepps_x = x,
                stepps_y = y) |>
  dplyr::full_join(y = soil_clim_insample,
                   by = c('stepps_x', 'stepps_y', 'time')) |>
  dplyr::rename(x = stepps_x,
                y = stepps_y) |>
  dplyr::filter(!is.na(draw))

post_oos_all <- post_oos |>
  dplyr::rename(stepps_x = x,
                stepps_y = y) |>
  dplyr::full_join(y = soil_clim_oos,
                   by = c('stepps_x', 'stepps_y', 'time')) |>
  dplyr::rename(x = stepps_x,
                y = stepps_y) |>
  dplyr::filter(!is.na(draw))

#### Plotting checks ####

## These are more production-level plots of response variables
## and covariates, rather than ones meant for checks
## They should look identical to those in step 3-2
## because we just added the data from step 3-2
## to the posterior draws from STEPPS from section 4

# Map of study region
states <- map_states()

# Order of facets
time_order <- c('1900 YBP', '1500 YBP', '1100 YBP', '700 YBP')

### Soil covariates ###

### CLAY ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(clay = median(clay)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  dplyr::mutate(data = dplyr::if_else(is.na(clay), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = clay)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', name = '% clay',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil Clay Content') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### SAND ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(sand = median(sand)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  dplyr::mutate(data = dplyr::if_else(is.na(sand), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', name = '% sand',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil Sand Content') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### SILT ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(silt = median(silt)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  dplyr::mutate(data = dplyr::if_else(is.na(silt), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = silt)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', name = '% silt',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil Silt Content') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### CACO3 ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(caco3 = median(caco3)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  dplyr::mutate(data = dplyr::if_else(is.na(caco3), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = caco3)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', name = '[CaCO3]',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil [CaCO3]') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### AWC ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(awc = median(awc)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  dplyr::mutate(data = dplyr::if_else(is.na(awc), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = awc)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', name = 'Available water\ncontent',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Available Water Content') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### Climate covariates ###

### AAT ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(aat = median(aat)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  dplyr::mutate(data = dplyr::if_else(is.na(aat), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'F', name = 'Average annual\ntemperature (°C)',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Average Annual Temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### TPR ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tpr = median(tpr)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  dplyr::mutate(data = dplyr::if_else(is.na(tpr), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'G', name = 'Total annual\nprecipitation (mm)',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Total Annual Precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### TSD ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tsd = median(tsd)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  dplyr::mutate(data = dplyr::if_else(is.na(tsd), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tsd)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'D', name = 'Temperature\nseasonality (°C)',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Temperature Seasonality (Standard Deviation)') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### PRSD ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(prsd = median(prsd)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  dplyr::mutate(data = dplyr::if_else(is.na(prsd), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'E', name = 'Precipitation\nseasonality (mm)',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Precipitation Seasonality (Standard Deviation)') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### Relative abundance ###

### ASH ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(ash = median(ASH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(ash), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ash)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(ASH, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(ASH, probs = 0.25, na.rm = TRUE),
                   `50%` = median(ASH, na.rm = TRUE),
                   `75%` = quantile(ASH, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(ASH, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### BEECH ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(beech = median(BEECH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(beech), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(BEECH, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(BEECH, probs = 0.25, na.rm = TRUE),
                   `50%` = median(BEECH, na.rm = TRUE),
                   `75%` = quantile(BEECH, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(BEECH, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### BIRCH ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(birch = median(BIRCH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(birch), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(BIRCH, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(BIRCH, probs = 0.25, na.rm = TRUE),
                   `50%` = median(BIRCH, na.rm = TRUE),
                   `75%` = quantile(BIRCH, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(BIRCH, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### ELM ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(elm = median(ELM)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(elm), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(ELM, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(ELM, probs = 0.25, na.rm = TRUE),
                   `50%` = median(ELM, na.rm = TRUE),
                   `75%` = quantile(ELM, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(ELM, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### HEMLOCK ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(hemlock = median(HEMLOCK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(hemlock), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(HEMLOCK, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(HEMLOCK, probs = 0.25, na.rm = TRUE),
                   `50%` = median(HEMLOCK, na.rm = TRUE),
                   `75%` = quantile(HEMLOCK, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(HEMLOCK, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### MAPLE ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(maple = median(MAPLE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(maple), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(MAPLE, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(MAPLE, probs = 0.25, na.rm = TRUE),
                   `50%` = median(MAPLE, na.rm = TRUE),
                   `75%` = quantile(MAPLE, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(MAPLE, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### OAK ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(oak = median(OAK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(oak), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(OAK, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(OAK, probs = 0.25, na.rm = TRUE),
                   `50%` = median(OAK, na.rm = TRUE),
                   `75%` = quantile(OAK, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(OAK, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color =data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### OTHER CONIFER ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_conifer = median(OTHER.CONIFER)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(other_conifer), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(OTHER.CONIFER, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(OTHER.CONIFER, probs = 0.25, na.rm = TRUE),
                   `50%` = median(OTHER.CONIFER, na.rm = TRUE),
                   `75%` = quantile(OTHER.CONIFER, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(OTHER.CONIFER, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### OTHER HARDWOOD ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_hardwood = median(OTHER.HARDWOOD)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(other_hardwood), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(OTHER.HARDWOOD, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(OTHER.HARDWOOD, probs = 0.25, na.rm = TRUE),
                   `50%` = median(OTHER.HARDWOOD, na.rm = TRUE),
                   `75%` = quantile(OTHER.HARDWOOD, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(OTHER.HARDWOOD, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### PINE ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(pine = median(PINE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(pine), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(PINE, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(PINE, probs = 0.25, na.rm = TRUE),
                   `50%` = median(PINE, na.rm = TRUE),
                   `75%` = quantile(PINE, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(PINE, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### SPRUCE ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(spruce = median(SPRUCE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(spruce), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(SPRUCE, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(SPRUCE, probs = 0.25, na.rm = TRUE),
                   `50%` = median(SPRUCE, na.rm = TRUE),
                   `75%` = quantile(SPRUCE, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(SPRUCE, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### TAMARACK ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tamarack = median(TAMARACK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  dplyr::mutate(data = dplyr::if_else(is.na(tamarack), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(TAMARACK, probs = 0.025, na.rm = TRUE),
                   `25%` = quantile(TAMARACK, probs = 0.25, na.rm = TRUE),
                   `50%` = median(TAMARACK, na.rm = TRUE),
                   `75%` = quantile(TAMARACK, probs = 0.75, na.rm = TRUE),
                   `97.5%` = quantile(TAMARACK, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  dplyr::mutate(data = dplyr::if_else(is.na(value), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_fill_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1,
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Save
save(post_insample_all, post_oos_all,
     file = 'data/processed/post_stepps_soil_clim.RData')
