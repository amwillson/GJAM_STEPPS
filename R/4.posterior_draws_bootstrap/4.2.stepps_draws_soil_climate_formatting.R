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

# Helper funs
source('R/funs.R')

#### Combine data ####

# Load formatted posterior draws
load('data/intermediate/stepps_post_subsampled.RData')

# Load formatted mean with climate and soil data
load('data/processed/mean_stepps_soil_clim.RData')

soil_clim_insample <- dplyr::select(taxon_insample_all, c(stepps_x:time, clay:prsd))
soil_clim_oos <- dplyr::select(taxon_oos_all, c(stepps_x:time, clay:prsd))

# Remove mean dfs
rm(taxon_insample_all, taxon_oos_all)

# Combine by time and coordinates
post_insample_all <- post_insample |>
  dplyr::rename(stepps_x = x,
                stepps_y = y) |>
  dplyr::full_join(y = soil_clim_insample,
                   by = c('stepps_x', 'stepps_y', 'time')) |>
  dplyr::rename(x = stepps_x,
                y = stepps_y)

post_oos_all <- post_oos |>
  dplyr::rename(stepps_x = x,
                stepps_y = y) |>
  dplyr::full_join(y = soil_clim_oos,
                   by = c('stepps_x', 'stepps_y', 'time')) |>
  dplyr::rename(x = stepps_x,
                y = stepps_y)

#### Plotting checks ####

# Map of study region
states <- map_states()

# Order of facets
time_order <- c('1900 YBP', '1500 YBP', '1100 YBP', '700 YBP')

## Plot covariates

### CLAY ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(clay = median(clay)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = clay),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Oranges', name = '% clay',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil Clay Content') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### SAND ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(sand = median(sand)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Oranges', name = '% sand',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil Sand Content') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### SILT ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(silt = median(silt)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = silt),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Oranges', name = '% silt',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil Silt Content') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### CACO3 ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(caco3 = median(caco3)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = caco3),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Oranges', name = '[CaCO3]',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Soil [CaCO3]') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### AWC ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(awc = median(awc)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = awc),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Oranges', name = 'Available water\ncontent',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Available Water Content') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### AAT ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(aat = median(aat)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aat),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_viridis_c(option = 'F', name = 'AAT') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Average Annual Temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### TPR ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tpr = median(tpr)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tpr),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_viridis_c(option = 'G', name = 'TPR') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Total Annual Precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### TSD ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tsd = median(tsd)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tsd),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_viridis_c(option = 'D', name = 'TSD') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Temperature Seasonality (Standard Deviation)') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### PRSD ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(prsd = median(prsd)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = prsd),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_viridis_c(option = 'E', name = 'PRSD') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Precipitation Seasonality (Standard Deviation)') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## Plot abundances

### ASH ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(ash = median(ASH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ash),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(ASH, probs = 0.025),
                   `25%` = quantile(ASH, probs = 0.25),
                   `50%` = median(ASH),
                   `75%` = quantile(ASH, probs = 0.75),
                   `97.5%` = quantile(ASH, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### BEECH ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(beech = median(BEECH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = beech),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(BEECH, probs = 0.025),
                   `25%` = quantile(BEECH, probs = 0.25),
                   `50%` = median(BEECH),
                   `75%` = quantile(BEECH, probs = 0.75),
                   `97.5%` = quantile(BEECH, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### BIRCH ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(birch = median(BIRCH)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = birch),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(BIRCH, probs = 0.025),
                   `25%` = quantile(BIRCH, probs = 0.25),
                   `50%` = median(BIRCH),
                   `75%` = quantile(BIRCH, probs = 0.75),
                   `97.5%` = quantile(BIRCH, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### ELM ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(elm = median(ELM)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elm),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(ELM, probs = 0.025),
                   `25%` = quantile(ELM, probs = 0.25),
                   `50%` = median(ELM),
                   `75%` = quantile(ELM, probs = 0.75),
                   `97.5%` = quantile(ELM, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### HEMLOCK ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(hemlock = median(HEMLOCK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = hemlock),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(HEMLOCK, probs = 0.025),
                   `25%` = quantile(HEMLOCK, probs = 0.25),
                   `50%` = median(HEMLOCK),
                   `75%` = quantile(HEMLOCK, probs = 0.75),
                   `97.5%` = quantile(HEMLOCK, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### MAPLE ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(maple = median(MAPLE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = maple),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(MAPLE, probs = 0.025),
                   `25%` = quantile(MAPLE, probs = 0.25),
                   `50%` = median(MAPLE),
                   `75%` = quantile(MAPLE, probs = 0.75),
                   `97.5%` = quantile(MAPLE, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OAK ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(oak = median(OAK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = oak),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(OAK, probs = 0.025),
                   `25%` = quantile(OAK, probs = 0.25),
                   `50%` = median(OAK),
                   `75%` = quantile(OAK, probs = 0.75),
                   `97.5%` = quantile(OAK, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OTHER CONIFER ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_conifer = median(OTHER.CONIFER)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = other_conifer),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(OTHER.CONIFER, probs = 0.025),
                   `25%` = quantile(OTHER.CONIFER, probs = 0.25),
                   `50%` = median(OTHER.CONIFER),
                   `75%` = quantile(OTHER.CONIFER, probs = 0.75),
                   `97.5%` = quantile(OTHER.CONIFER, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### OTHER HARDWOOD ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(other_hardwood = median(OTHER.HARDWOOD)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = other_hardwood),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(OTHER.HARDWOOD, probs = 0.025),
                   `25%` = quantile(OTHER.HARDWOOD, probs = 0.25),
                   `50%` = median(OTHER.HARDWOOD),
                   `75%` = quantile(OTHER.HARDWOOD, probs = 0.75),
                   `97.5%` = quantile(OTHER.HARDWOOD, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### PINE ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(pine = median(PINE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = pine),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(PINE, probs = 0.025),
                   `25%` = quantile(PINE, probs = 0.25),
                   `50%` = median(PINE),
                   `75%` = quantile(PINE, probs = 0.75),
                   `97.5%` = quantile(PINE, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### SPRUCE ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(spruce = median(SPRUCE)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = spruce),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(SPRUCE, probs = 0.025),
                   `25%` = quantile(SPRUCE, probs = 0.25),
                   `50%` = median(SPRUCE),
                   `75%` = quantile(SPRUCE, probs = 0.75),
                   `97.5%` = quantile(SPRUCE, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### TAMARACK ###

post_insample_all |>
  dplyr::group_by(time, x, y) |>
  dplyr::summarize(tamarack = median(TAMARACK)) |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tamarack),
                      shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

post_insample_all |>
  dplyr::filter(time == 7) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(`2.5%` = quantile(TAMARACK, probs = 0.025),
                   `25%` = quantile(TAMARACK, probs = 0.25),
                   `50%` = median(TAMARACK),
                   `75%` = quantile(TAMARACK, probs = 0.75),
                   `97.5%` = quantile(TAMARACK, probs = 0.975)) |>
  tidyr::pivot_longer(cols = `2.5%`:`97.5%`,
                      names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value),
                      shape = 15, size = 5) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~quantile) +
  ggplot2::scale_color_distiller(palette = 'Greens', name = 'Relative\nabundance',
                                 direction = 1) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Save
save(post_insample_all, post_oos_all,
     file = 'data/processed/post_stepps_soil_clim.RData')
