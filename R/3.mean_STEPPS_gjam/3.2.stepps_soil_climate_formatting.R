### STEP 3-2

## Adding soil and climate reconstructions to STEPPS subsampled mean relative abundances

## Input: data/intermediate/stepps_subsampled.RData
## Dataframe with subset of spatio-temporal locations

## Input: data/processed/gridded_soil.RData
## Soil estimates

## Input: data/processed/gridded_climate.RData
## Downscaled climate reconstructions

## Output: data/intermediate/taxon_insample_soil.RData
## Output: data/intermediate/taxon_oos_soil.RData
## Dataframes with STEPPS mean relative abundances and soil variables with
## in-sample and out-of-sample data saved separately.
## Not used, just an intermediate data step

## Output: data/processed/mean_stepps_soil_clim.RData
## Dataframes with STEPPS mean relative abundances, soil variables, and climate variables
## in-sample and out-of-sample data saved in separate data frames
## Used in 3.3.run_gjam.R, 3.6.oos_prediction.R, and 4.2.stepps_draws_soil_climate_formatting.R

rm(list = ls())

# Load helper functions
source('R/funs.R')

#### Soils ####

# Load STEPPS subsampled data from step 3-1
load('data/intermediate/stepps_subsampled.RData')

# Load soil data
load('data/processed/gridded_soil.RData')

# Join STEPPS and soil data
taxon_insample_soil <- taxon_insample |>
  dplyr::left_join(y = soil_grid,
                   by = c('x', 'y', 'time'))
taxon_oos_soil <- taxon_oos |>
  dplyr::left_join(y = soil_grid,
                   by = c('x', 'y', 'time'))

# If vegetation is NA, make soil NA
taxon_insample_soil <- taxon_insample_soil |>
  dplyr::mutate(clay = dplyr::if_else(is.na(ash), NA, clay),
                sand = dplyr::if_else(is.na(ash), NA, sand),
                silt = dplyr::if_else(is.na(ash), NA, silt),
                caco3 = dplyr::if_else(is.na(ash), NA, caco3),
                awc = dplyr::if_else(is.na(ash), NA, awc),
                flood = dplyr::if_else(is.na(ash), NA, flood))

taxon_oos_soil <- taxon_oos_soil |>
  dplyr::mutate(clay = dplyr::if_else(is.na(ash), NA, clay),
                sand = dplyr::if_else(is.na(ash), NA, sand),
                silt = dplyr::if_else(is.na(ash), NA, silt),
                caco3 = dplyr::if_else(is.na(ash), NA, caco3),
                awc = dplyr::if_else(is.na(ash), NA, awc),
                flood = dplyr::if_else(is.na(ash), NA, flood))

# Remove & rearrange columns
taxon_insample_soil <- dplyr::select(taxon_insample_soil,
                                     x, y, time,
                                     ash:tamarack,
                                     clay:awc)
taxon_oos_soil <- dplyr::select(taxon_oos_soil,
                                x, y, time,
                                ash:tamarack,
                                clay:awc)

#### Climate ####

# Load climate data
load('data/processed/gridded_climate.RData')

taxon_insample_all <- taxon_insample_soil |>
  dplyr::left_join(y = climate_grid,
                   by = c('x', 'y', 'time'))
taxon_oos_all <- taxon_oos_soil |>
  dplyr::left_join(y = climate_grid,
                   by = c('x', 'y', 'time'))

# If vegetation is NA, make soil NA
taxon_insample_all <- taxon_insample_all |>
  dplyr::mutate(aat = dplyr::if_else(is.na(ash), NA, aat),
               tpr = dplyr::if_else(is.na(ash), NA, tpr),
               tsd = dplyr::if_else(is.na(ash), NA, tsd),
               prsd = dplyr::if_else(is.na(ash), NA, prsd),
               prcv = dplyr::if_else(is.na(ash), NA, prcv))

taxon_oos_all <- taxon_oos_all |>
  dplyr::mutate(aat = dplyr::if_else(is.na(ash), NA, aat),
                tpr = dplyr::if_else(is.na(ash), NA, tpr),
                tsd = dplyr::if_else(is.na(ash), NA, tsd),
                prsd = dplyr::if_else(is.na(ash), NA, prsd),
                prcv = dplyr::if_else(is.na(ash), NA, prcv))

#### Plots ####

# These are more production-level plots of response variables
# and covariates, rather than ones meant for checks

# Order of facets
time_order <- c('1900 YBP', '1500 YBP', '1100 YBP', '700 YBP')

# Outline of states
states <- map_states()

### Relative abundance ###

## ASH 

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(ash), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ash)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                 name = 'Relative\nabundance',
                                 na.value = '#00000000',
                                 limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Ash') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## BEECH

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(beech), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## BIRCH

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(birch), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## ELM

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(elm), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## HEMLOCK

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(hemlock), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## MAPLE

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(maple), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## OAK

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(oak), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER CONIFER

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(other_conifer), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER HARDWOOD

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(other_hardwood), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## PINE

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(pine), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## SPRUCE

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(spruce), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## TAMARACK

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(tamarack), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = NA, color = data),
                     show.legend = FALSE) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, 
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

### Soil covariates ###

## CLAY

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(clay), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = clay)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, 
                                name = 'Soil %\nclay',
                                na.value = '#00000000',
                                limits = c(0, 100)) +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Soil % Clay') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## SAND

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(sand), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, 
                                name = 'Soil %\nsand',
                                na.value = '#00000000',
                                limits = c(0, 100)) +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Soil % Sand') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## SILT

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(silt), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = silt)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1,
                                name = 'Soil %\nsilt',
                                na.value = '#00000000',
                                limits = c(0, 100)) +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Soil % Silt') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## CACO3

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(caco3), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = caco3)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1,
                                name = '[CaCO3]',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('[CaCO3]') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## AWC

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(awc), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = awc)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Oranges', direction = 1, 
                                name = 'AWC',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Available Water Content') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

### Climate covariates ###

## AAT

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(aat), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'F', 
                                name = 'Average annual\ntemperature (°C)',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Average Annual Temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## TPR

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(tpr), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'G', 
                                name = 'Total annual\nprecipitation (mm)',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## TSD

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(tsd), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tsd)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'D', 
                                name = 'Temperature\nseasonality (°C)',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Temperature seasonality') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

## PRSD

taxon_insample_all |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::mutate(data = dplyr::if_else(is.na(prsd), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_viridis_c(option = 'E', 
                                name = 'Precipitation\nseasonality (mm)',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::ggtitle('Precipitation seasonality') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 12))

# Save
save(taxon_insample_all, taxon_oos_all,
     file = 'data/processed/mean_stepps_soil_clim.RData')
