### STEP 2-4

## Aggregate climate reconstructions to STEPPS grid
## Use STEPPS grid to identify all climate reconstructions in
## the grid cell at the same time step and average over all
## reconstructions in the grid cell

## Input: data/intermediate/clipped_clim_alltime.RData
## Climate reconstructions from preivous step

## Input: data/processed/mean_STEPPS.RData
## STEPPS data including coordinates and time step

## Output: data/processed/gridded_climate.RData
## Climate covariates in STEPPS gridded format
## Used in 3.2.stepps_soil_climate_formatting.R,
## 3.11.format_final_oos.R

rm(list = ls())

# Helper functions
source('R/funs.R')

# Load climate data that needs to be put in regular grid format
load('data/intermediate/clipped_clim_alltime.RData')

# Load STEPPS data
load('data/processed/mean_STEPPS.RData')

# Melt ash to dataframe to use for making spatiotemporal grid
# of climate data that matches STEPPS
ash_melt <- melt_array(taxon_mat = ash,
                       x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'ash'))

# Convert time in climate dataset to YBP
unbias <- unbias |>
  dplyr::mutate(time = 1950 - time,
                time = time / 100)

# For each STEPPS grid cell and time step, find all climate points corresponding within
for(i in 1:nrow(ash_melt)){
  # Take one spatiotemporal point from  STEPPS
  sub <- ash_melt[i,]
  # If there is no STEPPS reconstruction (where the grid cell is outside
  # spatiotemporal domain)
  if(is.na(sub$ash)){
    # Make all climate NA as well
    ash_melt$aat[i] <- NA
    ash_melt$tpr[i] <- NA
    ash_melt$tsd[i] <- NA
    ash_melt$prsd[i] <- NA
    ash_melt$prcv[i] <- NA
    # Otherwise proceed
  }else{
    # Find maximum extent of each grid cell
    # Since coordinate are centroids, +/- 12 km on each side
    xmin <- sub$x - 12000
    xmax <- sub$x + 12000
    ymin <- sub$y - 12000
    ymax <- sub$y + 12000
    
    # Find climate reconstructions occurring within the grid cell at that time
    sub2 <- dplyr::filter(unbias, time == sub$time &
                            x >= xmin & x <= xmax &
                            y >= ymin & y <= ymax)
    
    # Summarize climate variables for all reconstructions within the grid cell
    ash_melt$aat[i] <- mean(sub2$aat)
    ash_melt$tpr[i] <- mean(sub2$tpr)
    ash_melt$tsd[i] <- mean(sub2$tsd)
    ash_melt$prsd[i] <- mean(sub2$prsd)
    ash_melt$prcv[i] <- mean(sub2$prcv)
  }
  print(i)
}

# State outlines
states <- map_states()

## Plot climate variables to ensure geographical patterns are consistent
## with the plots from step 2-3

time_order <- c('1900 YBP', '1800 YBP', '1700 YBP', '1600 YBP',
                '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                '1100 YBP', '1000 YBP', '900 YBP', '800 YBP',
                '700 YBP', '600 YBP', '500 YBP', '400 YBP', '300 YBP')

ash_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'A',
                                na.value = '#00000000',
                                name = '°C') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ash_melt |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'G',
                                na.value = '#00000000',
                                name = 'mm/year') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ash_melt |>
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

ash_melt |>
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

ash_melt |>
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

# Save
climate_grid <- dplyr::select(ash_melt, -ash)

save(climate_grid,
     file = 'data/processed/gridded_climate.RData')
