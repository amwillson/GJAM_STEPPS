### STEP 2-5

## Aggregate previous estimates of soil texture to STEPPS grid
## Use STEPPS grid to identify all soil estimates in
## the grid cell and average over all estimates in the grid cell

## Input: data/input/gridded_soil.RData
## From amwillson/historic-modern-environment
## Estimates soil covariates on 8 x 8 km grid

## Input: data/processed/mean_STEPPS.RData
## STEPPS data including coordinates and time step
## Used in 3.2.stepps_soil_climate_formatting.R

## Output: 'data/processed/gridded_soil.RData
## Soil estimates in STEPPS gridded format
## Used in 3.2.stepps_soil_climate_formatting.R,
## 3.11.format_final_oos.R

rm(list = ls())

# Helper functions
source('R/funs.R')

# Load soil data that needs to be put in the coaresr grid format
load('data/input/gridded_soil.RData')

# Load STEPPS data
load('data/processed/mean_STEPPS.RData')

# Melt ash to dataframe to use for making spatiotemporal grid
# of soil data that matches STEPPS
ash_melt <- melt_array(taxon_mat = ash,
                       x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'ash'))

# For each STEPPS grid cell and time step, find all climate points corresponding within
for(i in 1:nrow(ash_melt)){
  # Take one spatiotemporal point from STEPPS
  sub <- ash_melt[i,]
  # If there is no STEPPS reconstruction (where the grid cell is outside
  # spatiotemporal domain)
  if(is.na(sub$ash)){
    # Make all soil NA as well
    ash_melt$clay[i] <- NA
    ash_melt$sand[i] <- NA
    ash_melt$silt[i] <- NA
    ash_melt$caco3[i] <- NA
    ash_melt$awc[i] <- NA
    ash_melt$flood[i] <- NA
    # Otherwise proceed
  }else{
    # Find maximum extent of each grid cell
    # Since coordinates are centroids, +/- 12 km on each side
    xmin <- sub$x - 12000
    xmax <- sub$x + 12000
    ymin <- sub$y - 12000
    ymax <- sub$y + 12000
    
    # Find soil estimates occurring within the gid cell
    sub2 <- dplyr::filter(veg_unique_grid, x >= xmin & x <= xmax &
                            y >= ymin & y <= ymax)
    
    # Summarize climate variables for all estimates within the grid cell
    ash_melt$clay[i] <- mean(sub2$clay)
    ash_melt$sand[i] <- mean(sub2$sand)
    ash_melt$silt[i] <- mean(sub2$silt)
    ash_melt$caco3[i] <- mean(sub2$caco3)
    ash_melt$awc[i] <- mean(sub2$awc)
    ash_melt$flood[i] <- mean(sub2$flood)
  }
  print(i)
}

# State outlines
states <- map_states()

## Plot soil variables to ensure geographical patterns are consistent
## with what was found at finer grid in amwillson/historic-modern-environment
## Since soil is the same at all times, just randomly subsetting one

ash_melt |>
  dplyr::filter(time == 10) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = clay)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                na.value = '#00000000',
                                name = '% clay') +
  ggplot2::ggtitle('Soil % clay') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ash_melt |>
  dplyr::filter(time == 10) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                na.value = '#00000000',
                                name = '% sand') +
  ggplot2::ggtitle('Soil % sand') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ash_melt |>
  dplyr::filter(time == 10) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = silt)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                na.value = '#00000000',
                                name = '% silt') +
  ggplot2::ggtitle('Soil % silt') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ash_melt |>
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

ash_melt |>
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

ash_melt |>
  dplyr::filter(time == 10) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = flood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                na.value = '#00000000',
                                name = 'Fraction grid cell') +
  ggplot2::ggtitle('Fraction of grid cell in a floodplain') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Save
soil_grid <- dplyr::select(ash_melt, -ash)

save(soil_grid,
     file = 'data/processed/gridded_soil.RData')
