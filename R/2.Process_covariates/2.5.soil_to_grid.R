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

# Save
soil_grid <- dplyr::select(ash_melt, -ash)

save(soil_grid,
     file = 'data/processed/gridded_soil.RData')
