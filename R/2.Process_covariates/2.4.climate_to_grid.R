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

# Save
climate_grid <- dplyr::select(ash_melt, -ash)

save(climate_grid,
     file = 'data/processed/gridded_climate.RData')
