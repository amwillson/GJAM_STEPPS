### STEP 1-3

## Formatting STEPPS posterior draws
## Posterior draws are already in array format
## but there are no associated dimensions, so need to apply those
## from separate data object

## Input: data/input/posterior_stepps/rIts_sub.RDS
## Array with relative abundance estimates for 100 posterior samples from STEPPS model
## Dimensions: 704 spatial locations x 12 taxa x 20 time steps x 100 posterior samples
## Currently available privately, but will be made publicly available upon request

## Input: data/input/posterior_stepps/input.rdata
## Associated indexing data for the posterior draws
## For this project, centers_veg is the data object of interest,
## which contains coordinates for the centriods of each "location" in the samples array

## Output: data/processed/post_STEPPS.RData
## array with dimension names and formatting
## Used in 1.4.plot_stepps_draws.R, 4.1.subsample_stepps_draws.R, 
## and 4.11.format_final_oos_draws.R

rm(list = ls())

# Helper funs
source('R/funs.R')

# Load posterior draws 
# (this is unpublished from STEPPS)
post <- readRDS(file = 'data/input/posterior_stepps/rIts_sub.RDS')

# Load dimension information
# (this was provided as part of the same directory
# of unpublished STEPPS data)
load('data/input/posterior_stepps/input.rdata')

# Rescale coordinates
# Just a scalar to multiply cooridnates by to get
# actual coordinates
rescale <- 1e6

# Map of states
states <- map_states()

# Plot coordinates
# This ensures all the grid centroids are within the geospatial domain we expect
ggplot2::ggplot() +
  ggplot2::geom_point(data = centers_veg, ggplot2::aes(x = x * rescale, y = y * rescale)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Add index column to centers_veg
# Uniquely identifies all grid cell locations
centers_veg <- dplyr::mutate(centers_veg,
                             ind = seq(from = 1, to = nrow(centers_veg)))

# Add dimension names to posterior draws
dimnames(post) <- list(centers_veg$ind,
                       taxa,
                       ages,
                       seq(from = 1, to = 100))

# Coordinates need to be rescaled using the rescale factor
x <- unique(centers_veg$x * rescale)
y <- unique(centers_veg$y * rescale)

# Save formatted posterior draws
save(post, centers_veg, x, y,
     file = 'data/processed/post_STEPPS.RData')
