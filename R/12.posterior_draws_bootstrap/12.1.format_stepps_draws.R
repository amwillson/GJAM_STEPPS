## Formatting STEPPS posterior draws

rm(list = ls())

# Helper funs
source('R/funs.R')

# Load posterior draws
post <- readRDS(file = 'data/raw/posterior_stepps/rIts_sub.RDS')

# Load dimension information
load('data/raw/posterior_stepps/input.rdata')

# Rescale coordinates
rescale <- 1e6

# Map of states
states <- map_states()

# Plot coordinates
ggplot2::ggplot() +
  ggplot2::geom_point(data = centers_veg, ggplot2::aes(x = x * rescale, y = y * rescale)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# Add index column to centers_veg
centers_veg <- dplyr::mutate(centers_veg,
                             ind = seq(from = 1, to = nrow(centers_veg)))

# Add dimension names to posterior draws
dimnames(post) <- list(centers_veg$ind,
                       taxa,
                       ages,
                       seq(from = 1, to = 100))

# Save formatted posterior draws
save(post, centers_veg,
     file = 'data/processed/post_STEPPS.RData')
