## Formatting STEPPS posterior draws

rm(list = ls())

# Helper funs
source('R/funs.R')

# Load posterior draws
post <- readRDS(file = 'data/raw/posterior_stepps/rIts_sub.RDS')

# Load dimension information
load('data/raw/posterior_stepps/input.rdata')

# Keep only things that are relevant
rm(centers_pls, centers_pol, d_inter, d_knots,
   knot_coords, lag, meta_pol, meta_pol_all, pollen_check,
   y, y_veg, eta, gamma, idx_cores, K, N, N_cores, N_knots, N_pls,
   phi, res, rho, sum_w_pot, T, w)

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

# Melt to dataframe
post_df <- reshape2::melt(post)

# Column names
colnames(post_df) <- c('ind', 'taxon', 'time', 'draw', 'val')

# Format
post_df <- post_df |>
  # Add coordinates
  dplyr::full_join(y = centers_veg, by = 'ind') |>
  # Fix coords
  dplyr::mutate(x = x * rescale,
                y = y * rescale) |>
  # Remove indexing
  dplyr::select(-ind)

# Save formatted posterior draws
save(post_df, file = 'data/processed/post_STEPPS.RData')
