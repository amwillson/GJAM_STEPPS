### STEP 3-13

## Plotting out-of-sample prediction in space and time
## Out-of-sample prediction for all locations and times not used to fit the model

## Input: out/mean/oos_prediction_all.RData
## OOS predictions for all spatiotemporal locations using
## non-conditional, conditional, and conditional only on oak methods

## Input: data/processed/mean_stepps_full_oos.RData
## OOS data for all spatiotemporal locations not used for model fitting

## Output: none

rm(list = ls())

source('R/funs.R')

# Load out-of-sample predictions
load('out/mean/oos_prediction_all.RData')

# Load out-of-sample data
load('data/processed/mean_stepps_full_oos.RData')

#### Non-conditional prediction ####

# Map of study region
states <- map_states()

# Predictive mean
pred_mean <- pred$sdList$yMu

# Format
pred_mean <- as.data.frame(pred_mean)

# Drop NAs from data
taxon_oos_all <- tidyr::drop_na(taxon_oos_all)

# Add coordinates and time steps
pred_mean$x <- taxon_oos_all$x
pred_mean$y <- taxon_oos_all$y
pred_mean$time <- taxon_oos_all$time

### Plot each taxon's predictive mean over space and time ###

## Predictive mean is what the model on average
## predicts the relative abundance of each taxon
## to be, given the environment

# Order of facets
time_order <- c('1900 YBP', '1800 YBP', '1700 YBP', '1600 YBP',
                '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                '1100 YBP', '1000 YBP', '900 YBP', '800 YBP',
                '700 YBP', '600 YBP', '500 YBP', '400 YBP', '300 YBP')

## BEECH

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## BIRCH

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## ELM

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## HEMLOCK

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## MAPLE

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OAK

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER CONIFER

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oc)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER HARDWOOD

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## PINE

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## SPRUCE

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## TAMARACK

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

### Plot observed vs predicted irrespective of space/time ###

## How far off are our predictions from the observations of 
## relative abundance across all time steps?

pred_mean_long <- pred_mean |>
  # pivot predictions longer
  tidyr::pivot_longer(cols = beech:tamarack,
                      names_to = 'taxon',
                      values_to = 'Predicted') |>
  # rename other conifer and other hardwood
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'oc', 'other conifer', taxon),
                taxon = dplyr::if_else(taxon == 'oh', 'other hardwood', taxon)) |>
  # format
  dplyr::mutate(taxon = stringr::str_to_title(taxon))

obs_long <- taxon_oos_all |>
  # Select relevant columns
  dplyr::select(x:tamarack) |>
  # remove ash taxon
  dplyr::select(-ash) |>
  # pivot observations longer
  tidyr::pivot_longer(cols = beech:tamarack,
                      names_to = 'taxon',
                      values_to = 'Observed') |>
  # format
  dplyr::mutate(taxon = sub(pattern = '_', replacement = ' ', x = taxon),
                taxon = stringr::str_to_title(taxon))

# Combine
pred_obs_long <- pred_mean_long |>
  dplyr::full_join(y = obs_long,
                   by = c('x', 'y', 'time', 'taxon'))

# Plot observed vs predicted with 1:1 line
# Predictions ideally would be along the 1:1 line and
# there would be even spread of points above and below 1:1 line
pred_obs_long |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Predicted, y = Observed)) +
  ggplot2::geom_abline(color = 'blue', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = Predicted, y = Observed),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                 strip.text = ggplot2::element_text(size = 14),
                 axis.title = ggplot2::element_text(size = 12))

# Color points by the number of points in a given region of the plot
# This function gets the relative density of points
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

pred_obs_long$density <- get_density(pred_obs_long$Predicted, pred_obs_long$Observed, n = 100)

# Plot predicted vs observed with point density colored
# this helps because there are so many points that it's not
# possible to see point density with different levels of opacity
pred_obs_long |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Predicted, y = Observed, color = density),
                      show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = Predicted, y = Observed),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::scale_color_distiller(transform = 'sqrt') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                 strip.text = ggplot2::element_text(size = 14),
                 axis.title = ggplot2::element_text(size = 12))

### Difference between observed and predicted ###

## How far off are our predictions from the observations of 
## relative abundance?

# Difference (observed - predicted)
diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(pred_mean, beech:tamarack)

# Assign coords
diff$x <- pred_mean$x
diff$y <- pred_mean$y
# Assign time
diff$time <- pred_mean$time

# Plot difference over space and time

## BEECH

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## BIRCH

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## ELM

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## HEMLOCK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## MAPLE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')')))
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OAK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER CONIFER

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER HARDWOOD

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## PINE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## SPRUCE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## TAMARACK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

#### Conditional prediction ####

## Repeat same types of figures but for prediction
## conditional on the observed relative abundance of
## all other taxa
## Code is nearly identical to the case of non-conditional prediction

# Loop through each output
pred_cond <- matrix(, nrow = nrow(pred_mean), ncol = 11)

for(i in 1:11){
  temp <- cond_pred[[i]]
  pred_cond[,i] <- temp$sdList$yMu[,i]
}

colnames(pred_cond) <- colnames(pred_mean)[1:11]
pred_cond <- as.data.frame(pred_cond)
pred_cond$x <- pred_mean$x
pred_cond$y <- pred_mean$y
pred_cond$time <- pred_mean$time

### Plot each taxon's predictive mean over space and time ###

## BEECH

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## BIRCH

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## ELM

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## HEMLOCK

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## MAPLE

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OAK

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER CONIFER

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oc)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER HARDWOOD

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## PINE

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## SPRUCE

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## TAMARACK

pred_cond |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

### Plot observed vs predicted irrespective of space/time ###

pred_cond_long <- pred_cond |>
  # pivot predictions longer
  tidyr::pivot_longer(cols = beech:tamarack,
                      names_to = 'taxon',
                      values_to = 'Predicted') |>
  # rename other conifer and other hardwood
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'oc', 'other conifer', taxon),
                taxon = dplyr::if_else(taxon == 'oh', 'other hardwood', taxon)) |>
  # format
  dplyr::mutate(taxon = stringr::str_to_title(taxon))

# Combine
pred_obs_long <- pred_cond_long |>
  dplyr::full_join(y = obs_long,
                   by = c('x', 'y', 'time', 'taxon'))

pred_obs_long |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Predicted, y = Observed)) +
  ggplot2::geom_abline(color = 'blue', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = Predicted, y = Observed),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                 strip.text = ggplot2::element_text(size = 14),
                 axis.title = ggplot2::element_text(size = 12))

pred_obs_long$density <- get_density(pred_obs_long$Predicted, pred_obs_long$Observed, n = 100)

pred_obs_long |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Predicted, y = Observed, color = density),
                      show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = Predicted, y = Observed),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::scale_color_distiller(transform = 'sqrt') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                 strip.text = ggplot2::element_text(size = 14),
                 axis.title = ggplot2::element_text(size = 12))

### Difference between observed and predicted ###

# Difference (observed - predicted)
diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(pred_cond, beech:tamarack)

# Assign coords
diff$x <- pred_mean$x
diff$y <- pred_mean$y
# Assign time
diff$time <- pred_mean$time

# Plot

## BEECH

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## BIRCH

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## ELM

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## HEMLOCK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## MAPLE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OAK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER CONIFER

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER HARDWOOD

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## PINE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## SPRUCE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## TAMARACK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

#### Conditional on oak ####

# Predictive mean
pred_mean <- oak_cond_pred$sdList$yMu

# Format
pred_mean <- as.data.frame(pred_mean)
pred_mean$x <- taxon_oos_all$x
pred_mean$y <- taxon_oos_all$y
pred_mean$time <- taxon_oos_all$time

### Plot each taxon's predictive mean over space and time ###

## BEECH

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## BIRCH

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5))

## ELM

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## HEMLOCK

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## MAPLE

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER CONIFER

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oc)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER HARDWOOD

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## PINE

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## SPRUCE

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## TAMARACK

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

### Plot observed vs predicted irrespective of space/time ###

pred_mean_long <- pred_mean |>
  # pivot predictions longer
  tidyr::pivot_longer(cols = beech:tamarack,
                      names_to = 'taxon',
                      values_to = 'Predicted') |>
  # rename other conifer and other hardwood
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'oc', 'other conifer', taxon),
                taxon = dplyr::if_else(taxon == 'oh', 'other hardwood', taxon)) |>
  # format
  dplyr::mutate(taxon = stringr::str_to_title(taxon))

# Combine
pred_obs_long <- pred_mean_long |>
  dplyr::full_join(y = obs_long,
                   by = c('x', 'y', 'time', 'taxon'))

pred_obs_long |>
  dplyr::filter(taxon != 'Oak') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Predicted, y = Observed)) +
  ggplot2::geom_abline(color = 'blue', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = Predicted, y = Observed),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                 strip.text = ggplot2::element_text(size = 14),
                 axis.title = ggplot2::element_text(size = 12))

pred_obs_long$density <- get_density(pred_obs_long$Predicted, pred_obs_long$Observed, n = 100)

pred_obs_long |>
  dplyr::filter(taxon != 'Oak') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Predicted, y = Observed, color = density),
                      show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = Predicted, y = Observed),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::scale_color_distiller(transform = 'sqrt') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                 strip.text = ggplot2::element_text(size = 14),
                 axis.title = ggplot2::element_text(size = 12))

### Difference between observed and predicted ###

# Difference (observed - predicted)
diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(pred_mean, beech:tamarack)

# Assign coords
diff$x <- pred_mean$x
diff$y <- pred_mean$y
# Assign time
diff$time <- pred_mean$time

# Plot

## BEECH

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## BIRCH

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## ELM

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## HEMLOCK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## MAPLE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER CONIFER

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER HARDWOOD

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## PINE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## SPRUCE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## TAMARACK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed-\nPredicted',
                                na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))
