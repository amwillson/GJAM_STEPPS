### STEP 3-7

## Plotting out-of-sample predictions in time
## Out-of-sample prediction of 300 YBP for same spatial locations as
## used to fit the model

## Input: out/mean/oos_prediciton_time.RData
## OOS predictions for 300 YBP using non-conditional, conditional, and
## conditional on only oak methods

## Input: data/processed/mean_stepps_soil_clim.RData
## OOS data

## Output: none

rm(list = ls())

source('R/funs.R')

# Load out-of-sample predictions
load('out/mean/oos_prediction_time.RData')

# Load out-of-sample data
load('data/processed/mean_stepps_soil_clim.RData')

#### Non-conditional prediction ####

# Map of study region
states <- map_states()

# Predictive mean
pred_mean <- pred$sdList$yMu

# Only non-NA observations
taxon_oos_all2 <- tidyr::drop_na(taxon_oos_all)

# Format
pred_mean <- as.data.frame(pred_mean)

# Add coordinates
pred_mean$x <- taxon_oos_all2$x
pred_mean$y <- taxon_oos_all2$y

# Add to full grid (with missing data)
pred_mean2 <- taxon_oos_all |>
  dplyr::select(x, y) |>
  dplyr::rename(x = x,
                y = y) |>
  dplyr::left_join(y = pred_mean, by = c('x', 'y'))

### Plot each taxon's predictive mean over space ###

## Predictive mean is what the model on average
## predicts the relative abundance of each taxon
## to be, given the environment

## BEECH

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(beech), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(birch), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(elm), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(hemlock), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(maple), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(oak), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(oc), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oc)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(oh), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(pine), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(spruce), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(tamarack), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### Difference between observed and predicted ###

## How far off are our predictions from the observations of 
## relative abundance?

# Difference (observed - predicted)
diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(pred_mean2, beech:tamarack)

# Assign coords
diff$x <- pred_mean2$x
diff$y <- pred_mean2$y

# Plot

## BEECH

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(beech), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(birch), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(elm), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(hemlock), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(maple), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(oak), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(other_conifer), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(other_hardwood), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(pine), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(spruce), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(tamarack), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

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

pred_cond2 <- taxon_oos_all |>
  dplyr::select(x, y) |>
  dplyr::rename(x = x,
                y = y) |>
  dplyr::left_join(y = pred_cond, by = c('x', 'y'))

### Plot each taxon's predictive mean over space ###

## BEECH

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(beech), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(birch), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(elm), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(hemlock), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(maple), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(oak), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(oc), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oc)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data), 
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(oh), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(pine), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(spruce), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

pred_cond2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(tamarack), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### Difference between observed and predicted ###

# Difference (observed - predicted)
diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(pred_cond2, beech:tamarack)

# Assign coords
diff$x <- pred_mean2$x
diff$y <- pred_mean2$y

# Plot

## BEECH

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(beech), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(birch), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(elm), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(hemlock), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(maple), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(oak), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(other_conifer), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(other_hardwood), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(pine), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(spruce), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(tamarack), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### Conditional on oak ####

## Repeat same types of figures but for prediction
## conditional on the observed relative abundance of oak
## Code is nearly identical to the case of non-conditional prediction

# Predictive mean
pred_mean <- oak_cond_pred$sdList$yMu

# Format
pred_mean <- as.data.frame(pred_mean)

# Add coordinates
pred_mean$x <- taxon_oos_all2$x
pred_mean$y <- taxon_oos_all2$y

# Add to full grid (with missing data)
pred_mean2 <- taxon_oos_all |>
  dplyr::select(x, y) |>
  dplyr::rename(x = x,
                y = y) |>
  dplyr::left_join(y = pred_mean, by = c('x', 'y'))

### Plot each taxon's predictive mean over space ###

## BEECH

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(beech), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(birch), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(elm), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(hemlock), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(maple), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(oc), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oc)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(oh), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oh)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(pine), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(spruce), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

pred_mean2 |>
  dplyr::mutate(data = dplyr::if_else(is.na(tamarack), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### Difference between observed and predicted ###

# Difference (observed - predicted)
diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(pred_mean2, beech:tamarack)

# Assign coords
diff$x <- pred_mean2$x
diff$y <- pred_mean2$y

# Plot

## BEECH

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(beech), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = beech)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(birch), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = birch)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(elm), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(hemlock), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(maple), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(other_conifer), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_conifer)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(other_hardwood), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = other_hardwood)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(pine), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(spruce), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = spruce)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

diff |>
  dplyr::mutate(data = dplyr::if_else(is.na(tamarack), FALSE, TRUE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tamarack)) +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, color = data),
                     show.legend = FALSE, fill = NA) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                direction = 1,
                                limits = c(-1, 1),
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::scale_color_manual(values = c('#00000000', 'black')) +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

