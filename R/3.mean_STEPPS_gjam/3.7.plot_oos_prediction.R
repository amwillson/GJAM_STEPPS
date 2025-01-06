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

pdf(file = 'figures/mean/oos_prediction/non_conditional_prediction.pdf',
    width = 8.5, height = 4.5)

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

dev.off()

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

pdf(file = 'figures/mean/oos_prediction/non_conditional_difference.pdf',
    width = 8.5, height = 4.5)

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

dev.off()

### Plot observed vs predicted irrespective of space/time ###

## If the model is doing a good job predicting relative abundances,
## then we would expect that the points would generally fall around
## the 1:1 line. If the model produces biased predictions,
## then we would see that the points generally fall either above
## or below the 1:1 line

pred_mean_long <- pred_mean2 |>
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
  # select relevant columns
  dplyr::select(x:tamarack) |>
  # remove ash taxon
  dplyr::select(-ash) |>
  # pivot observations longer
  tidyr::pivot_longer(cols = beech:tamarack,
                      names_to = 'taxon',
                      values_to = 'Observed') |>
  # format
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'other_conifer', 'other conifer', taxon),
                taxon = dplyr::if_else(taxon == 'other_hardwood', 'other hardwood', taxon)) |>
  dplyr::mutate(taxon = stringr::str_to_title(taxon))

# Combine
pred_obs_long <- pred_mean_long |>
  dplyr::full_join(y = obs_long,
                   by = c('x', 'y', 'taxon'))

pred_obs_long |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Observed, y = Predicted)) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/mean/oos_prediction/predicted_observed_nonconditional.png',
                width = 20, height = 10, units = 'cm')

### Correlations between observed and predicted ###

# Pivot wider to calculate densities
pred_obs_wide <- pred_obs_long |>
  dplyr::select(taxon, Predicted, Observed, x, y) |>
  tidyr::drop_na() |>
  tidyr::pivot_wider(names_from = 'taxon',
                     values_from = c('Predicted', 'Observed'))

cor_beech <- cor(pred_obs_wide$Predicted_Beech, pred_obs_wide$Observed_Beech)
cor_birch <- cor(pred_obs_wide$Predicted_Birch, pred_obs_wide$Observed_Birch)
cor_elm <- cor(pred_obs_wide$Predicted_Elm, pred_obs_wide$Observed_Elm)
cor_hemlock <- cor(pred_obs_wide$Predicted_Hemlock, pred_obs_wide$Observed_Hemlock)
cor_maple <- cor(pred_obs_wide$Predicted_Maple, pred_obs_wide$Observed_Maple)
cor_oak <- cor(pred_obs_wide$Predicted_Oak, pred_obs_wide$Observed_Oak)
cor_oc <- cor(pred_obs_wide$`Predicted_Other Conifer`, pred_obs_wide$`Observed_Other Conifer`)
cor_oh <- cor(pred_obs_wide$`Predicted_Other Hardwood`, pred_obs_wide$`Observed_Other Hardwood`)
cor_pine <- cor(pred_obs_wide$Predicted_Pine, pred_obs_wide$Observed_Pine)
cor_spruce <- cor(pred_obs_wide$Predicted_Spruce, pred_obs_wide$Observed_Spruce)
cor_tamarack <- cor(pred_obs_wide$Predicted_Tamarack, pred_obs_wide$Observed_Tamarack)

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

### Plot observed vs predicted irrespective of space/time ###

pred_cond_long <- pred_cond2 |>
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
pred_obs_cond_long <- pred_cond_long |>
  dplyr::full_join(y = obs_long,
                   by = c('x', 'y', 'taxon'))

pred_obs_cond_long |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Observed, y = Predicted)) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/mean/oos_prediction/predicted_observed_conditional.png',
                width = 20, height = 10, units = 'cm')

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

