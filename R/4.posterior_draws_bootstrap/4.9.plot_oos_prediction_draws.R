### STEP 4-9

## Plotting out-of-sample predictions in time
## Out-of-sample prediction of 300 YBP for same spatial locations as
## Used to fit the model

## Input: data/processed/mean_stepps_soil_clim.RData
## OOS data

## Input: /Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_nonconditional_time.RData
## OOS predictions for 300 YBP using non-conditional method
## Saved on external drive because of large file size (8 GB)

## Input: /Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_conditionoak_time.RData
## OOS predictions for 300 YBP using conditional on oak method
## Saved on external drive because of large file size (8 GB)

rm(list = ls())

source('R/funs.R')

# Load out-of-sample data
load('data/processed/mean_stepps_soil_clim.RData')

#### Non-conditional prediction ####

# Load out-of-sample predictions
load('/Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_nonconditional_time.RData')

# Map of study region
states <- map_states()

# Loop over posterior draws
for(i in 1:length(pred)){
  # Extract one output for one posterior draw
  temp <- pred[[i]]
  # If it's the first one
  if(i == 1){
    # Store prediction mean
    pred_mean <- temp$sdList$yMu
    # Format
    pred_mean <- as.data.frame(pred_mean)
    # Add coordinates
    pred_mean$x <- taxon_oos_all$stepps_x
    pred_mean$y <- taxon_oos_all$stepps_y
    # Add draw index
    pred_mean$draw <- rep(i, times = nrow(taxon_oos_all))
    # Otherwise add to previous draw
  }else{
    # Same steps but temporary storage
    temp2 <- temp$sdList$yMu
    temp2 <- as.data.frame(temp2)
    temp2$x <- taxon_oos_all$stepps_x
    temp2$y <- taxon_oos_all$stepps_y
    temp2$draw <- rep(i, times = nrow(taxon_oos_all))
    # Add to previous draw
    pred_mean <- rbind(pred_mean, temp2)
  }
  print(i)
}

### Plot each taxon's predictive mean over space ###

## BEECH

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BEECH),
                   low = quantile(BEECH, probs = 0.25),
                   high = quantile(BEECH, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BIRCH),
                   low = quantile(BIRCH, probs = 0.25),
                   high = quantile(BIRCH, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(ELM),
                   low = quantile(ELM, probs = 0.25),
                   high = quantile(ELM, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(HEMLOCK),
                   low = quantile(HEMLOCK, probs = 0.25),
                   high = quantile(HEMLOCK, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(MAPLE),
                   low = quantile(MAPLE, probs = 0.25),
                   high = quantile(MAPLE, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OAK

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OAK),
                   low = quantile(OAK, probs = 0.25),
                   high = quantile(OAK, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OC),
                   low = quantile(OC, probs = 0.25),
                   high = quantile(OC, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OH),
                   low = quantile(OH, probs = 0.25),
                   high = quantile(OH, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(PINE),
                   low = quantile(PINE, probs = 0.25),
                   high = quantile(PINE, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(SPRUCE),
                   low = quantile(SPRUCE, probs = 0.25),
                   high = quantile(SPRUCE, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(TAMARACK),
                   low = quantile(TAMARACK, probs = 0.25),
                   high = quantile(TAMARACK, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### Difference between observed and predicted ###

# Loop over each draw
for(i in 1:max(pred_mean$draw)){
  temp <- dplyr::filter(pred_mean, draw == i)
  if(i == 1){
    diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(temp, BEECH:TAMARACK)
    diff$x <- temp$x
    diff$y <- temp$y
    diff$draw <- rep(i, times = nrow(taxon_oos_all))
  }else{
    temp2 <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(temp, BEECH:TAMARACK)
    temp2$x <- temp$x
    temp2$y <- temp$y
    temp2$draw <- rep(i, times = nrow(taxon_oos_all))
    diff <- rbind(diff, temp2)
  }
}

# Plot

## BEECH

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(beech),
                   low = quantile(beech, probs = 0.25),
                   high = quantile(beech, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(birch),
                   low = quantile(birch, probs = 0.25),
                   high = quantile(birch, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(elm),
                   low = quantile(elm, probs = 0.25),
                   high = quantile(elm, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(hemlock),
                   low = quantile(hemlock, probs = 0.25),
                   high = quantile(hemlock, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(maple),
                   low = quantile(maple, probs = 0.25),
                   high = quantile(maple, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OAK

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(oak),
                   low = quantile(oak, probs = 0.25),
                   high = quantile(oak, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(other_conifer),
                   low = quantile(other_conifer, probs = 0.25),
                   high = quantile(other_conifer, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(other_hardwood),
                   low = quantile(other_hardwood, probs = 0.25),
                   high = quantile(other_hardwood, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(pine),
                   low = quantile(pine, probs = 0.25),
                   high = quantile(pine, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(spruce),
                   low = quantile(spruce, probs = 0.25),
                   high = quantile(spruce, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(tamarack),
                   low = quantile(tamarack, probs = 0.25),
                   high = quantile(tamarack, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

#### Conditional on oak ####

rm(pred)

# Load predictions
load('/Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_conditionoak_time.RData')

# Loop over posterior draws
for(i in 1:length(oak_cond_pred)){
  # Extract one output for one posterior draw
  temp <- oak_cond_pred[[i]]
  # If it's the first one
  if(i == 1){
    # Store prediction mean
    pred_mean <- temp$sdList$yMu
    # Format
    pred_mean <- as.data.frame(pred_mean)
    # Add coordinates
    pred_mean$x <- taxon_oos_all$stepps_x
    pred_mean$y <- taxon_oos_all$stepps_y
    # Add draw index
    pred_mean$draw <- rep(i, times = nrow(taxon_oos_all))
    # Otherwise add to previous draw
  }else{
    # Same steps but temporary storage
    temp2 <- temp$sdList$yMu
    temp2 <- as.data.frame(temp2)
    temp2$x <- taxon_oos_all$stepps_x
    temp2$y <- taxon_oos_all$stepps_y
    temp2$draw <- rep(i, times = nrow(taxon_oos_all))
    # Add to previous draw
    pred_mean <- rbind(pred_mean, temp2)
  }
  print(i)
}

### Plot each taxon's predictive mean over space ###

## BEECH

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BEECH),
                   low = quantile(BEECH, probs = 0.25),
                   high = quantile(BEECH, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BIRCH),
                   low = quantile(BIRCH, probs = 0.25),
                   high = quantile(BIRCH, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(ELM),
                   low = quantile(ELM, probs = 0.25),
                   high = quantile(ELM, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(HEMLOCK),
                   low = quantile(HEMLOCK, probs = 0.25),
                   high = quantile(HEMLOCK, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(MAPLE),
                   low = quantile(MAPLE, probs = 0.25),
                   high = quantile(MAPLE, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OC),
                   low = quantile(OC, probs = 0.25),
                   high = quantile(OC, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OH),
                   low = quantile(OH, probs = 0.25),
                   high = quantile(OH, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(PINE),
                   low = quantile(PINE, probs = 0.25),
                   high = quantile(PINE, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(SPRUCE),
                   low = quantile(SPRUCE, probs = 0.25),
                   high = quantile(SPRUCE, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

pred_mean |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(TAMARACK),
                   low = quantile(TAMARACK, probs = 0.25),
                   high = quantile(TAMARACK, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### Difference between observed and predicted ###

# Loop over each draw
for(i in 1:max(pred_mean$draw)){
  temp <- dplyr::filter(pred_mean, draw == i)
  if(i == 1){
    diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(temp, BEECH:TAMARACK)
    diff$x <- temp$x
    diff$y <- temp$y
    diff$draw <- rep(i, times = nrow(taxon_oos_all))
  }else{
    temp2 <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(temp, BEECH:TAMARACK)
    temp2$x <- temp$x
    temp2$y <- temp$y
    temp2$draw <- rep(i, times = nrow(taxon_oos_all))
    diff <- rbind(diff, temp2)
  }
}

# Plot

## BEECH

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(beech),
                   low = quantile(beech, probs = 0.25),
                   high = quantile(beech, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(birch),
                   low = quantile(birch, probs = 0.25),
                   high = quantile(birch, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(elm),
                   low = quantile(elm, probs = 0.25),
                   high = quantile(elm, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(hemlock),
                   low = quantile(hemlock, probs = 0.25),
                   high = quantile(hemlock, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(maple),
                   low = quantile(maple, probs = 0.25),
                   high = quantile(maple, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(other_conifer),
                   low = quantile(other_conifer, probs = 0.25),
                   high = quantile(other_conifer, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(other_hardwood),
                   low = quantile(other_hardwood, probs = 0.25),
                   high = quantile(other_hardwood, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(pine),
                   low = quantile(pine, probs = 0.25),
                   high = quantile(pine, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(spruce),
                   low = quantile(spruce, probs = 0.25),
                   high = quantile(spruce, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(tamarack),
                   low = quantile(tamarack, probs = 0.25),
                   high = quantile(tamarack, probs = 0.75)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = value), shape = 15, size = 6) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_color_gradient2(low = 'red',
                                 high = 'darkblue',
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))