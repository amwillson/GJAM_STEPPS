### STEP 4-13

## Plotting out-of-sample predictions in space and time
## Out-of-sample prediction for all locations and times not used to fit the model

## Input: data/processed/post_stepps_full.RData
## OOS data

## Input: /Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_nonconditional_final.RData
## OOS predictions for all spatiotemporal locations using non-conditional method

## Input: /Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_conditionaloak_final.RData
## OOS predictions for all spatiotemporal locations using conditional on oak method

## Output: none

rm(list = ls())

source('R/funs.R')

# Load out-of-sample data
load('data/processed/post_stepps_full_oos.RData')

#### Non-conditional prediction ####

# Load out-of-sample predictions
load('/Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_nonconditional_final.RData')

# Map of study region
states <- map_states()

# Loop over posterior draws
for(i in 1:length(pred)){
  # Extract one output for one posterior draw
  temp <- pred[[i]]
  # Extract oos data for one posterior draw
  oos <- dplyr::filter(post_oos_all, draw == i)
  
  # If it's the first one
  if(i == 1){
    # Store prediction mean
    pred_mean <- temp$sdList$yMu
    # Format
    pred_mean <- as.data.frame(pred_mean)
    # Add coordinates
    pred_mean$x <- oos$x
    pred_mean$y <- oos$y
    # Add time
    pred_mean$time <- oos$time
    # Add draw index
    pred_mean$draw <- rep(i, times = nrow(oos))
    # Otherwise add to previous draw
  }else{
    # Same steps but temporary storage
    temp2 <- temp$sdList$yMu
    temp2 <- as.data.frame(temp2)
    temp2$x <- oos$x
    temp2$y <- oos$y
    temp2$time <- oos$time
    temp2$draw <- rep(i, times = nrow(oos))
    # Add to previous draw
    pred_mean <- rbind(pred_mean, temp2)
  }
  print(i)
}

### Plot each taxon's predictive mean over space and time ###

# Time order
time_order <- c('1900 YBP', '1800 YBP', '1700 YBP', '1600 YBP',
                '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                '1100 YBP', '1000 YBP', '900 YBP', '800 YBP',
                '700 YBP', '600 YBP', '500 YBP', '400 YBP', '300 YBP')
## BEECH

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BEECH, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                 name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BEECH, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BIRCH, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BIRCH, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(ELM, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(ELM, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(HEMLOCK, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(HEMLOCK, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(MAPLE, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(MAPLE, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OAK

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OAK, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OAK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OAK, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OC, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OC)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OC, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OH, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OH, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(PINE, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(PINE, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(SPRUCE, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(SPRUCE, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(TAMARACK, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(TAMARACK, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### Difference between observed and predicted: each draw individually ###

# Loop over each draw
for(i in 1:max(pred_mean$draw)){
  temp <- dplyr::filter(pred_mean, draw == i)
  oos <- dplyr::filter(post_oos_all, draw == i)
  if(i == 1){
    diff <- dplyr::select(oos, BEECH:TAMARACK) - dplyr::select(temp, BEECH:TAMARACK)
    diff$x <- temp$x
    diff$y <- temp$y
    diff$time <- temp$time
    diff$draw <- rep(i, times = nrow(oos))
  }else{
    temp2 <- dplyr::select(oos, BEECH:TAMARACK) - dplyr::select(temp, BEECH:TAMARACK)
    temp2$x <- temp$x
    temp2$y <- temp$y
    temp2$time <- temp$time
    temp2$draw <- rep(i, times = nrow(oos))
    diff <- rbind(diff, temp2)
  }
  print(i)
}

# Plot

## BEECH

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BEECH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                 limits = c(-1, 1),
                                 direction = 1,
                                 name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BEECH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BIRCH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BIRCH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(ELM, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(ELM, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(HEMLOCK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(HEMLOCK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(MAPLE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(MAPLE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OAK

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OAK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OAK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OAK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OTHER.CONIFER, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.CONIFER)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OTHER.CONIFER, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OTHER.HARDWOOD, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.HARDWOOD)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OTHER.HARDWOOD, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(PINE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(PINE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(SPRUCE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(SPRUCE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(TAMARACK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(TAMARACK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

#### Conditional on oak ####

rm(pred)

# Load predictions
load('/Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_conditionaloak_final.RData')

# Loop over posterior draws
for(i in 1:length(oak_cond_pred)){
  # Extract one output for one posterior draw
  temp <- oak_cond_pred[[i]]
  # Extract oos data for one posterior draw
  oos <- dplyr::filter(post_oos_all, draw == i)
  
  # If it's the first one
  if(i == 1){
    # Store prediction mean
    pred_mean <- temp$sdList$yMu
    # Format
    pred_mean <- as.data.frame(pred_mean)
    # Add coordinates
    pred_mean$x <- oos$x
    pred_mean$y <- oos$y
    # Add time
    pred_mean$time <- oos$time
    # Add draw index
    pred_mean$draw <- rep(i, times = nrow(oos))
    # Otherwise add to previous draw
  }else{
    # Same steps but temporary storage
    temp2 <- temp$sdList$yMu
    temp2 <- as.data.frame(temp2)
    temp2$x <- oos$x
    temp2$y <- oos$y
    temp2$time <- oos$time
    temp2$draw <- rep(i, times = nrow(oos))
    # Add to previous draw
    pred_mean <- rbind(pred_mean, temp2)
  }
  print(i)
}

### Plot each taxon's predictive mean over space and time ###

## BEECH

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BEECH, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BEECH, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BIRCH, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BIRCH, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(ELM, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(ELM, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(HEMLOCK, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(HEMLOCK, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(MAPLE, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(MAPLE, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OAK

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OAK, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OAK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OAK, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Oak, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OC, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OC)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OC, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OH, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OH, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(PINE, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(PINE, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(SPRUCE, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(SPRUCE, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

# 25% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(TAMARACK, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, 25%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 75% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(TAMARACK, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, 75%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

### Difference between observed and predicted: each draw individually ###

# Loop over each draw
for(i in 1:max(pred_mean$draw)){
  temp <- dplyr::filter(pred_mean, draw == i)
  oos <- dplyr::filter(post_oos_all, draw == i)
  if(i == 1){
    diff <- dplyr::select(oos, BEECH:TAMARACK) - dplyr::select(temp, BEECH:TAMARACK)
    diff$x <- temp$x
    diff$y <- temp$y
    diff$time <- temp$time
    diff$draw <- rep(i, times = nrow(oos))
  }else{
    temp2 <- dplyr::select(oos, BEECH:TAMARACK) - dplyr::select(temp, BEECH:TAMARACK)
    temp2$x <- temp$x
    temp2$y <- temp$y
    temp2$time <- temp$time
    temp2$draw <- rep(i, times = nrow(oos))
    diff <- rbind(diff, temp2)
  }
  print(i)
}

# Plot

## BEECH

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BEECH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BEECH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Beech, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BIRCH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BIRCH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Birch, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(ELM, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(ELM, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Elm, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(HEMLOCK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(HEMLOCK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Hemlock, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(MAPLE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(MAPLE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Maple, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OTHER.CONIFER, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.CONIFER)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OTHER.CONIFER, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Conifer, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OTHER.HARDWOOD, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.HARDWOOD)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OTHER.HARDWOOD, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other Hardwood, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(PINE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(PINE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Pine, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(SPRUCE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(SPRUCE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Spruce, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(TAMARACK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, 2.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, Median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '19', '1900 YBP', time),
                time = dplyr::if_else(time == '18', '1800 YBP', time),
                time = dplyr::if_else(time == '17', '1700 YBP', time),
                time = dplyr::if_else(time == '16', '1600 YBP', time),
                time = dplyr::if_else(time == '15', '1500 YBP', time),
                time = dplyr::if_else(time == '14', '1400 YBP', time),
                time = dplyr::if_else(time == '13', '1300 YBP', time),
                time = dplyr::if_else(time == '12', '1200 YBP', time),
                time = dplyr::if_else(time == '11', '1100 YBP', time),
                time = dplyr::if_else(time == '10', '1000 YBP', time),
                time = dplyr::if_else(time == '9', '900 YBP', time),
                time = dplyr::if_else(time == '8', '800 YBP', time),
                time = dplyr::if_else(time == '7', '700 YBP', time),
                time = dplyr::if_else(time == '6', '600 YBP', time),
                time = dplyr::if_else(time == '5', '500 YBP', time),
                time = dplyr::if_else(time == '4', '400 YBP', time),
                time = dplyr::if_else(time == '3', '300 YBP', time)) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(TAMARACK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Tamarack, 97.5%') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))
