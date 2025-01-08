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
load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/oos_prediction_nonconditional_final.RData')

# Rename
pred <- pred_all

# Remove original
rm(pred_all)

# Map of study region
states <- map_states()

# Loop over posterior draws
for(i in 1:length(pred)){
  # Extract one output for one posterior draw
  temp <- pred[[i]]
  # Extract oos data for one posterior draw
  oos <- dplyr::filter(post_oos_all, draw == i)
  # Without missing data
  oos2 <- tidyr::drop_na(oos)
  
  # If it's the first one
  if(i == 1){
    # Store prediction mean
    pred_mean <- temp$sdList$yMu
    # Format
    pred_mean <- as.data.frame(pred_mean)
    # Add coordinates
    pred_mean$x <- oos2$x
    pred_mean$y <- oos2$y
    # Add time
    pred_mean$time <- oos2$time
    
    # Add to full grid (with missing data)
    pred_mean2 <- oos |>
      dplyr::select(x, y, time) |>
      dplyr::left_join(y = pred_mean, by = c('x', 'y', 'time'))
    
    # Add draw index
    pred_mean2$draw <- rep(i, times = nrow(pred_mean2))
    
    # Otherwise add to previous draw
  }else{
    # Same steps but temporary storage
    temp2 <- temp$sdList$yMu
    temp2 <- as.data.frame(temp2)
    temp2$x <- oos2$x
    temp2$y <- oos2$y
    temp2$time <- oos2$time
    
    temp3 <- oos |>
      dplyr::select(x, y, time) |>
      dplyr::left_join(y = temp2, by = c('x', 'y', 'time'))
    
    temp3$draw <- rep(i, times = nrow(temp3))
    
    # Add to previous draw
    pred_mean2 <- rbind(pred_mean2, temp3)
  }
  print(i)
}

### Plot each taxon's prediction over space and time ###

## Predictive mean is what the model on average
## predicts the relative abundance of each taxon
## to be, given the environment

## Note that we are plotting the median and 50% CrI 
## of predictive mean. This is over the 100 posterior
## draws of STEPPS
## Plotting median and CrI in separate figures
## to maximize the size of facets representing individual
## time intervals in each figure

# Time order
time_order <- c('1900 YBP', '1800 YBP', '1700 YBP', '1600 YBP',
                '1500 YBP', '1400 YBP', '1300 YBP', '1200 YBP',
                '1100 YBP', '1000 YBP', '900 YBP', '800 YBP',
                '700 YBP', '600 YBP', '500 YBP', '400 YBP', '300 YBP')

# Open pdf
pdf(file = 'figures/posteriors/oos_prediction/full_non_conditional_prediction.pdf',
    width = 8.5, height = 4.5)

## BEECH

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BEECH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), 2.',5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#0000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BEECH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## BIRCH

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BIRCH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BIRCH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## ELM

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(ELM, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ') 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(ELM, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## HEMLOCK

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(HEMLOCK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(HEMLOCK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## MAPLE

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(MAPLE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(MAPLE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OAK

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OAK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OAK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OAK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER CONIFER

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OC, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OC)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OC, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## OTHER HARDWOOD

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## PINE

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(PINE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(PINE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## SPRUCE

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(SPRUCE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(SPRUCE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

## TAMARACK

# 2.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(TAMARACK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# Median

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

# 97.5% CI

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(TAMARACK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12))

dev.off()

## Median plots for main text

## BEECH

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#0000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Predicted beech (', italic('Fagus grandifolia'), ') abundance'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_beech.png',
                height = 12, width = 18, units = 'cm')

## BIRCH

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Predicted birch (', italic('Betula spp.'), ') abundance'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_birch.png',
                height = 12, width = 18, units = 'cm')
## ELM

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Predicted elm (', italic('Ulmus spp.'), ') abundance'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_elm.png',
                height = 12, width = 18, units = 'cm')

## HEMLOCK

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Predicted hemlock (', italic('Tsuga canadensis'), ') abundance'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_hemlock.png',
                height = 12, width = 18, units = 'cm')

## MAPLE

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Predicted maple (', italic('Acer spp.'), ') abundance'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_maple.png',
                height = 12, width = 18, units = 'cm')

## OAK

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OAK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Predicted oak (', italic('Quercus spp.'), ') abundance'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_oak.png',
                height = 12, width = 18, units = 'cm')

## OTHER CONIFER

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OC)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Predicted other conifer taxa abundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_oc.png',
                height = 12, width = 18, units = 'cm')

## OTHER HARDWOOD

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Predicted other hardwood taxa abundance') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_oh.png',
                height = 12, width = 18, units = 'cm')

## PINE

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Predicted pine (', italic('Pinus spp.'), ') abundance'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_pine.png',
                height = 12, width = 18, units = 'cm')

## SPRUCE

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Predicted spruce (', italic('Picea spp.'), ') abundance'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_spruce.png',
                height = 12, width = 18, units = 'cm')

## TAMARACK

pred_mean2 |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Predicted tamarack (', italic('Larix laricina'), ') abundance'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_pred_tamarack.png',
                height = 12, width = 18, units = 'cm')

### Plot observed vs predicted irrespective of space/time ###

## If the model is doing a good job predicting relative abundances,
## then we would expect that the points would generally fall around
## the 1:1 line. If the model produces biased predictions,
## then we would see that the points generally fall either above
## or below the 1:1 line

pred_mean_long <- pred_mean2 |>
  # pivot predictions longer
  tidyr::pivot_longer(cols = BEECH:TAMARACK,
                      names_to = 'taxon',
                      values_to = 'Predicted') |>
  # rename other conifer and other hardwood
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'OC', 'other conifer', taxon),
                taxon = dplyr::if_else(taxon == 'OH', 'other hardwood', taxon)) |>
  # format
  dplyr::mutate(taxon = stringr::str_to_title(taxon)) |>
  # group for summarizing
  dplyr::group_by(x, y, time, taxon) |>
  # summarize over draws
  dplyr::summarize(predicted_mean = mean(Predicted),
                   predicted_sd = sd(Predicted),
                   predicted_quant_2.5 = quantile(Predicted, probs = 0.025),
                   predicted_quant_25 = quantile(Predicted, probs = 0.25),
                   predicted_quant_75 = quantile(Predicted, probs = 0.75),
                   predicted_quant_975 = quantile(Predicted, probs = 0.975))

obs_long <- post_oos_all |>
  # select relevant columns
  dplyr::select(x:TAMARACK) |>
  # remove ash taxon
  dplyr::select(-ASH) |>
  # pivot observations longer
  tidyr::pivot_longer(cols = BEECH:TAMARACK,
                      names_to = 'taxon',
                      values_to = 'Observed') |>
  # format
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'OTHER.CONIFER', 'other conifer', taxon),
                taxon = dplyr::if_else(taxon == 'OTHER.HARDWOOD', 'other hardwood', taxon)) |>
  dplyr::mutate(taxon = stringr::str_to_title(taxon)) |>
  # group for summarizing
  dplyr::group_by(x, y, time, taxon) |>
  # summarize over draws
  dplyr::summarize(observed_mean = mean(Observed),
                   observed_sd = sd(Observed),
                   observed_quant_2.5 = quantile(Observed, probs = 0.025),
                   observed_quant_25 = quantile(Observed, probs = 0.25),
                   observed_quant_75 = quantile(Observed, probs = 0.75),
                   observed_quant_975 = quantile(Observed, probs = 0.975))

# Combine
pred_obs_long <- pred_mean_long |>
  dplyr::full_join(y = obs_long,
                   by = c('x', 'y', 'time', 'taxon'))

## BEECH - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Beech') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue', linetype = 'dashed') +
  ggplot2::xlim(c(0, 0.7)) + ggplot2::ylim(c(0, 0.7)) +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## BIRCH - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Birch') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlim(c(0, 0.5)) + ggplot2::ylim(c(0, 0.5)) +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## ELM - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Elm') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.65)) + ggplot2::ylim(c(0, 0.65)) +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## HEMLOCK - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Hemlock') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.7)) + ggplot2::ylim(c(0, 0.7)) +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## MAPLE - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Maple') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.4)) + ggplot2::ylim(c(0, 0.4)) +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## OAK - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Oak') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## OTHER CONIFER - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Other Conifer') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.6)) + ggplot2::ylim(c(0, 0.6)) +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## OTHER HARDWOOD - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Other Hardwood') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.5)) + ggplot2::ylim(c(0, 0.5)) +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## PINE - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Pine') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## SPRUCE - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Spruce') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.5)) + ggplot2::ylim(c(0, 0.5)) +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## TAMARACK - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Tamarack') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## ALL - mean +/- sd

pred_obs_long |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd)) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd)) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## BEECH - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Beech') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## BIRCH  - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Birch') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.65)) + ggplot2::ylim(c(0, 0.65)) +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## ELM  - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Elm') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## HEMLOCK  - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Hemlock') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## MAPLE - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Maple') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.6)) + ggplot2::ylim(c(0, 0.6)) +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## OAK - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Oak') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## OTHER CONIFER - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Other Conifer') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## OTHER HARDWOOD - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Other Hardwood') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.6)) + ggplot2::ylim(c(0, 0.6)) +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## PINE - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Pine') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## SPRUCE - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Spruce') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.65)) + ggplot2::ylim(c(0, 0.65)) +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## TAMARACK - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Tamarack') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## ALL - mean [95% CI]

pred_obs_long |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## Mean predicted vs mean observed with colored density ##

# Color points by the number of points in a given region of the plot
# This function gets the relative density of points
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

# Pivot wider to calculate densities
pred_obs_wide <- pred_obs_long |>
  dplyr::select(taxon, predicted_mean, observed_mean) |>
  tidyr::pivot_wider(names_from = 'taxon',
                     values_from = c('observed_mean', 'predicted_mean'))

# Get density for each taxon
pred_obs_wide$density_beech <- get_density(pred_obs_wide$predicted_mean_Beech, pred_obs_wide$observed_mean_Beech, n = 100)
pred_obs_wide$density_birch <- get_density(pred_obs_wide$predicted_mean_Birch, pred_obs_wide$observed_mean_Birch, n = 100)
pred_obs_wide$density_elm <- get_density(pred_obs_wide$predicted_mean_Elm, pred_obs_wide$observed_mean_Elm, n = 100)
pred_obs_wide$density_hemlock <- get_density(pred_obs_wide$predicted_mean_Hemlock, pred_obs_wide$observed_mean_Hemlock, n = 100)
pred_obs_wide$density_maple <- get_density(pred_obs_wide$predicted_mean_Maple, pred_obs_wide$observed_mean_Maple, n = 100)
pred_obs_wide$density_oak <- get_density(pred_obs_wide$predicted_mean_Oak, pred_obs_wide$observed_mean_Oak, n = 100)
pred_obs_wide$density_oc <- get_density(pred_obs_wide$`predicted_mean_Other Conifer`, pred_obs_wide$`observed_mean_Other Conifer`, n = 100)
pred_obs_wide$density_oh <- get_density(pred_obs_wide$`predicted_mean_Other Hardwood`, pred_obs_wide$`observed_mean_Other Hardwood`, n = 100)
pred_obs_wide$density_pine <- get_density(pred_obs_wide$predicted_mean_Pine, pred_obs_wide$observed_mean_Pine, n = 100)
pred_obs_wide$density_spruce <- get_density(pred_obs_wide$predicted_mean_Spruce, pred_obs_wide$observed_mean_Spruce, n = 100)
pred_obs_wide$density_tamarack <- get_density(pred_obs_wide$predicted_mean_Tamarack, pred_obs_wide$observed_mean_Tamarack, n = 100)

# Plot predicted vs observed with point density colored
# this helps because there are so many points that it's not
# possible to see point density with different levels of opacity
pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean_Beech, y = predicted_mean_Beech, fill = density_beech),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = observed_mean_Beech, y = predicted_mean_Beech),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Beech') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_beech.png',
                width = 10, height = 10, units = 'cm')

pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean_Birch, y = predicted_mean_Birch, fill = density_birch),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = observed_mean_Birch, y = predicted_mean_Birch),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_minimal() +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_birch.png',
                width = 10, height = 10, units = 'cm')

pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean_Elm, y = predicted_mean_Elm, fill = density_elm),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = observed_mean_Elm, y = predicted_mean_Elm),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Elm') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_elm.png',
                width = 10, height = 10, units = 'cm')

pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean_Hemlock, y = predicted_mean_Hemlock, fill = density_hemlock),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = observed_mean_Hemlock, y = predicted_mean_Hemlock),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_hemlock.png',
                width = 10, height = 10, units = 'cm')

pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean_Maple, y = predicted_mean_Maple, fill = density_maple),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = observed_mean_Maple, y = predicted_mean_Maple),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Maple') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_maple.png',
                width = 10, height = 10, units = 'cm')

pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean_Oak, y = predicted_mean_Oak, fill = density_oak),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = observed_mean_Oak, y = predicted_mean_Oak),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Oak') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_oak.png',
                width = 10, height = 10, units = 'cm')

pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = `observed_mean_Other Conifer`, y = `predicted_mean_Other Conifer`, fill = density_oc),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = `observed_mean_Other Conifer`, y = `predicted_mean_Other Conifer`),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_oc.png',
                width = 10, height = 10, units = 'cm')

pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = `observed_mean_Other Hardwood`, y = `predicted_mean_Other Hardwood`, fill = density_oc),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = `observed_mean_Other Hardwood`, y = `predicted_mean_Other Hardwood`),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_oh.png',
                width = 10, height = 10, units = 'cm')

pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean_Pine, y = predicted_mean_Pine, fill = density_pine),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = observed_mean_Pine, y = predicted_mean_Pine),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Pine') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_pine.png',
                width = 10, height = 10, units = 'cm')

pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean_Spruce, y = predicted_mean_Spruce, fill = density_spruce),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = observed_mean_Spruce, y = predicted_mean_Spruce),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Spruce') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_spruce.png',
                width = 10, height = 10, units = 'cm')

pred_obs_wide |>
  dplyr::filter(time == 3) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean_Tamarack, y = predicted_mean_Tamarack, fill = density_tamarack),
                      shape = 21, color = '#00000020', show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = observed_mean_Tamarack, y = predicted_mean_Tamarack),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_tamarack.png',
                width = 10, height = 10, units = 'cm')

### Overall correlations between observation and prediction ###

# Make copies of dataframes
pred_mean3 <- pred_mean2
obs_3 <- dplyr::select(post_oos_all,
                       x:TAMARACK) |>
  dplyr::select(-ASH)

# Change column names
colnames(pred_mean3) <- c('x', 'y', 'time', 'beech_pred',
                          'birch_pred', 'elm_pred',
                          'hemlock_pred', 'maple_pred',
                          'oak_pred', 'oc_pred', 'oh_pred',
                          'pine_pred', 'spruce_pred',
                          'tamarack_pred', 'draw')
colnames(obs_3) <- c('x', 'y', 'time', 'draw',
                     'beech_obs', 'birch_obs',
                     'elm_obs', 'hemlock_obs',
                     'maple_obs', 'oak_obs',
                     'oc_obs', 'oh_obs', 'pine_obs',
                     'spruce_obs', 'tamarack_obs')

# Combine
pred_obs <- pred_mean3 |>
  dplyr::left_join(y = obs_3,
                   by = c('x', 'y', 'time', 'draw'))

# Initialization
beech_cors <- c()
birch_cors <- c()
elm_cors <- c()
hemlock_cors <- c()
maple_cors <- c()
oak_cors <- c()
oc_cors <- c()
oh_cors <- c()
pine_cors <- c()
spruce_cors <- c()
tamarack_cors <- c()

# Loop over all draws
for(i in 1:100){
  temp <- dplyr::filter(pred_obs, draw == i)
  beech_cors[i] <- cor(temp$beech_obs, temp$beech_pred)
  birch_cors[i] <- cor(temp$birch_obs, temp$birch_pred)
  elm_cors[i] <- cor(temp$elm_obs, temp$elm_pred)
  hemlock_cors[i] <- cor(temp$hemlock_obs, temp$hemlock_pred)
  maple_cors[i] <- cor(temp$maple_obs, temp$maple_pred)
  oak_cors[i] <- cor(temp$oak_obs, temp$oak_pred)
  oc_cors[i] <- cor(temp$oc_obs, temp$oc_pred)
  oh_cors[i] <- cor(temp$oh_obs, temp$oh_pred)
  pine_cors[i] <- cor(temp$pine_obs, temp$pine_pred)
  spruce_cors[i] <- cor(temp$spruce_obs, temp$spruce_pred)
  tamarack_cors[i] <- cor(temp$tamarack_obs, temp$tamarack_pred)
}

# Calculate summary statistics
beech_cor_quant <- quantile(beech_cors, probs = c(0.025, 0.5, 0.975))
birch_cor_quant <- quantile(birch_cors, probs = c(0.025, 0.5, 0.975))
elm_cor_quant <- quantile(elm_cors, probs = c(0.025, 0.5, 0.975))
hemlock_cor_quant <- quantile(hemlock_cors, probs = c(0.025, 0.5, 0.975))
maple_cor_quant <- quantile(maple_cors, probs = c(0.025, 0.5, 0.975))
oak_cor_quant <- quantile(oak_cors, probs = c(0.025, 0.5, 0.975))
oc_cor_quant <- quantile(oc_cors, probs = c(0.025, 0.5, 0.975))
oh_cor_quant <- quantile(oh_cors, probs = c(0.025, 0.5, 0.975))
pine_cor_quant <- quantile(pine_cors, probs = c(0.025, 0.5, 0.975))
spruce_cor_quant <- quantile(spruce_cors, probs = c(0.025, 0.5, 0.975))
tamarack_cor_quant <- quantile(tamarack_cors, probs = c(0.025, 0.5, 0.975))

cor_quants <- rbind(beech_cor_quant,
                    birch_cor_quant,
                    elm_cor_quant,
                    hemlock_cor_quant,
                    maple_cor_quant,
                    oak_cor_quant,
                    oc_cor_quant,
                    oh_cor_quant,
                    pine_cor_quant,
                    spruce_cor_quant,
                    tamarack_cor_quant)

cor_quants <- as.data.frame(cor_quants)

cor_quants <- tibble::rownames_to_column(cor_quants)

tibble::tibble(cor_quants)

### Difference between observed and predicted: each draw individually ###

# Loop over each draw
for(i in 1:max(pred_mean2$draw)){
  temp <- dplyr::filter(pred_mean2, draw == i)
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

# Open pdf
pdf(file = 'figures/posteriors/oos_prediction/full_non_conditional_diff.pdf',
    width = 8.5, height = 4.5)

# Plot

## BEECH

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BEECH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BEECH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BIRCH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BIRCH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(ELM, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(ELM, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(HEMLOCK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(HEMLOCK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(MAPLE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(MAPLE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OAK

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OAK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OAK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OAK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OTHER.CONIFER, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.CONIFER)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OTHER.CONIFER, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OTHER.HARDWOOD, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.HARDWOOD)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OTHER.HARDWOOD, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(PINE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(PINE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(SPRUCE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(SPRUCE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#0000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(TAMARACK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(TAMARACK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

dev.off()

## Median plots for main text

## BEECH

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_beech.png',
                height = 12, width = 18, units = 'cm')

## BIRCH

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_birch.png',
                height = 12, width = 18, units = 'cm')

## ELM

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_elm.png',
                height = 12, width = 18, units = 'cm')

## HEMLOCK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_hemlock.png',
                height = 12, width = 18, units = 'cm')

## MAPLE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_maple.png',
                height = 12, width = 18, units = 'cm')

## OAK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OAK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_oak.png',
                height = 12, width = 18, units = 'cm')

## OTHER CONIFER

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.CONIFER)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_oc.png',
                height = 12, width = 18, units = 'cm')

## OTHER HARDWOOD

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.HARDWOOD)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa median prediction bias') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_oh.png',
                height = 12, width = 18, units = 'cm')

## PINE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_pine.png',
                height = 12, width = 18, units = 'cm')

## SPRUCE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_spruce.png',
                height = 12, width = 18, units = 'cm')

## TAMARACK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_tamarack.png',
                height = 12, width = 18, units = 'cm')

## Median plots with bias overlapping zero set to zero

## BEECH

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH),
                   low = quantile(BEECH, probs = 0.025),
                   high = quantile(BEECH, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_beech_CI.png',
                height = 12, width = 18, units = 'cm')

## BIRCH

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH),
                   low = quantile(BIRCH, probs = 0.025),
                   high = quantile(BIRCH, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_birch_CI.png',
                height = 12, width = 18, units = 'cm')

## ELM

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM),
                   low = quantile(ELM, probs = 0.025),
                   high = quantile(ELM, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_elm_CI.png',
                height = 12, width = 18, units = 'cm')

## HEMLOCK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK),
                   low = quantile(HEMLOCK, probs = 0.025),
                   high = quantile(HEMLOCK, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_hemlock_CI.png',
                height = 12, width = 18, units = 'cm')

## MAPLE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE),
                   low = quantile(MAPLE, probs = 0.025),
                   high = quantile(MAPLE, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_maple_CI.png',
                height = 12, width = 18, units = 'cm')

## OAK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OAK),
                   low = quantile(OAK, probs = 0.025),
                   high = quantile(OAK, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_oak_CI.png',
                height = 12, width = 18, units = 'cm')

## OTHER CONIFER

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.CONIFER),
                   low = quantile(OTHER.CONIFER, probs = 0.025),
                   high = quantile(OTHER.CONIFER, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_oc_CI.png',
                height = 12, width = 18, units = 'cm')

## OTHER HARDWOOD

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.HARDWOOD),
                   low = quantile(OTHER.HARDWOOD, probs = 0.025),
                   high = quantile(OTHER.HARDWOOD, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa median prediction bias') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_oh_CI.png',
                height = 12, width = 18, units = 'cm')

## PINE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE),
                   low = quantile(PINE, probs = 0.025),
                   high = quantile(PINE, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_pine_CI.png',
                height = 12, width = 18, units = 'cm')

## SPRUCE

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE),
                   low = quantile(SPRUCE, probs = 0.025),
                   high = quantile(SPRUCE, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_spruce_CI.png',
                height = 12, width = 18, units = 'cm')

## TAMARACK

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK),
                   low = quantile(TAMARACK, probs = 0.025),
                   high = quantile(TAMARACK, probs = 0.975)) |>
  dplyr::mutate(median = dplyr::if_else(low < 0 & high > 0, 0, median)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ') median prediction bias'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_tamarack_CI.png',
                height = 12, width = 18, units = 'cm')

### New section with observed vs predicted plots ###
### and bias density plots together ###

## BEECH

p1_beech <- pred_obs_long |>
  dplyr::filter(taxon == 'Beech') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_beech  

p2_beech <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = BEECH)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_beech

p1_beech + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_beech),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_beech.png',
                height = 12, width = 18, units = 'cm')

## BIRCH

p1_birch <- pred_obs_long |>
  dplyr::filter(taxon == 'Birch') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_birch 

p2_birch <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = BIRCH)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_birch

p1_birch + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_birch),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_birch.png',
                height = 12, width = 18, units = 'cm')

## ELM
p1_elm <- pred_obs_long |>
  dplyr::filter(taxon == 'Elm') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_elm  

p2_elm <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = ELM)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_elm

p1_elm + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_elm),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_elm.png',
                height = 12, width = 18, units = 'cm')

## HEMLOCK
p1_hemlock <- pred_obs_long |>
  dplyr::filter(taxon == 'Hemlock') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_hemlock

p2_hemlock <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = HEMLOCK)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_hemlock

p1_hemlock + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_hemlock),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_hemlock.png',
                height = 12, width = 18, units = 'cm')

## MAPLE
p1_maple <- pred_obs_long |>
  dplyr::filter(taxon == 'Maple') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_maple

p2_maple <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = MAPLE)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_maple

p1_maple + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_maple),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_maple.png',
                height = 12, width = 18, units = 'cm')

## OAK
p1_oak <- pred_obs_long |>
  dplyr::filter(taxon == 'Oak') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_oak

p2_oak <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = OAK)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_oak

p1_oak + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_oak),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_oak.png',
                height = 12, width = 18, units = 'cm')

## OTHER CONIFER
p1_oc <- pred_obs_long |>
  dplyr::filter(taxon == 'Other Conifer') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_oc

p2_oc <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = OTHER.CONIFER)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_oc

p1_oc + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_oc),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_oc.png',
                height = 12, width = 18, units = 'cm')

## OTHER HARDWOOD
p1_oh <- pred_obs_long |>
  dplyr::filter(taxon == 'Other Hardwood') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_oh

p2_oh <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = OTHER.HARDWOOD)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_oh

p1_oh + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_oh),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_oh.png',
                height = 12, width = 18, units = 'cm')

## PINE
p1_pine <- pred_obs_long |>
  dplyr::filter(taxon == 'Pine') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_pine

p2_pine <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = PINE)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_pine

p1_pine + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_pine),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_pine.png',
                height = 12, width = 18, units = 'cm')

## SPRUCE
p1_spruce <- pred_obs_long |>
  dplyr::filter(taxon == 'Spruce') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_spruce

p2_spruce <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = SPRUCE)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_spruce

p1_spruce + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_spruce),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_spruce.png',
                height = 12, width = 18, units = 'cm')

## TAMARACK
p1_tamarack <- pred_obs_long |>
  dplyr::filter(taxon == 'Tamarack') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean,
                                   y = predicted_mean),
                      alpha = 0.5) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))
p1_tamarack

p2_tamarack <- diff |>
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = TAMARACK)) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), color = 'maroon') +
  ggplot2::xlim(c(-1, 1)) +
  ggplot2::xlab('Prediction bias') +
  ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white'),
                 panel.grid = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 8),
                 axis.text = ggplot2::element_text(size = 8))
p2_tamarack

p1_tamarack + 
  ggplot2::annotation_custom(ggplot2::ggplotGrob(p2_tamarack),
                             xmin = -0.05, xmax = 0.4,
                             ymin = 0.57, ymax = 1.05)

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_predvobs_inset_tamarack.png',
                height = 12, width = 18, units = 'cm')

## Proportion of grid cells with bias > |0.1| (> 10% difference)
beech_prop <- length(which(abs(diff$BEECH) > 0.1)) / nrow(diff)
birch_prop <- length(which(abs(diff$BIRCH) > 0.1)) / nrow(diff)
elm_prop <- length(which(abs(diff$ELM) > 0.1)) / nrow(diff)
hemlock_prop <- length(which(abs(diff$HEMLOCK) > 0.1)) / nrow(diff)
maple_prop <- length(which(abs(diff$MAPLE) > 0.1)) / nrow(diff)
oak_prop <- length(which(abs(diff$OAK) > 0.1)) / nrow(diff)
oc_prop <- length(which(abs(diff$OTHER.CONIFER) > 0.1)) / nrow(diff)
oh_prop <- length(which(abs(diff$OTHER.HARDWOOD) > 0.1)) / nrow(diff)
pine_prop <- length(which(abs(diff$PINE) > 0.1)) / nrow(diff)
spruce_prop <- length(which(abs(diff$SPRUCE) > 0.1)) / nrow(diff)
tamarack_prop <- length(which(abs(diff$TAMARACK) > 0.1)) / nrow(diff)

props <- c(beech_prop, birch_prop, elm_prop,
           hemlock_prop, maple_prop, oak_prop,
           oc_prop, oh_prop, pine_prop,
           spruce_prop, tamarack_prop)
taxa <- c('Beech', 'Birch', 'Elm', 'Hemlock',
          'Maple', 'Oak', 'Other conifer',
          'Other hardwood', 'Pine', 'Spruce',
          'Tamarack')

props <- as.data.frame(cbind(taxa, props))
tibble::tibble(props)

#### Conditional on oak ####

rm(pred)

# Load predictions
load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/oos_prediction_conditionoak_final.RData')

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

# Open pdf
pdf(file = 'figures/posteriors/oos_prediction/full_oak_conditional_prediction.pdf',
    width = 8.5, height = 4.5)

## BEECH

# 2.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BEECH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BEECH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

# 2.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BIRCH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BIRCH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

# 2.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(ELM, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(ELM, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

# 2.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(HEMLOCK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(HEMLOCK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

# 2.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(MAPLE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(MAPLE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

# 2.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OC, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OC)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa, posterior median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OC, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

# 2.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa, posterior median') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OH, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

# 2.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(PINE, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(PINE, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

# 2.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(SPRUCE, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(SPRUCE, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

# 2.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(TAMARACK, probs = 0.25)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), 2.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), posterior median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

pred_mean |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(TAMARACK, probs = 0.75)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'Greens', 
                                direction = 1,
                                name = 'Relative\nabundance',
                                limits = c(0, 1),
                                transform = 'sqrt',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), 97.', 5^{th}, ' posterior percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

dev.off()

### Plot observed vs predicted irrespective of space/time ###

pred_mean_long <- pred_mean |>
  # pivot predictions longer
  tidyr::pivot_longer(cols = BEECH:TAMARACK,
                      names_to = 'taxon',
                      values_to = 'Predicted') |>
  # rename other conifer and other hardwood
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'OC', 'other conifer', taxon),
                taxon = dplyr::if_else(taxon == 'OH', 'other hardwood', taxon)) |>
  # format
  dplyr::mutate(taxon = stringr::str_to_title(taxon)) |>
  # group for summarizing
  dplyr::group_by(x, y, time, taxon) |>
  # summarize over draws
  dplyr::summarize(predicted_mean = mean(Predicted),
                   predicted_sd = sd(Predicted),
                   predicted_quant_2.5 = quantile(Predicted, probs = 0.025),
                   predicted_quant_25 = quantile(Predicted, probs = 0.25),
                   predicted_quant_75 = quantile(Predicted, probs = 0.75),
                   predicted_quant_975 = quantile(Predicted, probs = 0.975))

# Combine
pred_obs_long <- pred_mean_long |>
  dplyr::full_join(y = obs_long,
                   by = c('x', 'y', 'time', 'taxon'))

## BEECH - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Beech') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlim(c(-0.01, 0.7)) + ggplot2::ylim(c(-0.01, 0.7)) +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## BIRCH - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Birch') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlim(c(-0.01, 0.5)) + ggplot2::ylim(c(-0.01, 0.5)) +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## ELM - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Elm') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(-0.01, 0.65)) + ggplot2::ylim(c(-0.01, 0.65)) +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## HEMLOCK - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Hemlock') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(-0.01, 0.7)) + ggplot2::ylim(c(-0.01, 0.7)) +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## MAPLE - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Maple') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(-0.01, 0.4)) + ggplot2::ylim(c(-0.01, 0.4)) +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## OTHER CONIFER - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Other Conifer') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(-0.01, 0.6)) + ggplot2::ylim(c(-0.01, 0.6)) +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## OTHER HARDWOOD - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Other Hardwood') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(-0.01, 0.5)) + ggplot2::ylim(c(-0.01, 0.5)) +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## PINE - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Pine') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(-0.01, 1)) + ggplot2::ylim(c(-0.01, 1)) +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## SPRUCE - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Spruce') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(-0.01, 0.5)) + ggplot2::ylim(c(-0.01, 0.5)) +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## TAMARACK - mean +/- sd

pred_obs_long |>
  dplyr::filter(taxon == 'Tamarack') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(-0.01, 0.8)) + ggplot2::ylim(c(-0.01, 0.8)) +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## ALL - mean +/- sd

pred_obs_long |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_mean - observed_sd,
                                      ymax = observed_mean + observed_sd)) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_mean - predicted_sd,
                                       xmax = predicted_mean + predicted_sd)) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## BEECH - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Beech') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## BIRCH  - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Birch') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.65)) + ggplot2::ylim(c(0, 0.65)) +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## ELM  - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Elm') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## HEMLOCK  - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Hemlock') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## MAPLE - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Maple') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.6)) + ggplot2::ylim(c(0, 0.6)) +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## OTHER CONIFER - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Other Conifer') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## OTHER HARDWOOD - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Other Hardwood') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.6)) + ggplot2::ylim(c(0, 0.6)) +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## PINE - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Pine') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## SPRUCE - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Spruce') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.65)) + ggplot2::ylim(c(0, 0.65)) +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## TAMARACK - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon == 'Tamarack') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, y = observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 0.8)) + ggplot2::ylim(c(0, 0.8)) +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 panel.border = ggplot2::element_rect(color = 'black', fill = NA))

## ALL - mean [95% CI]

pred_obs_long |>
  dplyr::filter(taxon != 'Oak') |>
  ggplot2::ggplot(ggplot2::aes(x = predicted_mean, observed_mean)) +
  ggplot2::geom_point(alpha = 0.8) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = observed_quant_2.5,
                                      ymax = observed_quant_975),
                         alpha = 0.3) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = predicted_quant_2.5,
                                       xmax = predicted_quant_975),
                          alpha = 0.3) +
  ggplot2::geom_abline(color = 'blue') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA))

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

# Open pdf
pdf(file = 'figures/posteriors/oos_prediction/full_oak_conditional_diff.pdf',
    width = 8.5, height = 4.5)


## BEECH

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BEECH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BEECH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BEECH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(BIRCH, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(BIRCH)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(BIRCH, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(ELM, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(ELM)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(ELM, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(HEMLOCK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(HEMLOCK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(HEMLOCK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(MAPLE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(MAPLE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(MAPLE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OTHER.CONIFER, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.CONIFER)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OTHER.CONIFER, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other conifer taxa, 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(OTHER.HARDWOOD, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(OTHER.HARDWOOD)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(OTHER.HARDWOOD, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Other hardwood taxa, 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(PINE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(PINE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'),' ), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(PINE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(SPRUCE, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(SPRUCE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(SPRUCE, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

# 2.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(low = quantile(TAMARACK, probs = 0.025)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = low)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), 2.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# Median

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(median = median(TAMARACK)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = median)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), accuracy median'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

# 97.5% CI

diff |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  dplyr::group_by(x, y, time) |>
  dplyr::summarize(high = quantile(TAMARACK, probs = 0.975)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = high)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Observed -\nPredicted') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), '), 97.', 5^{th}, ' accuracy percentile'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

dev.off()
