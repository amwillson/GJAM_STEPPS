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

# Get density
pred_obs_long$density <- get_density(pred_obs_long$predicted_mean, pred_obs_long$observed_mean, n = 100)

# Plot predicted vs observed with point density colored
# this helps because there are so many points that it's not
# possible to see point density with different levels of opacity
pred_obs_long |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = predicted_mean, y = observed_mean, color = density),
                      show.legend = FALSE) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed', linewidth = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = predicted_mean, y = observed_mean),
                       se = FALSE, method = 'lm', color = 'red',
                       linetype = 'solid', linewidth = 1) +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::scale_color_distiller(transform = 'sqrt') +
  ggplot2::xlab('Predicted') + ggplot2::ylab('Observed') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                 strip.text = ggplot2::element_text(size = 10),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_blank())

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed.png',
                height = 10, width = 14, units = 'cm')

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
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ') median prediction accuracy'))) +
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
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ') median prediction accuracy'))) +
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
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ') median prediction accuracy'))) +
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
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ') median prediction accuracy'))) +
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
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ') median prediction accuracy'))) +
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
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ') median prediction accuracy'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_oak.png')
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
  ggplot2::ggtitle(expression(paste('Other conifer taxa median prediction accuracy'))) +
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
  ggplot2::ggtitle('Other hardwood taxa median prediction accuracy') +
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
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ') median prediction accuracy'))) +
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
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ') median prediction accuracy'))) +
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
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ') median prediction accuracy'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/full_non_conditional_diff_tamarack.png',
                height = 12, width = 18, units = 'cm')

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
