### STEP 4-7

## Plotting out-of-sample predictions in time
## Out-of-sample prediction of 300 YBP for same spatial locations as
## Used to fit the model

## Input: data/processed/post_stepps_soil_clim.RData
## OOS data

## Input: /Volumes/FileBackup/GJAM_STEPPS_output/oos_prediction_nonconditional_time.RData
## OOS predictions for 300 YBP using non-conditional method
## Saved on external drive because of large file size (8 GB)

## Input: /Volumes/FileBackup/GJAM_STEPPS_output/posteriors/oos_prediction_conditionoak_time.RData
## OOS predictions for 300 YBP using conditional on oak method
## Saved on external drive because of large file size (8 GB)

## Output: none

rm(list = ls())

source('R/funs.R')

# Load out-of-sample data
load('data/processed/post_stepps_soil_clim.RData')

#### Non-conditional prediction ####

# Load out-of-sample predictions
load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/oos_prediction_nonconditional_time.RData')

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
    
    # Add to full grid (with missing data)
    pred_mean2 <- oos |>
      dplyr::select(x, y) |>
      dplyr::left_join(y = pred_mean, by = c('x', 'y'))
    
    # Add draw index
    pred_mean2$draw <- rep(i, times = nrow(pred_mean2))
    
    # Otherwise add to previous draw
  }else{
    # Same steps but temporary storage
    temp2 <- temp$sdList$yMu
    temp2 <- as.data.frame(temp2)
    temp2$x <- oos2$x
    temp2$y <- oos2$y

    temp3 <- oos |>
      dplyr::select(x, y) |>
      dplyr::left_join(y = temp2, by = c('x', 'y'))
    
    temp3$draw <- rep(i, times = nrow(temp3))
    
    # Add to previous draw
    pred_mean2 <- rbind(pred_mean2, temp3)
  }
  print(i)
}

### Check how close we get to sum to 1 ###

## We just want to make sure that the model is in fact
## following the sum to 1 constraint of proportional data
## There is likely some wiggle room but it should be close to 1
## in each Gibbs sample

pred_mean_sum <- pred_mean2 |>
  dplyr::mutate(sum = rowSums(dplyr::across(BEECH:TAMARACK)))

pred_mean_sum |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(low = min(sum),
                   mean = mean(sum),
                   high = max(sum)) |>
  tidyr::pivot_longer(cols = low:high,
                      names_to = 'summary', values_to = 'total') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = total)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(summary, levels = c('low', 'mean', 'high'))) +
  ggplot2::theme_void()

### Plot each taxon's predictive mean over space ###

## Predictive mean is what the model on average
## predicts the relative abundance of each taxon
## to be, given the environment

## Note that we are plotting the median and 75% CrI 
## of predictive mean. This is over the 100 posterior
## draws of STEPPS

## BEECH

pdf(file = 'figures/posteriors/oos_prediction/non_conditional_prediction.pdf',
    width = 8.5, height = 4.5)

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BEECH, na.rm = TRUE),
                   low = quantile(BEECH, probs = 0.25, na.rm = TRUE),
                   high = quantile(BEECH, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BIRCH, na.rm = TRUE),
                   low = quantile(BIRCH, probs = 0.25, na.rm = TRUE),
                   high = quantile(BIRCH, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(ELM, na.rm = TRUE),
                   low = quantile(ELM, probs = 0.25, na.rm = TRUE),
                   high = quantile(ELM, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(HEMLOCK, na.rm = TRUE),
                   low = quantile(HEMLOCK, probs = 0.25, na.rm = TRUE),
                   high = quantile(HEMLOCK, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(MAPLE, na.rm = TRUE),
                   low = quantile(MAPLE, probs = 0.25, na.rm = TRUE),
                   high = quantile(MAPLE, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OAK

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OAK, na.rm = TRUE),
                   low = quantile(OAK, probs = 0.25, na.rm = TRUE),
                   high = quantile(OAK, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OC, na.rm = TRUE),
                   low = quantile(OC, probs = 0.25, na.rm = TRUE),
                   high = quantile(OC, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1),
                                transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OH, na.rm = TRUE),
                   low = quantile(OH, probs = 0.25, na.rm = TRUE),
                   high = quantile(OH, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(PINE, na.rm = TRUE),
                   low = quantile(PINE, probs = 0.25, na.rm = TRUE),
                   high = quantile(PINE, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(SPRUCE, na.rm = TRUE),
                   low = quantile(SPRUCE, probs = 0.25, na.rm = TRUE),
                   high = quantile(SPRUCE, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(TAMARACK, na.rm = TRUE),
                   low = quantile(TAMARACK, probs = 0.25, na.rm = TRUE),
                   high = quantile(TAMARACK, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile, 
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

dev.off()

### Difference between observed and predicted: each draw individually ###

# Loop over each draw
for(i in 1:max(pred_mean2$draw)){
  temp <- dplyr::filter(pred_mean2, draw == i)
  oos <- dplyr::filter(post_oos_all, draw == i)
  if(i == 1){
    diff <- dplyr::select(oos, BEECH:TAMARACK) - dplyr::select(temp, BEECH:TAMARACK)
    diff$x <- temp$x
    diff$y <- temp$y
    diff$draw <- rep(i, times = nrow(oos))
  }else{
    temp2 <- dplyr::select(oos, BEECH:TAMARACK) - dplyr::select(temp, BEECH:TAMARACK)
    temp2$x <- temp$x
    temp2$y <- temp$y
    temp2$draw <- rep(i, times = nrow(oos))
    diff <- rbind(diff, temp2)
  }
  print(i)
}

# Plot

## These plots show the median and 95% CrI of the difference between
## observation and prediction. I interpret the CrI as meaning that
## if the direction of the difference (negative or positive difference)
## is the same at a given location in both quantiles, this would
## be indicative of a "significant" divergence from observation
## by the model. The model does not capture the process that is
## giving rise to the relative abundance at those locations

pdf(file = 'figures/posteriors/oos_prediction/non_conditional_difference.pdf',
    width = 8.5, height = 4.5)

## BEECH

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BEECH, na.rm = TRUE),
                   low = quantile(BEECH, probs = 0.025, na.rm = TRUE),
                   high = quantile(BEECH, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BIRCH, na.rm = TRUE),
                   low = quantile(BIRCH, probs = 0.025, na.rm = TRUE),
                   high = quantile(BIRCH, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(ELM, na.rm = TRUE),
                   low = quantile(ELM, probs = 0.025, na.rm = TRUE),
                   high = quantile(ELM, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(HEMLOCK, na.rm = TRUE),
                   low = quantile(HEMLOCK, probs = 0.025, na.rm = TRUE),
                   high = quantile(HEMLOCK, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(MAPLE, na.rm = TRUE),
                   low = quantile(MAPLE, probs = 0.025, na.rm = TRUE),
                   high = quantile(MAPLE, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OAK

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OAK, na.rm = TRUE),
                   low = quantile(OAK, probs = 0.025, na.rm = TRUE),
                   high = quantile(OAK, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OTHER.CONIFER, na.rm = TRUE),
                   low = quantile(OTHER.CONIFER, probs = 0.025, na.rm = TRUE),
                   high = quantile(OTHER.CONIFER, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OTHER.HARDWOOD, na.rm = TRUE),
                   low = quantile(OTHER.HARDWOOD, probs = 0.025, na.rm = TRUE),
                   high = quantile(OTHER.HARDWOOD, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(PINE, na.rm = TRUE),
                   low = quantile(PINE, probs = 0.025, na.rm = TRUE),
                   high = quantile(PINE, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(SPRUCE, na.rm = TRUE),
                   low = quantile(SPRUCE, probs = 0.025, na.rm = TRUE),
                   high = quantile(SPRUCE, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(TAMARACK, na.rm = TRUE),
                   low = quantile(TAMARACK, probs = 0.025, na.rm = TRUE),
                   high = quantile(TAMARACK, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

dev.off()

### Plot observed vs predicted irrespective of space/time ###

## If the model is doing a good job predicting relative abundances,
## then we would expect that the points would generally fall around
## the 1:1 line. If the model produces biased predictions,
## then we would see that the points generally fall either above
## or below the 1:1 line

pred_mean_long <- pred_mean2 |>
  tidyr::drop_na() |>
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
  dplyr::group_by(x, y, taxon) |>
  # summarize over draws
  dplyr::summarize(predicted_mean = mean(Predicted),
                   predicted_sd = sd(Predicted),
                   predicted_quant_2.5 = quantile(Predicted, probs = 0.025),
                   predicted_quant_25 = quantile(Predicted, probs = 0.25),
                   predicted_quant_75 = quantile(Predicted, probs = 0.75),
                   predicted_quant_975 = quantile(Predicted, probs = 0.975))

obs_long <- post_oos_all |>
  tidyr::drop_na() |>
  # select relevant columns
  dplyr::select(draw:y) |>
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
  dplyr::group_by(x, y, taxon) |>
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
                   by = c('x', 'y', 'taxon'))

pred_obs_long |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = observed_mean, y = predicted_mean)) +
  ggplot2::geom_errorbar(ggplot2::aes(x = observed_mean,
                                      ymin = predicted_quant_2.5,
                                      ymax = predicted_quant_975)) +
  ggplot2::geom_abline(color = 'black', linetype = 'dashed') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::xlab('Observed') + ggplot2::ylab('Predicted') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 plot.title = ggplot2::element_text(size = 12, hjust = 0.5))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/posteriors/oos_prediction/predicted_observed_300YBP.png',
                width = 20, height = 10, units = 'cm')

### Correlations between observed and predicted ###

# Drop NAs
pred_mean3 <- tidyr::drop_na(pred_mean2)
obs_3 <- post_oos_all |>
  dplyr::select(draw, BEECH:y) |>
  tidyr::drop_na()

# Change column names
colnames(pred_mean3) <- c('x', 'y', 'beech_pred',
                          'birch_pred', 'elm_pred',
                          'hemlock_pred', 'maple_pred',
                          'oak_pred', 'oc_pred', 'oh_pred',
                          'pine_pred', 'spruce_pred',
                          'tamarack_pred', 'draw')
colnames(obs_3) <- c('draw', 'beech_obs',
                     'birch_obs', 'elm_obs', 'hemlock_obs',
                     'maple_obs', 'oak_obs', 'oc_obs', 'oh_obs',
                     'pine_obs', 'spruce_obs',
                     'tamarack_obs', 'x', 'y')

# Combine
pred_obs <- pred_mean3 |>
  dplyr::left_join(y = obs_3,
                   by = c('x', 'y', 'draw'))

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

#### Conditional on oak ####

rm(pred)

# Load predictions
load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/oos_prediction_conditionoak_time.RData')

# Loop over posterior draws
for(i in 1:length(oak_cond_pred)){
  # Extract one output for one posterior draw
  temp <- oak_cond_pred[[i]]
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
    
    # Add to full grid (with missing data)
    pred_mean2 <- oos |>
      dplyr::select(x, y) |>
      dplyr::left_join(y = pred_mean, by = c('x', 'y'))
    
    # Add draw index
    pred_mean2$draw <- rep(i, times = nrow(pred_mean2))
    
    # Otherwise add to previous draw
  }else{
    # Same steps but temporary storage
    temp2 <- temp$sdList$yMu
    temp2 <- as.data.frame(temp2)
    temp2$x <- oos2$x
    temp2$y <- oos2$y
    
    temp3 <- oos |>
      dplyr::select(x, y) |>
      dplyr::left_join(y = temp2, by = c('x', 'y'))
    
    temp3$draw <- rep(i, times = nrow(temp3))
    
    # Add to previous draw
    pred_mean2 <- rbind(pred_mean2, temp3)
  }
  print(i)
}

### Plot each taxon's predictive mean over space ###

## Interpretation of the median and CrI is the same as for
## non-conditional prediction above. We expect that for the taxa
## near biome boundaries and that interface with the region of
## oak dominance should be better predicted (mainly higher predictive
## means) when we know the relative abundance of oak

pdf(file = 'figures/posteriors/oos_prediction/oak_conditional_prediction.pdf',
    width = 8.5, height = 4.5)

## BEECH

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BEECH, na.rm = TRUE),
                   low = quantile(BEECH, probs = 0.25, na.rm = TRUE),
                   high = quantile(BEECH, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BIRCH, na.rm = TRUE),
                   low = quantile(BIRCH, probs = 0.25, na.rm = TRUE),
                   high = quantile(BIRCH, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(ELM, na.rm = TRUE),
                   low = quantile(ELM, probs = 0.25, na.rm = TRUE),
                   high = quantile(ELM, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(HEMLOCK, na.rm = TRUE),
                   low = quantile(HEMLOCK, probs = 0.25, na.rm = TRUE),
                   high = quantile(HEMLOCK, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(MAPLE, na.rm = TRUE),
                   low = quantile(MAPLE, probs = 0.25, na.rm = TRUE),
                   high = quantile(MAPLE, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OC, na.rm = TRUE),
                   low = quantile(OC, probs = 0.25, na.rm = TRUE),
                   high = quantile(OC, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OH, na.rm = TRUE),
                   low = quantile(OH, probs = 0.25, na.rm = TRUE),
                   high = quantile(OH, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(PINE, na.rm = TRUE),
                   low = quantile(PINE, probs = 0.25, na.rm = TRUE),
                   high = quantile(PINE, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(SPRUCE, na.rm = TRUE),
                   low = quantile(SPRUCE, probs = 0.25, na.rm = TRUE),
                   high = quantile(SPRUCE, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

pred_mean2 |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(TAMARACK, na.rm = TRUE),
                   low = quantile(TAMARACK, probs = 0.25, na.rm = TRUE),
                   high = quantile(TAMARACK, probs = 0.75, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('25%', '50%', '75%'))) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                name = 'Relative\nabundance',
                                na.value = '#00000000',
                                limits = c(0, 1), transform = 'sqrt') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

dev.off()

### Difference between observed and predicted: each draw individually ###

# Loop over each draw
for(i in 1:max(pred_mean2$draw)){
  temp <- dplyr::filter(pred_mean2, draw == i)
  oos <- dplyr::filter(post_oos_all, draw == i)
  if(i == 1){
    diff <- dplyr::select(oos, BEECH:TAMARACK) - dplyr::select(temp, BEECH:TAMARACK)
    diff$x <- temp$x
    diff$y <- temp$y
    diff$draw <- rep(i, times = nrow(oos))
  }else{
    temp2 <- dplyr::select(oos, BEECH:TAMARACK) - dplyr::select(temp, BEECH:TAMARACK)
    temp2$x <- temp$x
    temp2$y <- temp$y
    temp2$draw <- rep(i, times = nrow(oos))
    diff <- rbind(diff, temp2)
  }
  print(i)
}

# Plot

## Similarly, we would expect that, for taxa that interface with
## regions of high oak abundance, difference between observation
## and prediction should be lower than in the case of predictions
## made without knowing the observed relative abundance of oak
## Taxa whose dynamics are not captured by the model because
## the model doesn't represent other important proceses (e.g.,
## hemlock, pine) should not show marked increases in prediction
## accuracy here

pdf(file = 'figures/posteriors/oos_prediction/oak_conditional_difference.pdf',
    width = 8.5, height = 4.5)

## BEECH

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BEECH, na.rm = TRUE),
                   low = quantile(BEECH, probs = 0.025, na.rm = TRUE),
                   high = quantile(BEECH, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## BIRCH

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(BIRCH, na.rm = TRUE),
                   low = quantile(BIRCH, probs = 0.025, na.rm = TRUE),
                   high = quantile(BIRCH, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## ELM

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(ELM, na.rm = TRUE),
                   low = quantile(ELM, probs = 0.025, na.rm = TRUE),
                   high = quantile(ELM, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## HEMLOCK

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(HEMLOCK, na.rm = TRUE),
                   low = quantile(HEMLOCK, probs = 0.025, na.rm = TRUE),
                   high = quantile(HEMLOCK, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## MAPLE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(MAPLE, na.rm = TRUE),
                   low = quantile(MAPLE, probs = 0.025, na.rm = TRUE),
                   high = quantile(MAPLE, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER CONIFER

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OTHER.CONIFER, na.rm = TRUE),
                   low = quantile(OTHER.CONIFER, probs = 0.025, na.rm = TRUE),
                   high = quantile(OTHER.CONIFER, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## OTHER HARDWOOD

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(OTHER.HARDWOOD, na.rm = TRUE),
                   low = quantile(OTHER.HARDWOOD, probs = 0.025, na.rm = TRUE),
                   high = quantile(OTHER.HARDWOOD, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 14))

## PINE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(PINE, na.rm = TRUE),
                   low = quantile(PINE, probs = 0.025, na.rm = TRUE),
                   high = quantile(PINE, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## SPRUCE

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(SPRUCE, na.rm = TRUE),
                   low = quantile(SPRUCE, probs = 0.025, na.rm = TRUE),
                   high = quantile(SPRUCE, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

## TAMARACK

diff |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(median = median(TAMARACK, na.rm = TRUE),
                   low = quantile(TAMARACK, probs = 0.025, na.rm = TRUE),
                   high = quantile(TAMARACK, probs = 0.975, na.rm = TRUE)) |>
  tidyr::pivot_longer(cols = median:high, names_to = 'quantile', values_to = 'value') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = value)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~factor(quantile,
                              levels = c('low', 'median', 'high'),
                              labels = c('2.5%', '50%', '97.5%'))) +
  ggplot2::scale_fill_distiller(palette = 'RdBu',
                                limits = c(-1, 1),
                                direction = 1,
                                name = 'Observed -\nPredicted',
                                na.value = '#00000000') +
  ggplot2::theme_void() +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'),
                 strip.text = ggplot2::element_text(size = 14))

dev.off()
