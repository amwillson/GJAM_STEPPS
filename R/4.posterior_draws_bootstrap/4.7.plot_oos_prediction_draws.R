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
