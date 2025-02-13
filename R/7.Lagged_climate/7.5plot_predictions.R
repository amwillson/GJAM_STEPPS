### STEP 7-4

## Comparing out-of-sample predictions from models with
## and without lagged climate variables

## This provides an indicator of whether lagged climate variables
## improve our ability to capture trends in relative abundances

## Output: none

rm(list = ls())

source('R/funs.R')

#### 1. Lagged model predictions ####

# Load out-of-sample predictions
load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/oos_prediction_nonconditional_time_lagged.RData')

## Need to load and format data to make sure the rows are in the
## same order as what was used in the predict() function

# Load data
load('data/processed/post_stepps_soil_clim.RData')

# Remove NAs
post_insample_all <- tidyr::drop_na(post_insample_all)
post_oos_all <- tidyr::drop_na(post_oos_all)

# Combine
data1 <- rbind(post_insample_all, post_oos_all)

# Remove
rm(post_insample_all, post_oos_all)

# Load data
load('data/processed/post_stepps_full_oos.RData')

# Remove NAs
post_oos_all <- tidyr::drop_na(post_oos_all)

# Combine
all_data <- rbind(data1, post_oos_all)

# Create lagged climate variables
all_data <- all_data |>
  dplyr::group_by(draw, x, y) |>
  dplyr::arrange(desc(time)) |>
  dplyr::mutate(aatLag = dplyr::lag(aat),
                tprLag = dplyr::lag(tpr),
                prsdLag = dplyr::lag(prsd))

# Load original data again
load('data/processed/post_stepps_soil_clim.RData')

# Add lagged climate variable to oos data
post_oos_all <- post_oos_all |>
  dplyr::left_join(y = dplyr::select(all_data,
                                     x, y, draw, time,
                                     aatLag, tprLag, prsdLag),
                   by = c('x', 'y', 'draw', 'time'))

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
      dplyr::ungroup() |>
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
      dplyr::ungroup() |>
      dplyr::select(x, y) |>
      dplyr::left_join(y = temp2, by = c('x', 'y'))
    
    temp3$draw <- rep(i, times = nrow(temp3))
    
    # Add to previous draw
    pred_mean2 <- rbind(pred_mean2, temp3)
  }
  print(i)
}

# Rename
pred_mean_lagged <- pred_mean2

#### 2. Non-lagged model predictions ####

# Load out-of-sample predictions
load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/oos_prediction_nonconditional_time.RData')

# Load out-of-sample data
# Need to reload to make sure that it's the same order
# as what was fed into the predict function
load('data/processed/post_stepps_soil_clim.RData')

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

# Rename
pred_mean_nolag <- pred_mean2

#### 3. Format with observations ####

# Take only vegetation columns from observations
abundance_obs <- post_oos_all |>
  dplyr::select(draw, BEECH:TAMARACK, x, y) |>
  tidyr::pivot_longer(cols = BEECH:TAMARACK,
                      names_to = 'Taxon',
                      values_to = 'Observed') |>
  dplyr::mutate(Taxon = stringr::str_to_sentence(Taxon),
                Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other conifer', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other hardwood', Taxon)) |>
  tidyr::drop_na()

# Combine lagged predictions and observations
pred_obs_lagged <- pred_mean_lagged |>
  tidyr::pivot_longer(cols = BEECH:TAMARACK,
                      names_to = 'Taxon',
                      values_to = 'Predicted') |>
  dplyr::mutate(Taxon = stringr::str_to_sentence(Taxon),
                Taxon = dplyr::if_else(Taxon == 'Oc', 'Other conifer', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Oh', 'Other hardwood', Taxon)) |>
  tidyr::drop_na() |>
  dplyr::full_join(y = abundance_obs,
                   by = c('x', 'y', 'draw', 'Taxon'))

# Combine non-lagged predictions and observations
pred_obs_nolag <- pred_mean_nolag |>
  tidyr::pivot_longer(cols = BEECH:TAMARACK,
                      names_to = 'Taxon',
                      values_to = 'Predicted') |>
  dplyr::mutate(Taxon = stringr::str_to_sentence(Taxon),
                Taxon = dplyr::if_else(Taxon == 'Oc', 'Other conifer', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Oh', 'Other hardwood', Taxon)) |>
  tidyr::drop_na() |>
  dplyr::full_join(y = abundance_obs,
                   by = c('x', 'y', 'draw', 'Taxon'))

#### 4. Correlations between predicted and observed

# Correlation between observed and predicted from lagged climate model
# for each taxon
cor_lagged <- pred_obs_lagged |>
  dplyr::group_by(Taxon) |>
  dplyr::summarize(lagged_cor = cor(Predicted, Observed))

# Correlation between median observed and median predicted from 
# lagged climate model for each taxon across draws
cor_med_lagged <- pred_obs_lagged |>
  dplyr::group_by(Taxon, x, y) |>
  dplyr::summarize(Predicted = median(Predicted),
                   Observed = median(Observed)) |>
  dplyr::ungroup() |>
  dplyr::group_by(Taxon) |>
  dplyr::summarize(lagged_cor = cor(Predicted, Observed))

# Correlation between observed and predicted from non-lagged
# climate model for each taxon
cor_nolag <- pred_obs_nolag |>
  dplyr::group_by(Taxon) |>
  dplyr::summarize(nolag_cor = cor(Predicted, Observed))

# Correlation between median observed and median predicted from
# non-lagged climate model for each taxon across draws
cor_med_nolag <- pred_obs_nolag |>
  dplyr::group_by(Taxon, x, y) |>
  dplyr::summarize(Predicted = median(Predicted),
                   Observed = median(Observed)) |>
  dplyr::ungroup() |>
  dplyr::group_by(Taxon) |>
  dplyr::summarize(nolag_cor = cor(Predicted, Observed))

# Combined correlations
corrs <- cor_lagged |>
  dplyr::full_join(y = cor_nolag,
                   by = 'Taxon') |>
  dplyr::rename(`With climate lags` = lagged_cor,
                `Without climate lags` = nolag_cor)
corrs

# Combined median correlations
corrs_med <- cor_med_lagged |>
  dplyr::full_join(y = cor_med_nolag,
                   by = 'Taxon') |>
  dplyr::rename(`With climate lags` = lagged_cor,
                `Without climate lags` = nolag_cor)
corrs_med

#### 4. Observed vs predicted figures ####

# Plot all predicted vs observed for all draws independently
pred_obs_lagged |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'With climate lags'),
                      shape = 1, size = 0.5) +
  ggplot2::geom_point(data = pred_obs_nolag,
                      ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'Without climate lags'),
                      shape = 1, size = 0.5) +
  ggplot2::geom_smooth(ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'With climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::geom_smooth(data = pred_obs_nolag,
                       ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'Without climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::scale_color_manual(values = c('#004488', '#BB5566'),
                              name = 'Model') +
  ggplot2::scale_linetype_manual(values = c('solid', 'dashed'),
                                 name = 'Mean trend') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::facet_wrap(~Taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 12),
                 axis.title = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/300_predvobs.png',
                height = 8, width = 8, units = 'in')

# Plot all predicted vs observed for median observed and predicted
# across draws
pred_obs_lagged |>
  dplyr::group_by(Taxon, x, y) |>
  dplyr::summarize(Predicted = median(Predicted),
                   Observed = median(Observed)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'With climate lags'),
                      shape = 1, size = 1) +
  ggplot2::geom_point(data = pred_obs_nolag |>
                        dplyr::group_by(Taxon, x, y) |>
                        dplyr::summarize(Predicted = median(Predicted),
                                         Observed = median(Observed)),
                      ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'Without climate lags'),
                      shape = 1, size = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'With climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::geom_smooth(data = pred_obs_nolag |>
                         dplyr::group_by(Taxon, x, y) |>
                         dplyr::summarize(Predicted = median(Predicted),
                                          Observed = median(Observed)),
                       ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'Without climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::scale_color_manual(values = c('#004488', '#BB5566'),
                              name = 'Model') +
  ggplot2::scale_linetype_manual(values = c('solid', 'dashed'),
                                 name = 'Mean trend') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::facet_wrap(~Taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 12),
                 axis.title = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/300_predvobs_median.png',
                height = 8, width = 8, units = 'in')

# Plot only oak with median predicted and observed
pred_obs_lagged |>
  dplyr::group_by(Taxon, x, y) |>
  dplyr::summarize(Predicted = median(Predicted),
                   Observed = median(Observed)) |>
  dplyr::filter(Taxon == 'Oak') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'With climate lags'),
                      shape = 1, size = 1) +
  ggplot2::geom_point(data = pred_obs_nolag |>
                        dplyr::group_by(Taxon, x, y) |>
                        dplyr::summarize(Predicted = median(Predicted),
                                         Observed = median(Observed)) |>
                        dplyr::filter(Taxon == 'Oak'),
                      ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'Without climate lags'),
                      shape = 1, size = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'With climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::geom_smooth(data = pred_obs_nolag |>
                         dplyr::group_by(Taxon, x, y) |>
                         dplyr::summarize(Predicted = median(Predicted),
                                          Observed = median(Observed)) |>
                         dplyr::filter(Taxon == 'Oak'),
                       ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'Without climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::scale_color_manual(values = c('#004488', '#BB5566'),
                              name = 'Model') +
  ggplot2::scale_linetype_manual(values = c('solid', 'dashed'),
                                 name = 'Mean trend') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12),
                 axis.title = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/300_predvobs_median_oak.png',
                height = 4, width = 5, units = 'in')

# Plot only hemlock with median predicted and observed
pred_obs_lagged |>
  dplyr::group_by(Taxon, x, y) |>
  dplyr::summarize(Predicted = median(Predicted),
                   Observed = median(Observed)) |>
  dplyr::filter(Taxon == 'Hemlock') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'With climate lags'),
                      shape = 1, size = 1) +
  ggplot2::geom_point(data = pred_obs_nolag |>
                        dplyr::group_by(Taxon, x, y) |>
                        dplyr::summarize(Predicted = median(Predicted),
                                         Observed = median(Observed)) |>
                        dplyr::filter(Taxon == 'Hemlock'),
                      ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'Without climate lags'),
                      shape = 1, size = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'With climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::geom_smooth(data = pred_obs_nolag |>
                         dplyr::group_by(Taxon, x, y) |>
                         dplyr::summarize(Predicted = median(Predicted),
                                          Observed = median(Observed)) |>
                         dplyr::filter(Taxon == 'Hemlock'),
                       ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'Without climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::scale_color_manual(values = c('#004488', '#BB5566'),
                              name = 'Model') +
  ggplot2::scale_linetype_manual(values = c('solid', 'dashed'),
                                 name = 'Mean trend') +
  ggplot2::xlim(c(0, 1)) + ggplot2::ylim(c(0, 1)) +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 12),
                 axis.title = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/300_predvobs_median_hemlock.png',
                height = 4, width = 5, units = 'in')

#### 5. Change in bias figures ####

# Calculate median difference between observed and predicted from
# each model
bias_lagged <- pred_obs_lagged |>
  dplyr::mutate(Difference = abs(Observed - Predicted)) |>
  dplyr::group_by(Taxon, x, y) |>
  dplyr::summarize(Difference_lagged = median(Difference))
bias_nolag <- pred_obs_nolag |>
  dplyr::mutate(Difference = abs(Observed - Predicted)) |>
  dplyr::group_by(Taxon, x, y) |>
  dplyr::summarize(Difference_nolag = median(Difference))

# Combine
bias_both <- bias_lagged |>
  dplyr::full_join(y = bias_nolag,
                   by = c('Taxon', 'x', 'y'))

# Load data to get full grid
load('data/processed/post_stepps_soil_clim.RData')

# Select coordinate columns
grid <- post_oos_all |>
  tidyr::pivot_longer(cols = BEECH:TAMARACK,
                      names_to = 'Taxon',
                      values_to = 'val') |>
  dplyr::mutate(Taxon = stringr::str_to_sentence(Taxon),
                Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other conifer', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other hardwood', Taxon)) |>
  dplyr::select(x, y, Taxon) |>
  dplyr::distinct()

# Join with bias
bias_both <- grid |>
  dplyr::left_join(y = bias_both,
                   by = c('Taxon', 'x', 'y'))

# Calculate change in bias
bias_both <- bias_both |>
  dplyr::mutate(Change = Difference_nolag - Difference_lagged)

# Map of states
states <- map_states()

# Plot each taxon's change in bias between models
bias_both |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~Taxon) +
  ggplot2::theme_void() +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 12),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/300_pred-obs_median.png',
                height = 8, width = 8, units = 'in')
