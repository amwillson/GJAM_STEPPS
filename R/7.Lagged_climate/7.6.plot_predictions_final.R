#### STEP 7-6

## Comparing out-of-sample predictions across entire spatiotemporal
## domain between models with and without climate lags

rm(list = ls())

source('R/funs.R')

#### 1. Lagged model predictions ####

# Load out-of-sample predictions
load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/oos_prediction_nonconditional_final_lagged.RData')

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

# Load oos data again
load('data/processed/post_stepps_full_oos.RData')

# Add lagged climate variables to oos data
post_oos_all <- post_oos_all |>
  dplyr::left_join(y = dplyr::select(all_data,
                                     x, y, draw, time,
                                     aatLag, tprLag, prsdLag),
                   by = c('x', 'y', 'draw', 'time'))

# Rename
pred <- pred_all

# Remove original
rm(pred_all)

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
      dplyr::ungroup() |>
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
      dplyr::ungroup() |>
      dplyr::select(x, y, time) |>
      dplyr::left_join(y = temp2, by = c('x', 'y', 'time'))
    
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
load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/oos_prediction_nonconditional_final.RData')

# Load out-of-sample data
# Need to reload to make sure that it's the same order
# as what was fed into the predict function
load('data/processed/post_stepps_full_oos.RData')

# Rename
pred <- pred_all

# Remove original
rm(pred_all)

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
      dplyr::ungroup() |>
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
      dplyr::ungroup() |>
      dplyr::select(x, y, time) |>
      dplyr::left_join(y = temp2, by = c('x', 'y', 'time'))
    
    temp3$draw <- rep(i, times = nrow(temp3))
    
    # Add to previous draw
    pred_mean2 <- rbind(pred_mean2, temp3)
  }
  print(i)
}

# Rename
pred_mean_nolag <- pred_mean2

#### 3. Format with observations ####

# Take only vegetation columns from vegetation
abundance_obs <- post_oos_all |>
  dplyr::select(draw, BEECH:TAMARACK, x, y, time) |>
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
                   by = c('x', 'y', 'time', 'draw', 'Taxon'))

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
                   by = c('x', 'y', 'time', 'draw', 'Taxon'))

#### 4. Correlations between predicted and observed

# Correlation between observed and predicted from lagged climate model
# for each taxon
cor_lagged <- pred_obs_lagged |>
  dplyr::filter(time != 19) |>
  dplyr::group_by(Taxon) |>
  dplyr::summarize(lagged_cor = cor(Predicted, Observed))

# Correlation between median observed and median preidcted from
# lagged climate model for each taxon across draws
cor_med_lagged <- pred_obs_lagged |>
  dplyr::filter(time != 19) |>
  dplyr::group_by(Taxon, x, y, time) |>
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

# Correlation between median observed and predicted from
# non-lagged climate model for each taxon across draws
cor_med_nolag <- pred_obs_nolag |>
  dplyr::group_by(Taxon, x, y) |>
  dplyr::summarize(Predicted = median(Predicted),
                   Observed = median(Observed)) |>
  dplyr::ungroup() |>
  dplyr::group_by(Taxon) |>
  dplyr::summarize(nolag_cor = cor(Predicted, Observed))

# Combine correlations
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
  dplyr::filter(time != 19) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'With climate lags'),
                      shape = 1, size = 0.5) +
  ggplot2::geom_point(data = dplyr::filter(pred_obs_nolag, time != 19),
                      ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'Without climate lags'),
                      shape = 1, size = 0.5) +
  ggplot2::geom_smooth(ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'With climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::geom_smooth(data = dplyr::filter(pred_obs_nolag, time != 19),
                       ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'Without climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::scale_color_manual(values = c('#004488', '#885566'),
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
                filename = 'figures/lagged/full_predvobs.png',
                height = 8, width = 8, units = 'in')

# Plot all predicted vs observed for median observed and predicted
# across draws
pred_obs_lagged |>
  dplyr::filter(time != 19) |>
  dplyr::group_by(Taxon, x, y, time) |>
  dplyr::summarize(Predicted = median(Predicted),
                   Observed = median(Observed)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'With climate lags'),
                      shape = 1, size = 1) +
  ggplot2::geom_point(data = pred_obs_nolag |>
                        dplyr::filter(time != 19) |>
                        dplyr::group_by(Taxon, x, y, time) |>
                        dplyr::summarize(Predicted = median(Predicted),
                                         Observed = median(Observed)),
                      ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'Without climate lags'),
                      shape = 1, size = 1) +
  ggplot2::geom_smooth(ggplot2::aes(x = Observed, y = Predicted,
                                    linetype = 'With climate lags'),
                       method = 'lm', color = 'black', se = FALSE) +
  ggplot2::geom_smooth(data = pred_obs_nolag |>
                         dplyr::filter(time != 19) |>
                         dplyr::group_by(Taxon, x, y, time) |>
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
                filename = 'figures/lagged/full_predvobs_median.png',
                height = 8, width = 8, units = 'in')

# Plot only oak with median predicted and observed
pred_obs_lagged |>
  dplyr::filter(time != 19) |>
  dplyr::group_by(Taxon, x, y) |>
  dplyr::summarize(Predicted = median(Predicted),
                   Observed = median(Observed)) |>
  dplyr::filter(Taxon == 'Oak') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'With climate lags'),
                      shape = 1, size = 1) +
  ggplot2::geom_point(data = pred_obs_nolag |>
                        dplyr::filter(time != 19) |>
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
                         dplyr::filter(time != 19) |>
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

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_predvobs_median_oak.png',
                height = 4, width = 5, units = 'in')

# Plot only hemlock with median predicted and observed
pred_obs_lagged |>
  dplyr::filter(time != 19) |>
  dplyr::group_by(Taxon, x, y) |>
  dplyr::summarize(Predicted = median(Predicted),
                   Observed = median(Observed)) |>
  dplyr::filter(Taxon == 'Hemlock') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Observed, y = Predicted,
                                   color = 'With climate lags'),
                      shape = 1, size = 1) +
  ggplot2::geom_point(data = pred_obs_nolag |>
                        dplyr::filter(time != 19) |>
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
                         dplyr::filter(time != 19) |>
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
                filename = 'figures/lagged/full_predvobs_median_hemlock.png',
                height = 4, width = 5, units = 'in')

#### 5. Change in bias figures ####

# Calculate median difference between observed and predicted from
# each model
bias_lagged <- pred_obs_lagged |>
  dplyr::mutate(Difference = abs(Observed - Predicted)) |>
  dplyr::group_by(Taxon, x, y, time) |>
  dplyr::summarize(Difference_lagged = median(Difference))
bias_nolag <- pred_obs_nolag |>
  dplyr::mutate(Difference = abs(Observed - Predicted)) |>
  dplyr::group_by(Taxon, x, y, time) |>
  dplyr::summarize(Difference_nolag = median(Difference))

# Combine
bias_both <- bias_lagged |>
  dplyr::full_join(y = bias_nolag,
                   by = c('Taxon', 'x', 'y', 'time'))

# Load data to get full grid
load('data/processed/post_stepps_full_oos.RData')

# Select coordinate columns
grid <- post_oos_all |>
  tidyr::pivot_longer(cols = BEECH:TAMARACK,
                      names_to = 'Taxon',
                      values_to = 'val') |>
  dplyr::mutate(Taxon = stringr::str_to_sentence(Taxon),
                Taxon = dplyr::if_else(Taxon == 'Other.conifer', 'Other conifer', Taxon),
                Taxon = dplyr::if_else(Taxon == 'Other.hardwood', 'Other hardwood', Taxon)) |>
  dplyr::select(x, y, time, Taxon) |>
  dplyr::distinct()

# Join with bias
bias_both <- grid |>
  dplyr::left_join(y = bias_both,
                   by = c('Taxon', 'x', 'y', 'time'))

# Calculate change in bias
bias_both <- bias_both |>
  dplyr::mutate(Change = Difference_nolag - Difference_lagged)

# Map of states
states <- map_states()

## Plot each taxon's change in bias between models

time_order <- c('1800 YBP', '1700 YBP', '1600 YBP',
                '1500 YBP', '1400 YBP', '1300 YBP',
                '1200 YBP', '1100 YBP', '1000 YBP',
                '900 YBP', '800 YBP', '700 YBP',
                '600 YBP', '500 YBP', '400 YBP',
                '300 YBP')

# Beech
bias_both |>
  dplyr::filter(Taxon == 'Beech',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_beech.png',
                height = 6, width = 8, units = 'in')

# Birch
bias_both |>
  dplyr::filter(Taxon == 'Birch',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_birch.png',
                height = 6, width = 8, units = 'in')

# Elm
bias_both |>
  dplyr::filter(Taxon == 'Elm',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_elm.png',
                height = 6, width = 8, units = 'in')

# Hemlock
bias_both |>
  dplyr::filter(Taxon == 'Hemlock',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_hemlock.png',
                height = 6, width = 8, units = 'in')

# Maple
bias_both |>
  dplyr::filter(Taxon == 'Maple',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_maple.png',
                height = 6, width = 8, units = 'in')

# Oak
bias_both |>
  dplyr::filter(Taxon == 'Oak',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_oak.png',
                height = 6, width = 8, units = 'in')

# Other conifer
bias_both |>
  dplyr::filter(Taxon == 'Other conifer',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle('Other conifer taxa') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_oc.png',
                height = 6, width = 8, units = 'in')

# Other hardwood
bias_both |>
  dplyr::filter(Taxon == 'Other hardwood',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle('Other hardwood taxa') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_oh.png',
                height = 6, width = 8, units = 'in')

# Pine
bias_both |>
  dplyr::filter(Taxon == 'Pine',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_pine.png',
                height = 6, width = 8, units = 'in')

# Spruce
bias_both |>
  dplyr::filter(Taxon == 'Spruce',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_spruce.png',
                height = 6, width = 8, units = 'in')

# Tamarack
bias_both |>
  dplyr::filter(Taxon == 'Tamarack',
                time != 19) |>
  dplyr::mutate(time = as.character(time),
                time = paste0(time, '00 YBP')) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = Change)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(na.value = '#00000000') +
  ggplot2::facet_wrap(~factor(time,
                              levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/lagged/full_pred-obs_tamarack.png',
                height = 6, width = 8, units = 'in')
