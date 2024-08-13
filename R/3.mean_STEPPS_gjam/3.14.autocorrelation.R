rm(list = ls())

source('R/funs.R')

# Load out-of-sample predictions
load('out/mean/oos_prediction_all.RData')

# Load out-of-sample data
load('data/processed/mean_stepps_full_oos.RData')

# Map of study region
states <- map_states()

# Predictive mean
pred_mean <- pred$sdList$yMu

# Format
pred_mean <- as.data.frame(pred_mean)
pred_mean$x <- taxon_oos_all$stepps_x
pred_mean$y <- taxon_oos_all$stepps_y
pred_mean$time <- taxon_oos_all$time

# Difference (observed - predicted)
diff <- dplyr::select(taxon_oos_all, beech:tamarack) - dplyr::select(pred_mean, beech:tamarack)

# Assign coords
diff$x <- pred_mean$x
diff$y <- pred_mean$y
# Assign time
diff$time <- pred_mean$time

#### Hemlock mountain ####

# Maximum at x = 445000, y = 982000
# Surrounding x = 397000, 421000, 469000, 493000
# Surrounding y = 934000, 958000, 1006000, 1030000

hemlock_mountain_locs <- expand.grid(x = c(397000, 421000, 445000,
                                           469000, 493000),
                                     y = c(934000, 958000, 982000,
                                           1006000, 1030000))

hemlock_mountain_locs$loc <- paste0(hemlock_mountain_locs$x, '_', hemlock_mountain_locs$y)

hemlock_mountain <- diff |>
  dplyr::mutate(loc = paste0(x, '_', y)) |>
  dplyr::filter(loc %in% c(hemlock_mountain_locs$loc))

### Using observed-predicted at previous time step

lagged <- hemlock_mountain |>
  dplyr::mutate(time = time - 1) |>
  dplyr::rename(lagged_hemlock = hemlock) |>
  dplyr::select(x, y, time, loc, lagged_hemlock)

hemlock_mountain_lagged <- hemlock_mountain |>
  dplyr::select(x, y, time, loc, hemlock) |>
  dplyr::full_join(y = lagged,
                   by = c('x', 'y', 'time', 'loc')) |>
  tidyr::drop_na()

hemlock_mountain_lmer <- lme4::lmer(formula = hemlock ~ lagged_hemlock + (1|loc),
                                 data = hemlock_mountain_lagged)
summary(hemlock_mountain_lmer)

hemlock_mountain_lm <- lm(formula = hemlock ~ lagged_hemlock,
                          data = hemlock_mountain_lagged)
summary(hemlock_mountain_lm)

hemlock_mountain_lagged |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = rev(time), y = hemlock, color = loc), 
                     show.legend = FALSE) +
  ggplot2::xlab('Time (YBP)') + ggplot2::ylab('Observed - Predicted Hemlock Relative Abundance') +
  ggplot2::theme_minimal()

hemlock_mountain_lagged |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = lagged_hemlock, y = hemlock, color = loc),
                     show.legend = FALSE) +
  ggplot2::geom_abline() +
  ggplot2::xlab('Observed - Predicted at Previous Time Step') +
  ggplot2::ylab('Observed - Predicted at Current Time Step') +
  ggplot2::theme_minimal()

### Using observed relative abundance at previous timestep

lagged <- taxon_oos_all |>
  dplyr::mutate(loc = paste0(stepps_x, '_', stepps_y)) |>
  dplyr::filter(loc %in% hemlock_mountain_locs$loc) |>
  dplyr::mutate(time = time - 1) |>
  dplyr::rename(lagged_hemlock = hemlock,
                x = stepps_x,
                y = stepps_y) |>
  dplyr::select(x, y, time, loc, lagged_hemlock)

hemlock_mountain_lagged <- hemlock_mountain |>
  dplyr::select(x, y, time, loc, hemlock) |>
  dplyr::full_join(y = lagged,
                   by = c('x', 'y', 'time', 'loc')) |>
  tidyr::drop_na()

hemlock_mountain_lmer <- lme4::lmer(formula = hemlock ~ lagged_hemlock + (1|loc),
                                    data = hemlock_mountain_lagged)
summary(hemlock_mountain_lmer)

hemlock_mountain_lm <- lm(formula = hemlock ~ lagged_hemlock,
                          data = hemlock_mountain_lagged)
summary(hemlock_mountain_lm)

hemlock_mountain_lagged |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = rev(time), y = hemlock, color = loc), 
                     show.legend = FALSE) +
  ggplot2::xlab('Time (YBP)') + ggplot2::ylab('Observed - Predicted Hemlock Relative Abundance') +
  ggplot2::theme_minimal()

hemlock_mountain_lagged |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = rev(time), y = lagged_hemlock, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time (YBP)') + ggplot2::ylab('Observed Hemlock Relative Abundance') +
  ggplot2::theme_minimal()

hemlock_mountain_lagged |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = lagged_hemlock, y = hemlock, color = loc),
                      show.legend = FALSE) +
  ggplot2::geom_abline() +
  ggplot2::xlab('Observed Hemlock Relative Abundance at Previous Time Step') +
  ggplot2::ylab('Observed - Predicted at Current Time Step') +
  ggplot2::theme_minimal()
