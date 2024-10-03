rm(list = ls())

source('R/funs.R')

# Load out-of-sample predictions
load('out/mean/oos_prediction_all.RData')

# Load out-of-sample data
load('data/processed/mean_stepps_full_oos.RData')

#### Non-conditional prediction ####

# Map of study region
states <- map_states()

# Predictive mean
pred_mean <- pred$sdList$yMu

# Format
pred_mean <- as.data.frame(pred_mean)

# Add coordinates and time steps
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

#### Explain GJAM model bias ####

# Extract hemlock
hemlock_diff <- dplyr::select(diff, hemlock, x, y, time)

# Shift time for previous time step in observation
hemlock_obs <- taxon_oos_all |>
  dplyr::select(hemlock, stepps_x,
                stepps_y, time) |>
  dplyr::rename(x = stepps_x,
                y = stepps_y,
                prev_obs = hemlock) |>
  dplyr::mutate(time = time - 1)

# Combine observation and bias
hemlock_diff_obs <- hemlock_diff |>
  dplyr::rename(bias = hemlock) |>
  dplyr::left_join(y = hemlock_obs,
                   by = c('x', 'y', 'time')) |>
  tidyr::drop_na()

# Format environmental data
env <- taxon_oos_all |>
  dplyr::select(stepps_x, stepps_y, time,
                clay, sand, silt,
                caco3, awc, aat, tpr,
                tsd, prsd, prcv) |>
  dplyr::rename(x = stepps_x,
                y = stepps_y)

# Combine with environmental data (at the correct time step)
hemlock <- hemlock_diff_obs |>
  dplyr::left_join(y = env,
                   by = c('x', 'y', 'time'))

# Plots of all variables over time
hemlock |>
  tidyr::pivot_longer(cols = c(bias, prev_obs:prcv),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(loc = paste0(x, '_', y)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = val, color = loc),
                     show.legend = FALSE) +
  ggplot2::facet_wrap(~var, scales = 'free') +
  ggplot2::scale_x_reverse() +
  ggplot2::theme_minimal()

# Correlations between bias and abundance & environmental variables
cors <- cor(x = hemlock$bias,
            y = dplyr::select(hemlock, -bias, -x, -y, -time))

cors <- as.data.frame(t(cors))

cors |>
 tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(correlation = V1) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(variable, correlation), y = correlation),
                    stat = 'identity') +
  ggplot2::xlab('Correlation') + ggplot2::ylab('') +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

cors |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(correlation = V1) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(variable, abs(correlation)), y = abs(correlation)),
                    stat = 'identity') +
  ggplot2::xlab('Correlation') + ggplot2::ylab('') +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

# Correlations between environmental variables
hemlock_allcors <- cor(dplyr::select(hemlock, -x, -y, -time,
                                     -bias, -prev_obs),
                       use = 'everything')

corrplot::corrplot(hemlock_allcors, type = 'upper',
                   diag = FALSE)

# Standardize variables
hemlock <- hemlock |>
  dplyr::mutate(prev_obs_stand = (prev_obs - mean(prev_obs)) / sd(prev_obs),
                clay_stand = (clay - mean(clay)) / sd(clay),
                sand_stand = (sand - mean(sand)) / sd(sand),
                silt_stand = (silt - mean(silt)) / sd(silt),
                caco3_stand = (caco3 - mean(caco3)) / sd(caco3),
                awc_stand = (awc - mean(awc)) / sd(awc),
                aat_stand = (aat - mean(aat)) / sd(aat),
                tpr_stand = (tpr - mean(tpr)) / sd(tpr),
                tsd_stand = (tsd - mean(tsd)) / sd(tsd),
                prsd_stand = (prsd - mean(prsd)) / sd(prsd),
                prcv_stand = (prcv - mean(prcv)) / sd(prcv))

### Linear models
# How much variance is explained by all environmental covariates
# Vs just previous abundance

# All enivronmental covariates
mod1 <- lm(bias ~ clay_stand + sand_stand + silt_stand +
             caco3_stand + awc_stand + aat_stand +
             tpr_stand + tsd_stand + prsd_stand + prcv_stand,
           data = hemlock)
summary(mod1)
summary(mod1)$r.squared

# Only environmental covariates included in GJAM
mod2 <- lm(bias ~ sand_stand + aat_stand + tpr_stand + prsd_stand,
           data = hemlock)
summary(mod2)
summary(mod2)$r.squared

# Only previous abundance
mod3 <- lm(bias ~ prev_obs_stand,
           data = hemlock)
summary(mod3)
summary(mod3)$r.squared

#### Explain current observed abundance ####

# Extract hemlock
hemlock_obs <- dplyr::select(taxon_oos_all,
                             stepps_x, stepps_y,
                             time, hemlock,
                             clay:prcv)

# Shift time for previous time step in observation
hemlock_prev <- taxon_oos_all |>
  dplyr::select(hemlock, stepps_x, stepps_y, time) |>
  dplyr::rename(prev_obs = hemlock) |>
  dplyr::mutate(time = time - 1)

# Combine observation and previous observation
hemlock <- hemlock_obs |>
  dplyr::rename(obs = hemlock) |>
  dplyr::left_join(y = hemlock_prev,
                   by = c('stepps_x', 'stepps_y', 'time')) |>
  tidyr::drop_na()

# Plots of all variables over time
hemlock |>
  tidyr::pivot_longer(obs:prev_obs,
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(loc = paste0(stepps_x, '_', stepps_y)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = val, color = loc),
                     show.legend = FALSE) +
  ggplot2::facet_wrap(~var, scales = 'free') +
  ggplot2::scale_x_reverse() +
  ggplot2::theme_minimal()

# Correlations between current abundance & previous abunadnce & environmental variables
cors <- cor(x = hemlock$obs,
            y = dplyr::select(hemlock, -obs, -stepps_x, -stepps_y, -time))

cors <- as.data.frame(t(cors))

cors |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(correlation = V1) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(variable, correlation), y = correlation),
                    stat = 'identity') +
  ggplot2::xlab('Correlation') + ggplot2::ylab('') +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

cors |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(correlation = V1) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = reorder(variable, abs(correlation)), y = abs(correlation)),
                    stat = 'identity') +
  ggplot2::xlab('Correlation') + ggplot2::ylab('') +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()

# Standardize variables
hemlock <- hemlock |>
  dplyr::mutate(prev_obs_stand = (prev_obs - mean(prev_obs)) / sd(prev_obs),
                clay_stand = (clay - mean(clay)) / sd(clay),
                sand_stand = (sand - mean(sand)) / sd(sand),
                silt_stand = (silt - mean(silt)) / sd(silt),
                caco3_stand = (caco3 - mean(caco3)) / sd(caco3),
                awc_stand = (awc - mean(awc)) / sd(awc),
                aat_stand = (aat - mean(aat)) / sd(aat),
                tpr_stand = (tpr - mean(tpr)) / sd(tpr),
                tsd_stand = (tsd - mean(tsd)) / sd(tsd),
                prsd_stand = (prsd - mean(prsd)) / sd(prsd),
                prcv_stand = (prcv - mean(prcv)) / sd(prcv))

### Linear models
# How much variance is explained by all environmental covariates 
# vs just previous abundance

# All environmental covariates
mod1 <- lm(obs ~ clay_stand + sand_stand + silt_stand +
             caco3_stand + awc_stand + aat_stand +
             tpr_stand + tsd_stand + prsd_stand + prcv_stand,
           data = hemlock)
summary(mod1)
summary(mod1)$r.squared

# Only environmental covariates included in GJAM
mod2 <- lm(obs ~ sand_stand + aat_stand + tpr_stand + prsd_stand,
           data = hemlock)
summary(mod2)
summary(mod2)$r.squared

# Only previous abundance
mod3 <- lm(obs ~ prev_obs_stand,
           data = hemlock)
summary(mod3)
summary(mod3)$r.squared

# Relative contribution of previous abundance vs GJAM covariates
mod4 <- lm(obs ~ prev_obs_stand + sand_stand + aat_stand + tpr_stand + prsd_stand,
           data = hemlock)
summary(mod4)
coefficients(mod4) # order of magnitude higher

#### Coefficient estimates for current observed abundance ####

## Itertively fit models to each grid cell describing the relationship between
## Current and previous time step

# Add "location" column
hemlock <- dplyr::mutate(hemlock, loc = paste0(stepps_x, '_', stepps_y))

# Unique locations
locs <- unique(hemlock$loc)

# Storage
coeffs <- matrix(, nrow = length(locs), ncol = 6)

# Loop through each grid cell
for(i in 1:length(locs)){
  # Subset data for one grid cell
  sub <- dplyr::filter(hemlock, loc == locs[i])
  
  # Model describing relationship between previous and current hemlock abundance
  mod <- lm(formula = obs ~ prev_obs_stand + aat_stand + tpr_stand + prsd_stand,
            data = sub)
  
  # Save grid cell and coefficient
  coeffs[i,1] <- locs[i]
  coeffs[i,2] <- coefficients(mod)[1]
  coeffs[i,3] <- coefficients(mod)[2]
  coeffs[i,4] <- coefficients(mod)[3]
  coeffs[i,5] <- coefficients(mod)[4]
  coeffs[i,6] <- coefficients(mod)[5]
}

# Format
coeffs <- as.data.frame(coeffs)

# Add coordinates
coeffs <- coeffs |>
  dplyr::rename(loc = V1,
                Intercept = V2,
                prev_obs = V3,
                aat = V4,
                tpr = V5,
                prsd = V6) |>
  dplyr::mutate(x = sub(pattern = '_.*', replacement = '', x = loc),
                y = sub(pattern = '.*_', replacement = '', x = loc),
                x = as.numeric(x),
                y = as.numeric(y),
                Intercept = as.numeric(Intercept),
                prev_obs = as.numeric(prev_obs),
                aat = as.numeric(aat),
                tpr = as.numeric(tpr),
                prsd = as.numeric(prsd))

# Plot in space
coeffs |>
  tidyr::pivot_longer(cols = prev_obs:prsd,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var, y = val))

coeffs |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prev_obs)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
