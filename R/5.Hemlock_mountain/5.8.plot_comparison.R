#### STEP 5-8

## Plotting the results of both the Hemlock Mountain
## and oak ecotone autoregressive models together

## This is to add a figure to the main text comparing the
## coefficient estimates for oak and hemlock in these regions

## Input: out/hemlock/out_small_hemlock_mountain.RData
## Model output of model fit to small hemlock mountain area

## Input: out/hemlock/out_small_oak_ecotone.RData
## Model output of model fit to small oak ecotone area
## (where oak prediction bias is worst)

## Output: none except figures saved in figures/ directory

rm(list = ls())

# Helper functions
source('R/funs.R')

#### 1. Load fitted models ####

# Load hemlock output
load('out/hemlock/out_small_hemlock_mountain.RData')

# Rename
out_hm <- out

# Load oak output
load('out/hemlock/out_small_oak_ecotone.RData')

# Rename
out_oak <- out

#### 2. Formatting ####

# Convert output to matrix
out_hm <- as.matrix(out_hm)
out_oak <- as.matrix(out_oak)

# Assign more relevant column names
colnames(out_hm) <- colnames(out_oak) <- 
  c('previous_abundance', 'aat', 'tpr', 'prsd', 'sand', 'intercrept', 'obs_prec', 'proc_prec')

# Make into dataframe
out_hm <- as.data.frame(out_hm)
out_oak <- as.data.frame(out_oak)

# Pivot longer
out_hm <- out_hm |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'parameter',
                      values_to = 'hemlock')

out_oak <- out_oak |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'parameter',
                      values_to = 'oak')

# Summarize
out_hm_summ <- out_hm |>
  dplyr::group_by(parameter) |>
  dplyr::summarize(median = median(hemlock),
                   low = quantile(hemlock, probs = 0.025),
                   high = quantile(hemlock, probs = 0.975),
                   sd = sd(hemlock))

out_oak_summ <- out_oak |>
  dplyr::group_by(parameter) |>
  dplyr::summarize(median = median(oak),
                   low = quantile(oak, probs = 0.025),
                   high = quantile(oak, probs = 0.975),
                   sd = sd(oak))


##### 3. Plot coefficients ####

# Parameters of interest
params <- c('aat', 'previous_abundance', 'prsd', 'sand',
            'tpr')

# Order of x-axis
x_order <- c('Previous\nabundance', 'Average annual\ntemperature',
             'Total annual\nprecipitation', 'Precipitation\nseasonality',
             'Soil % sand')

# Plot coefficient estimates in each model

# Hemlock
out_hm_summ |>
  dplyr::filter(parameter %in% params) |>
  dplyr::mutate(parameter = dplyr::if_else(parameter == 'aat', 'Average annual\ntemperature', parameter),
                parameter = dplyr::if_else(parameter == 'previous_abundance', 'Previous\nabundance', parameter),
                parameter = dplyr::if_else(parameter == 'prsd', 'Precipitation\nseasonality', parameter),
                parameter = dplyr::if_else(parameter == 'sand', 'Soil % sand', parameter),
                parameter = dplyr::if_else(parameter == 'tpr', 'Total annual\nprecipitation', parameter)) |>
  dplyr::mutate(abund = dplyr::if_else(parameter == 'Previous\nabundance', TRUE, FALSE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = factor(parameter,
                                              levels = x_order), 
                                   y = median,
                                   color = abund)) +
  ggplot2::geom_errorbar(ggplot2::aes(x = factor(parameter,
                                                 levels = x_order), 
                                      ymin = low, ymax = high,
                                      color = abund),
                         width = 0.2) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed') +
  ggplot2::scale_color_manual(name = '',
                              values = c('black', '#2a72b2')) +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient estimate') +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.title.x = ggplot2::element_text(size = 10),
                 axis.text.x = ggplot2::element_text(angle = 45),
                 legend.position = 'none')

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/hemlock_oak_small.svg',
                height = 8.5, width = 8.5, units = 'cm')

out_oak_summ |>
  dplyr::filter(parameter %in% params) |>
  dplyr::mutate(parameter = dplyr::if_else(parameter == 'aat', 'Average annual\ntemperature', parameter),
                parameter = dplyr::if_else(parameter == 'previous_abundance', 'Previous\nabundance', parameter),
                parameter = dplyr::if_else(parameter == 'prsd', 'Precipitation\nseasonality', parameter),
                parameter = dplyr::if_else(parameter == 'sand', 'Soil % sand', parameter),
                parameter = dplyr::if_else(parameter == 'tpr', 'Total annual\nprecipitation', parameter)) |>
  dplyr::mutate(abund = dplyr::if_else(parameter == 'Previous\nabundance', TRUE, FALSE)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = factor(parameter,
                                              levels = x_order),
                                   y = median,
                                   color = abund)) +
  ggplot2::geom_errorbar(ggplot2::aes(x = factor(parameter,
                                                 levels = x_order),
                                      ymin = low, ymax = high,
                                      color = abund),
                         width = 0.2) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed') +
  ggplot2::scale_color_manual(name = '',
                              values = c('black', '#2a72b2')) +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient estimate') +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.title.x = ggplot2::element_text(size = 10),
                 axis.text.x = ggplot2::element_text(angle = 90),
                 legend.position = 'none')

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/hemlock_oak_small2.svg',
                height = 8.5, width = 8.5, units = 'cm')

#### 4. Plot locations ####

# Make outline of states
states <- map_states()

# 25 grid cells near Hemlock Mountain
hemlock_mountain_locs <- expand.grid(x = c(397000, 421000, 445000,
                                           469000, 493000),
                                     y = c(934000, 958000, 982000,
                                           1006000, 1030000))

# Location of Hemlock Mountain
hemlock_mountain_locs |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y),
                     fill = '#2a72b2') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/hemlock_mountain_loc.svg',
                height = 5, width = 5, units = 'cm')

# 25 grid cells near peak oak prediction error
oak_ecotone_locs <- expand.grid(x = c(109000, 133000, 157000,
                                      181000, 205000),
                                y = c(1006000, 1030000, 1054000,
                                      1078000, 1102000))

# Location of savanna-forest ecotone peak in oak prediction error
oak_ecotone_locs |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y),
                     fill = '#2a72b2') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/savanna_oak_loc.svg',
                height = 5, width = 5, units = 'cm')
