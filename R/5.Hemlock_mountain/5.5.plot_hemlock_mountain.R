#### STEP 5-5

## Plotting hemlock mountain output
## Plotting coefficient estimates for relatinoship
## between previous and current relative abundance
## and environmental covariates and current relative abundance

## Input: out/hemlock/out_small_hemlock_mountain.RData
## Model output of model fit to small hemlock mountain area

## Input: out/hemlock/out_med_hemlock_mountain.RData
## Model output of model fit to larger hemlock mountain area

## Input: out/hemlock/out_large_hemlock_mountain.RData
## Model output of model fit to largest hemlock mountain area

## Input: out/hemlock/out_full_domain.RData
## Model output of model fit to the entire geographic
## domain in our study

## Output: none, except figures saved to figures/hemlock/

rm(list = ls())

# Load coda package
# Needs to be loaded to mask base::as.matrix()
library(coda)

#### Case 1: Hemlock Mountain region only ####

# Load output from case 1
load('out/hemlock/out_small_hemlock_mountain.RData')

# Convert output to matrix
out <- as.matrix(out)

# Assign more relevant column names
colnames(out) <- c('previous_abundance', 'aat', 'tpr', 'prsd', 'sand', 'intercrept', 'obs_prec', 'proc_prec')

# Make into dataframe
out <- as.data.frame(out)

# Order of x-axis
x_order <- c('Previous\nabundance', 'Average annual\ntemperature',
             'Total annual\nprecipitation', 'Precipitation\nseasonality',
             'Soil % sand')

# Plots all slopes
out |>
  tidyr::pivot_longer(cols = previous_abundance:sand,
                      names_to = 'var', values_to = 'val') |>
  dplyr::mutate(var = dplyr::if_else(var == 'aat', 'Average annual\ntemperature', var),
                var = dplyr::if_else(var == 'previous_abundance', 'Previous\nabundance', var),
                var = dplyr::if_else(var == 'prsd', 'Precipitation\nseasonality', var),
                var = dplyr::if_else(var == 'sand', 'Soil % sand', var),
                var = dplyr::if_else(var == 'tpr', 'Total annual\nprecipitation', var)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(var, levels = x_order), y = val)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient estimate') +
  ggplot2::ggtitle('Small Hemlock Mountain area') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.text = ggplot2::element_text(size = 10),
                 axis.title = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/small_coefficient_estimates.png')

#### Case 2: 49 cell Hemlock Mountain ####

# Load output from case 2
load('out/hemlock/out_med_hemlock_mountain.RData')

# Convert output to matrix
out <- as.matrix(out)

# Assign more relevant column names
colnames(out) <- c('previous_abundance', 'aat', 'tpr', 'prsd', 'sand', 'intercrept', 'obs_prec', 'proc_prec')

# Make into dataframe
out <- as.data.frame(out)

# Order of x-axis
x_order <- c('Previous\nabundance', 'Average annual\ntemperature',
             'Total annual\nprecipitation', 'Precipitation\nseasonality',
             'Soil % sand')

# Plots all slopes
out |>
  tidyr::pivot_longer(cols = previous_abundance:sand,
                      names_to = 'var', values_to = 'val') |>
  dplyr::mutate(var = dplyr::if_else(var == 'aat', 'Average annual\ntemperature', var),
                var = dplyr::if_else(var == 'previous_abundance', 'Previous\nabundance', var),
                var = dplyr::if_else(var == 'prsd', 'Precipitation\nseasonality', var),
                var = dplyr::if_else(var == 'sand', 'Soil % sand', var),
                var = dplyr::if_else(var == 'tpr', 'Total annual\nprecipitation', var)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(var, levels = x_order), y = val)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient estimate') +
  ggplot2::ggtitle('Medium Hemlock Mountain area') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.text = ggplot2::element_text(size = 10),
                 axis.title = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/medium_coefficient_estimates.png')

#### Case 3: 81 cell Hemlock Mountain ####

#### Case 1: Hemlock Mountain region only ####

# Load output from case 3
load('out/hemlock/out_large_hemlock_mountain.RData')

# Convert output to matrix
out <- as.matrix(out)

# Assign more relevant column names
colnames(out) <- c('previous_abundance', 'aat', 'tpr', 'prsd', 'sand', 'intercrept', 'obs_prec', 'proc_prec')

# Make into dataframe
out <- as.data.frame(out)

# Order of x-axis
x_order <- c('Previous\nabundance', 'Average annual\ntemperature',
             'Total annual\nprecipitation', 'Precipitation\nseasonality',
             'Soil % sand')

# Plots all slopes
out |>
  tidyr::pivot_longer(cols = previous_abundance:sand,
                      names_to = 'var', values_to = 'val') |>
  dplyr::mutate(var = dplyr::if_else(var == 'aat', 'Average annual\ntemperature', var),
                var = dplyr::if_else(var == 'previous_abundance', 'Previous\nabundance', var),
                var = dplyr::if_else(var == 'prsd', 'Precipitation\nseasonality', var),
                var = dplyr::if_else(var == 'sand', 'Soil % sand', var),
                var = dplyr::if_else(var == 'tpr', 'Total annual\nprecipitation', var)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(var, levels = x_order), y = val)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient estimate') +
  ggplot2::ggtitle('Large Hemlock Mountain area') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/large_coefficient_estimates.png')

#### Case 4: Entire domain ####

# Load output from case 4
load('out/hemlock/out_full_domain.RData')

# Convert output to matrix
out <- as.matrix(out)

# Assign more relevant column names
colnames(out) <- c('previous_abundance', 'aat', 'tpr', 'prsd', 'sand', 'intercept', 'obs_prec', 'proc_prec')

# Make into dataframe
out <- as.data.frame(out)

# Order of x-axis
x_order <- c('Previous\nabundance', 'Average annual\ntemperature',
             'Total annual\nprecipitation', 'Precipitation\nseasonality',
             'Soil % sand')

# Plot all slopes
out |>
  tidyr::pivot_longer(cols = previous_abundance:sand,
                      names_to = 'var', values_to = 'val') |>
  dplyr::mutate(var = dplyr::if_else(var == 'aat', 'Average annual\ntemperature', var),
                var = dplyr::if_else(var == 'previous_abundance', 'Previous\nabundance', var),
                var = dplyr::if_else(var == 'prsd', 'Precipitation\nseasonality', var),
                var = dplyr::if_else(var == 'sand', 'Soil % sand', var),
                var = dplyr::if_else(var == 'tpr', 'Total annual\nprecipitation', var)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(var, levels = x_order), y = val)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient estimate') +
  ggplot2::ggtitle('Entire geographic domain') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.text = ggplot2::element_text(size = 10),
                 axis.title = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/full_coefficient_estimates.png')
