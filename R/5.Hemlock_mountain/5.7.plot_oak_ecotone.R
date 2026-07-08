#### STEP 5-7

## Plotting oak ecotone output
## Plotting coefficient estimates for relationship
## between previous and current relative abundance
## and environmental covariates and current relative abundance

## Input: out/hemlock/out_small_oak_ecotone.RData
## Model output of model fit to small oak ecotone area
## (where oak prediction bias is worst)

## Input: out/hemlock/out_med_oak_ecotone.RData
## Model output of model fit to larger oak ecotone area

## Input: out/hemlock/out_med_oak_ecotone.RData
## Model output of model fit to largest oak ecotone area

## Input: out/hemlock/out_oak_full_domain.RData
## Model output of model fit to the entire geographic
## domain in our study area

## Output: non, except figures saved to figures/hemlock/

rm(list = ls())

# Load coda package
# Needs to be loaded to mask base::as.matrix()
library(coda)

#### 1. Case 1: savanna-forest ecotone region only ####

# Load output from case 1
load('out/hemlock/out_small_oak_ecotone.RData')

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
  ggplot2::ggtitle('Small savanna-forest ecotone area') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.text = ggplot2::element_text(size = 10),
                 axis.title = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/small_oak_coefficient_estimates.png')

#### Case 2: 49 cell savanna-forest ecotone region ####

# Load output from case 2
load('out/hemlock/out_med_oak_ecotone.RData')

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
  ggplot2::ggtitle('Medium savanna-forest ecotone area') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.text = ggplot2::element_text(size = 10),
                 axis.title = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/medium_oak_coefficient_estimates.png')

#### Case 3: 81 cell savanna-forest ecotone region ####

# Load output from case 3
load('out/hemlock/out_large_oak_ecotone.RData.RData')

# Convert output to matrix
out <- as.matrix(out)

colnames(out) <- c('previous_abundance', 'aat', 'tpr', 'prsd', 'sand', 'intercept', 'obs_prec', 'proc_prec')

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
  ggplot2::geom_violin(ggplot2::aes(x = factor(var, levels = x_order), val)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient estimate') +
  ggplot2::ggtitle('Large savanna-forest ecotone area') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/large_oak_coefficient_estimates.png')

#### Case 4: Entire domain ####

# Load output from case 4
load('out/hemlock/out_oak_full_domain.RData')

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
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 axis.text = ggplot2::element_text(size = 10),
                 axis.title = ggplot2::element_text(size = 10))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/full_oak_coefficient_estimates.png')
