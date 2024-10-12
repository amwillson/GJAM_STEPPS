## Plotting hemlock mountain output

## How does fixing the intercept change things?
## Define intercept from drawing from a tighter distribution

rm(list = ls())

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
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

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
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

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
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

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
  ggplot2::ggtitle('Entire geographical domain') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### Case 5: Allowing and alpha to vary at Hemlock Mountain ####

# Load output from case 5
load('out/hemlock/out_large_hemlock_mountain_variable.RData')

# Convert output to matrix
out <- as.matrix(out)
out_alpha <- as.matrix(out_alpha)

# Take location order from data
loc_order <- colnames(data$OBS)

# Apply location names to alpha column names
colnames(out_alpha) <- loc_order

# Make dataframe
out_alpha <- as.data.frame(out_alpha)

# Format
out_alpha_df <- out_alpha |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'x_y',
                      values_to = 'val') |>
  dplyr::mutate(x = sub(pattern = '_.*', replacement = '', x = x_y),
                y = sub(pattern = '.*_', replacement = '', x = x_y),
                x = as.numeric(x),
                y = as.numeric(y))

# Helper funs
source('R/funs.R')

# Map of study region
states <- map_states()

# Plot alpha over space
out_alpha_df |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(val = mean(val)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = val)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
