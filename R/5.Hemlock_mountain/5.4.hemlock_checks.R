#### STEP 5-4

## Check similiarity between model parameterizations
## We want to make sure the intercept term doesn't notably
## affect the slope terms since there is some evidence of
## trade-offs

## Input: out/hemlock/out_small_hemlock_mountain.RData
## Model output for model with estimated intercept for small
## hemlock mountain region

## Input: out/hemlock/out_small_hemlock_mountain_fixedint0.RData
## Model output for model with intercept fixed at 0
## for small hemlock mountain region

## Input: out/hemlock/out_small_hemlock_mountain_fixedint.RData
## Model output for model with intercept fixed at value
## randomly selected from posterior distribution of intercept
## in model estimating the intercept for small hemlock
## mountain region

## Input: out/hemlock/out_med_hemlock_mountain.RData
## Model output for model with estimated intercept for medium
## hemlock mountain region

## Input: out/hemlock/out_med_hemlock_mountain_fixedint0.RData
## Model output for model with intercept fixed at 0
## for medium hemlock mountain region

## Input: out/hemlock/out_med_hemlock_mountain_fixedint.RData
## Model output for model with intercept fixed at value
## randomly selected from posterior distribution of intercept
## in model estimating the intercept for medium hemlock
## mountain region

## Input: out/hemlock/out_large_hemlock_mountain.RData
## Model output for model with estimated intercept for large
## hemlock mountain region

## Input: out/hemlock/out_large_hemlock_mountain_fixedint0.RData
## Model output for model with intercept fixed at 0
## for large hemlock mountain region

## Input: out/hemlock/out_large_hemlock_mountain_fixedint.RData
## Model output for model with intercept fixed at value
## randomly selected from posterior distribution of intercept
## in model estimating the intercept for large hemlock
## mountain region

## Output: none except figures in figures/hemlock/ directory

rm(list = ls())

#### Small hemlock mountain ####

# Load output with estimated intercept term
load('out/hemlock/out_small_hemlock_mountain.RData')

# Save in different variable
out_int <- out

# Load otuput with intercept term fixed at 0
load('out/hemlock/out_small_hemlock_mountain_fixedint0.RData')

out_fixed0 <- out

# Load output with intercept term fixed at a randomly generated value
load('out/hemlock/out_small_hemlock_mountain_fixedint.RData')

out_fixed <- out

# Format
out_int <- as.matrix(out_int)
out_fixed0 <- as.matrix(out_fixed0)
out_fixed <- as.matrix(out_fixed)

out_int <- as.data.frame(out_int)
out_fixed0 <- as.data.frame(out_fixed0)
out_fixed <- as.data.frame(out_fixed)

# Change column names
colnames(out_int) <- c('previousabundance_int', 'aat_int', 'tpr_int', 
                       'prsd_int', 'sand_int', 'int', 'phiobs_int',
                       'tauproc_int')

colnames(out_fixed0) <- c('previousabundance_fixed0', 'aat_fixed0',
                          'tpr_fixed0', 'prsd_fixed0', 'sand_fixed0',
                          'phiobs_fixed0', 'tauproc_fixed0')

colnames(out_fixed) <- c('previousabundance_fixed', 'aat_fixed',
                         'tpr_fixed', 'prsd_fixed', 'sand_fixed',
                         'phiobs_fixed', 'tauproc_fixed')

# Combine
out <- cbind(out_int, out_fixed0, out_fixed)

# Slopes 
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::filter(!(var2 %in% c('phiobs', 'tauproc', 'int'))) |>
  dplyr::mutate(var2 = dplyr::if_else(var2 == 'aat', 'Average annual\ntemperature', var2),
                var2 = dplyr::if_else(var2 == 'previousabundance', 'Previous\nabundance', var2),
                var2 = dplyr::if_else(var2 == 'prsd', 'Precipitation\nseasonality', var2),
                var2 = dplyr::if_else(var2 == 'sand', 'Soil % sand', var2),
                var2 = dplyr::if_else(var2 == 'tpr', 'Total annual\nprecipitation', var2)) |>
  dplyr::mutate(run = dplyr::if_else(run == 'fixed', 'Fixed intercept', run),
                run = dplyr::if_else(run == 'fixed0', 'Intercept = 0', run),
                run = dplyr::if_else(run == 'int', 'Estimated intercept', run)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(var2,
                                               levels = c('Previous\nabundance',
                                                          'Average annual\ntemperature',
                                                          'Total annual\nprecipitation',
                                                          'Precipitation\nseasonality',
                                                          'Soil % sand')), 
                                    y = val, 
                                    fill = factor(run,
                                                  levels = c('Estimated intercept',
                                                             'Fixed intercept',
                                                             'Intercept = 0')), 
                                    color = factor(run,
                                                   levels = c('Estimated intercept',
                                                              'Fixed intercept',
                                                              'Intercept = 0')))) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient estimate') +
  ggplot2::scale_color_discrete(name = 'Model') +
  ggplot2::scale_fill_discrete(name = 'Model') +
  ggplot2::theme_minimal()

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_small_model_coefficients.png')

# Intercept
ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = 'Estimated intercept', y = out$int)) +
  ggplot2::geom_point(ggplot2::aes(x = 'Intercept = 0', y = 0)) +
  ggplot2::geom_point(ggplot2::aes(x = 'Fixed intercept', y = data$int)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('Model') + ggplot2::ylab('Intercept value') +
  ggplot2::theme_minimal()

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_small_model_intercepts.png')

# Process error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::mutate(run = dplyr::if_else(run == 'fixed', 'Fixed intercept', run),
                run = dplyr::if_else(run == 'fixed0', 'Intercept = 0', run),
                run = dplyr::if_else(run == 'int', 'Estimated intercept', run)) |>
  dplyr::filter(var2 == 'tauproc') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, 
                                    color = factor(run,
                                                   levels = c('Estimated intercept',
                                                              'Fixed intercept',
                                                              'Intercept = 0')), 
                                    fill = factor(run,
                                                  levels = c('Estimated intercept',
                                                             'Fixed intercept',
                                                             'Intercept = 0')))) +
  ggplot2::xlab('Process error') + ggplot2::ylab('Parameter estimate') +
  ggplot2::scale_color_discrete(name = 'Model') + 
  ggplot2::scale_fill_discrete(name = 'Model') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_small_model_processerror.png')

# Observation error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::mutate(run = dplyr::if_else(run == 'fixed', 'Fixed intercept', run),
                run = dplyr::if_else(run == 'fixed0', 'Intercept = 0', run),
                run = dplyr::if_else(run == 'int', 'Estimated intercept', run)) |>
  dplyr::filter(var2 == 'phiobs') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, 
                                    color = factor(run,
                                                   levels = c('Estimated intercept',
                                                              'Fixed intercept',
                                                              'Intercept = 0')), 
                                    fill = factor(run,
                                                  levels = c('Estimated intercept',
                                                             'Fixed intercept',
                                                             'Intercept = 0')))) +
  ggplot2::xlab('Observation error') + ggplot2::ylab('Parameter estimate') +
  ggplot2::scale_color_discrete(name = 'Model') + 
  ggplot2::scale_fill_discrete(name = 'Model') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_small_model_obserror.png')

#### Medium hemlock mountain ####

rm(list = ls())

# Load output with estimated intercept term
load('out/hemlock/out_med_hemlock_mountain.RData')

# Save in different variable
out_int <- out

# Load otuput with intercept term fixed at 0
load('out/hemlock/out_med_hemlock_mountain_fixedint0.RData')

out_fixed0 <- out

# Load output with intercept term fixed at a randomly generated value
load('out/hemlock/out_med_hemlock_mountain_fixedint.RData')

out_fixed <- out

# Format
out_int <- as.matrix(out_int)
out_fixed0 <- as.matrix(out_fixed0)
out_fixed <- as.matrix(out_fixed)

out_int <- as.data.frame(out_int)
out_fixed0 <- as.data.frame(out_fixed0)
out_fixed <- as.data.frame(out_fixed)

# Change column names
colnames(out_int) <- c('previousabundance_int', 'aat_int', 'tpr_int', 
                       'prsd_int', 'sand_int', 'int', 'phiobs_int',
                       'tauproc_int')

colnames(out_fixed0) <- c('previousabundance_fixed0', 'aat_fixed0',
                          'tpr_fixed0', 'prsd_fixed0', 'sand_fixed0',
                          'phiobs_fixed0', 'tauproc_fixed0')

colnames(out_fixed) <- c('previousabundance_fixed', 'aat_fixed',
                         'tpr_fixed', 'prsd_fixed', 'sand_fixed',
                         'phiobs_fixed', 'tauproc_fixed')

# Combine
out <- cbind(out_int, out_fixed0, out_fixed)

# Slopes 
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::filter(!(var2 %in% c('phiobs', 'tauproc', 'int'))) |>
  dplyr::mutate(var2 = dplyr::if_else(var2 == 'aat', 'Average annual\ntemperature', var2),
                var2 = dplyr::if_else(var2 == 'previousabundance', 'Previous\nabundance', var2),
                var2 = dplyr::if_else(var2 == 'prsd', 'Precipitation\nseasonality', var2),
                var2 = dplyr::if_else(var2 == 'sand', 'Soil % sand', var2),
                var2 = dplyr::if_else(var2 == 'tpr', 'Total annual\nprecipitation', var2)) |>
  dplyr::mutate(run = dplyr::if_else(run == 'fixed', 'Fixed intercept', run),
                run = dplyr::if_else(run == 'fixed0', 'Intercept = 0', run),
                run = dplyr::if_else(run == 'int', 'Estimated intercept', run)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(var2,
                                               levels = c('Previous\nabundance',
                                                          'Average annual\ntemperature',
                                                          'Total annual\nprecipitation',
                                                          'Precipitation\nseasonality',
                                                          'Soil % sand')), 
                                    y = val, 
                                    fill = factor(run,
                                                  levels = c('Estimated intercept',
                                                             'Fixed intercept',
                                                             'Intercept = 0')), 
                                    color = factor(run,
                                                   levels = c('Estimated intercept',
                                                              'Fixed intercept',
                                                              'Intercept = 0')))) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient estimate') +
  ggplot2::scale_color_discrete(name = 'Model') +
  ggplot2::scale_fill_discrete(name = 'Model') +
  ggplot2::theme_minimal()

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_med_model_coefficients.png')

# Intercept
ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = 'Estimated intercept', y = out$int)) +
  ggplot2::geom_point(ggplot2::aes(x = 'Intercept = 0', y = 0)) +
  ggplot2::geom_point(ggplot2::aes(x = 'Fixed intercept', y = data$int)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('Model') + ggplot2::ylab('Intercept value') +
  ggplot2::theme_minimal()

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_med_model_intercepts.png')

# Process error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::mutate(run = dplyr::if_else(run == 'fixed', 'Fixed intercept', run),
                run = dplyr::if_else(run == 'fixed0', 'Intercept = 0', run),
                run = dplyr::if_else(run == 'int', 'Estimated intercept', run)) |>
  dplyr::filter(var2 == 'tauproc') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, 
                                    color = factor(run,
                                                   levels = c('Estimated intercept',
                                                              'Fixed intercept',
                                                              'Intercept = 0')), 
                                    fill = factor(run,
                                                  levels = c('Estimated intercept',
                                                             'Fixed intercept',
                                                             'Intercept = 0')))) +
  ggplot2::xlab('Process error') + ggplot2::ylab('Parameter estimate') +
  ggplot2::scale_color_discrete(name = 'Model') + 
  ggplot2::scale_fill_discrete(name = 'Model') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_med_model_processerror.png')

# Observation error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::mutate(run = dplyr::if_else(run == 'fixed', 'Fixed intercept', run),
                run = dplyr::if_else(run == 'fixed0', 'Intercept = 0', run),
                run = dplyr::if_else(run == 'int', 'Estimated intercept', run)) |>
  dplyr::filter(var2 == 'phiobs') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, 
                                    color = factor(run,
                                                   levels = c('Estimated intercept',
                                                              'Fixed intercept',
                                                              'Intercept = 0')), 
                                    fill = factor(run,
                                                  levels = c('Estimated intercept',
                                                             'Fixed intercept',
                                                             'Intercept = 0')))) +
  ggplot2::xlab('Observation error') + ggplot2::ylab('Parameter estimate') +
  ggplot2::scale_color_discrete(name = 'Model') + 
  ggplot2::scale_fill_discrete(name = 'Model') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_med_model_obserror.png')

#### Large hemlock mountain ####

rm(list = ls())

# Load output with estimated intercept term
load('out/hemlock/out_large_hemlock_mountain.RData')

# Save in different variable
out_int <- out

# Load otuput with intercept term fixed at 0
load('out/hemlock/out_large_hemlock_mountain_fixedint0.RData')

out_fixed0 <- out

# Load output with intercept term fixed at a randomly generated value
load('out/hemlock/out_large_hemlock_mountain_fixedint.RData')

out_fixed <- out

# Format
out_int <- as.matrix(out_int)
out_fixed0 <- as.matrix(out_fixed0)
out_fixed <- as.matrix(out_fixed)

out_int <- as.data.frame(out_int)
out_fixed0 <- as.data.frame(out_fixed0)
out_fixed <- as.data.frame(out_fixed)

# Change column names
colnames(out_int) <- c('previousabundance_int', 'aat_int', 'tpr_int', 
                       'prsd_int', 'sand_int', 'int', 'phiobs_int',
                       'tauproc_int')

colnames(out_fixed0) <- c('previousabundance_fixed0', 'aat_fixed0',
                          'tpr_fixed0', 'prsd_fixed0', 'sand_fixed0',
                          'phiobs_fixed0', 'tauproc_fixed0')

colnames(out_fixed) <- c('previousabundance_fixed', 'aat_fixed',
                         'tpr_fixed', 'prsd_fixed', 'sand_fixed',
                         'phiobs_fixed', 'tauproc_fixed')

# Combine
out <- cbind(out_int, out_fixed0, out_fixed)

# Slopes 
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::filter(!(var2 %in% c('phiobs', 'tauproc', 'int'))) |>
  dplyr::mutate(var2 = dplyr::if_else(var2 == 'aat', 'Average annual\ntemperature', var2),
                var2 = dplyr::if_else(var2 == 'previousabundance', 'Previous\nabundance', var2),
                var2 = dplyr::if_else(var2 == 'prsd', 'Precipitation\nseasonality', var2),
                var2 = dplyr::if_else(var2 == 'sand', 'Soil % sand', var2),
                var2 = dplyr::if_else(var2 == 'tpr', 'Total annual\nprecipitation', var2)) |>
  dplyr::mutate(run = dplyr::if_else(run == 'fixed', 'Fixed intercept', run),
                run = dplyr::if_else(run == 'fixed0', 'Intercept = 0', run),
                run = dplyr::if_else(run == 'int', 'Estimated intercept', run)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(var2,
                                               levels = c('Previous\nabundance',
                                                          'Average annual\ntemperature',
                                                          'Total annual\nprecipitation',
                                                          'Precipitation\nseasonality',
                                                          'Soil % sand')), 
                                    y = val, 
                                    fill = factor(run,
                                                  levels = c('Estimated intercept',
                                                             'Fixed intercept',
                                                             'Intercept = 0')), 
                                    color = factor(run,
                                                   levels = c('Estimated intercept',
                                                              'Fixed intercept',
                                                              'Intercept = 0')))) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient estimate') +
  ggplot2::scale_color_discrete(name = 'Model') +
  ggplot2::scale_fill_discrete(name = 'Model') +
  ggplot2::theme_minimal()

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_large_model_coefficients.png')

# Intercept
ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = 'Estimated intercept', y = out$int)) +
  ggplot2::geom_point(ggplot2::aes(x = 'Intercept = 0', y = 0)) +
  ggplot2::geom_point(ggplot2::aes(x = 'Fixed intercept', y = data$int)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('Model') + ggplot2::ylab('Intercept value') +
  ggplot2::theme_minimal()

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_large_model_intercepts.png')

# Process error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::mutate(run = dplyr::if_else(run == 'fixed', 'Fixed intercept', run),
                run = dplyr::if_else(run == 'fixed0', 'Intercept = 0', run),
                run = dplyr::if_else(run == 'int', 'Estimated intercept', run)) |>
  dplyr::filter(var2 == 'tauproc') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, 
                                    color = factor(run,
                                                   levels = c('Estimated intercept',
                                                              'Fixed intercept',
                                                              'Intercept = 0')), 
                                    fill = factor(run,
                                                  levels = c('Estimated intercept',
                                                             'Fixed intercept',
                                                             'Intercept = 0')))) +
  ggplot2::xlab('Process error') + ggplot2::ylab('Parameter estimate') +
  ggplot2::scale_color_discrete(name = 'Model') + 
  ggplot2::scale_fill_discrete(name = 'Model') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_large_model_processerror.png')

# Observation error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::mutate(run = dplyr::if_else(run == 'fixed', 'Fixed intercept', run),
                run = dplyr::if_else(run == 'fixed0', 'Intercept = 0', run),
                run = dplyr::if_else(run == 'int', 'Estimated intercept', run)) |>
  dplyr::filter(var2 == 'phiobs') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, 
                                    color = factor(run,
                                                   levels = c('Estimated intercept',
                                                              'Fixed intercept',
                                                              'Intercept = 0')), 
                                    fill = factor(run,
                                                  levels = c('Estimated intercept',
                                                             'Fixed intercept',
                                                             'Intercept = 0')))) +
  ggplot2::xlab('Observation error') + ggplot2::ylab('Parameter estimate') +
  ggplot2::scale_color_discrete(name = 'Model') + 
  ggplot2::scale_fill_discrete(name = 'Model') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/hemlock/compare_large_model_obserror.png')
