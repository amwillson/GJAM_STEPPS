## Check similiarity between model parameterizations
## We want to make sure the intercept term doesn't notably
## affect the slope terms since there is some evidence of
## trade-offs

## Need to check if I labelled columns correctly
## Then check if estimates are close to GJAM

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
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, color = run)) +
  ggplot2::theme_minimal()

# Intercept
ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = 'estimated', y = out$int)) +
  ggplot2::geom_point(ggplot2::aes(x = 'fixed 0', y = 0)) +
  ggplot2::geom_point(ggplot2::aes(x = 'fixed', y = data$int)) +
  ggplot2::theme_minimal()

# Process error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::filter(var2 == 'tauproc') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, color = run)) +
  ggplot2::theme_minimal()

# Observation error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::filter(var2 == 'phiobs') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, color = run)) +
  ggplot2::theme_minimal()

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
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, color = run)) +
  ggplot2::theme_minimal()

# Intercept
ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = 'estimated', y = out$int)) +
  ggplot2::geom_point(ggplot2::aes(x = 'fixed 0', y = 0)) +
  ggplot2::geom_point(ggplot2::aes(x = 'fixed', y = data$int)) +
  ggplot2::theme_minimal()

# Process error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::filter(var2 == 'tauproc') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, color = run)) +
  ggplot2::theme_minimal()

# Observation error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::filter(var2 == 'phiobs') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, color = run)) +
  ggplot2::theme_minimal()

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
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, color = run)) +
  ggplot2::theme_minimal()

# Intercept
ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = 'estimated', y = out$int)) +
  ggplot2::geom_point(ggplot2::aes(x = 'fixed 0', y = 0)) +
  ggplot2::geom_point(ggplot2::aes(x = 'fixed', y = data$int)) +
  ggplot2::theme_minimal()

# Process error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::filter(var2 == 'tauproc') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, color = run)) +
  ggplot2::theme_minimal()

# Observation error
out |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::mutate(var2 = sub(pattern = '_.*', replacement = '', x = var),
                run = sub(pattern = '.*_', replacement = '', x = var)) |>
  dplyr::filter(var2 == 'phiobs') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var2, y = val, color = run)) +
  ggplot2::theme_minimal()
