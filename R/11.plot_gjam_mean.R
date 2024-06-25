## Processing output from mean model run

rm(list = ls())

# Load model output
load('out/mean_stepps.RData')

# parameters
niter <- 10000
burnin <- 2500

# Save iterations in separate objects
bFacGibbs <- out$chains$bFacGibbs
bgibbs <- out$chains$bgibbs
bgibbsUn <- out$chains$bgibbsUn
fSensGibbs <- out$chains$fSensGibbs
sgibbs <- out$chains$sgibbs

# Formatting
bFacGibbs <- as.data.frame(bFacGibbs)
bgibbs <- as.data.frame(bgibbs)
bgibbsUn <- as.data.frame(bgibbsUn)
fSensGibbs <- as.data.frame(fSensGibbs)
sgibbs <- as.data.frame(sgibbs)

# Add iteration index to dataframes
bFacGibbs$iter <- bgibbs$iter <- bgibbsUn$iter <- fSensGibbs$iter <- sgibbs$iter <- seq(from = 1, to = niter, by = 1)

# Remove burnin
bFacGibbs <- dplyr::filter(bFacGibbs, iter > burnin)
bgibbs <- dplyr::filter(bgibbs, iter > burnin)
bgibbsUn <- dplyr::filter(bgibbsUn, iter > burnin)
fSensGibbs <- dplyr::filter(fSensGibbs, iter > burnin)
sgibbs <- dplyr::filter(sgibbs, iter > burnin)

#### Trace plots ####

### bFacGibbs ###

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[1:20], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[1:20],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal()

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[21:40], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[21:40],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal()

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[41:44], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[41:44],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal()

### bgibbs ###

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[1:20], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[1:20],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal()

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[21:40], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[21:40],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal()

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[41:55], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[41:55],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal()

### bgibbsUn ###

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[1:20], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[1:20],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal()

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[21:40], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[21:40],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal()

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[41:55], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[41:55],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::theme_minimal()

### fSensGibbs ###

fSensGibbs |>
  tidyr::pivot_longer(sand:prsd,
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Sensitivity Estimate') +
  ggplot2::theme_minimal()

### sgibbs ###

sgibbs |>
  dplyr::select(c(colnames(sgibbs)[1:20], iter)) |>
  tidyr::pivot_longer(colnames(sgibbs)[1:20],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance estimate') +
  ggplot2::theme_minimal()

sgibbs |>
  dplyr::select(c(colnames(sgibbs)[21:40], iter)) |>
  tidyr::pivot_longer(colnames(sgibbs)[21:40],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance estimate') +
  ggplot2::theme_minimal()

sgibbs |>
  dplyr::select(c(colnames(sgibbs)[41:60], iter)) |>
  tidyr::pivot_longer(colnames(sgibbs)[41:60],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance estimate') +
  ggplot2::theme_minimal()

sgibbs |>
  dplyr::select(c(colnames(sgibbs)[61:66], iter)) |>
  tidyr::pivot_longer(colnames(sgibbs)[61:66],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Covariance estimate') +
  ggplot2::theme_minimal()

#### Correlations between taxa and environment ####

# Number of columns we're working with
cols <- ncol(bFacGibbs)

# Format for distributions
bFacGibbs_long <- bFacGibbs |>
  dplyr::select(-iter) |>
  tidyr::pivot_longer(beech_sand:tamarack_prsd,
                      names_to = 'var', values_to = 'val') |>
  dplyr::mutate(taxon = sub(pattern = '_.*', replacement = '', x = var),
                covariate = sub(pattern = '.*_', replacement = '', x = var))

# Remove unnecessary columsn and generate summary statistics
bFacGibbs_corr <- bFacGibbs |>
  dplyr::select(-iter)
corr_mean <- apply(bFacGibbs_corr, 2, mean, na.rm = TRUE)
corr_sd <- apply(bFacGibbs_corr, 2, stats::sd, na.rm = TRUE)
corr_lower <- apply(bFacGibbs_corr, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
corr_upper <- apply(bFacGibbs_corr, 2, stats::quantile, probs = 0.975, na.rm = TRUE)

# Formatting our summary statistics
corr <- rbind(corr_mean, corr_sd, corr_lower, corr_upper)
rownames(corr) <- c('mean', 'sd', 'lower', 'upper')
corr <- t(corr)
corr <- as.data.frame(corr)
corr <- corr |>
  tibble::rownames_to_column(var = 'beta') |>
  dplyr::mutate(taxon = sub('_.*', '', beta),
                covariate = sub('.*_', '', beta))

pal <- RColorBrewer::brewer.pal(n = length(unique(corr$taxon)), name = 'Set3')

for_plotting <- corr |>
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'beech', 'Beech', taxon),
                taxon = dplyr::if_else(taxon == 'birch', 'Birch', taxon),
                taxon = dplyr::if_else(taxon == 'elm', 'Elm', taxon),
                taxon = dplyr::if_else(taxon == 'hemlock', 'Hemlock', taxon),
                taxon = dplyr::if_else(taxon == 'maple', 'Maple', taxon),
                taxon = dplyr::if_else(taxon == 'oak', 'Oak', taxon),
                taxon = dplyr::if_else(taxon == 'conifer', 'Other Conifer', taxon),
                taxon = dplyr::if_else(taxon == 'hardwood', 'Other Hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'pine', 'Pine', taxon),
                taxon = dplyr::if_else(taxon == 'spruce', 'Spruce', taxon),
                taxon = dplyr::if_else(taxon == 'tamarack', 'Tamarack', taxon)) |>
  dplyr::rename(Taxon = taxon)

my_labeller <- ggplot2::as_labeller(x = c(aat = '`Average annual temperature`',
                                          tpr = '`Total annual precipitation`',
                                          sand = '`Soil % sand`',
                                          prsd = '`Precipitation seasonality`'),
                                    default = ggplot2::label_parsed)

bFacGibbs_long |>
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'beech', 'Beech', taxon),
                taxon = dplyr::if_else(taxon == 'birch', 'Birch', taxon),
                taxon = dplyr::if_else(taxon == 'elm', 'Elm', taxon),
                taxon = dplyr::if_else(taxon == 'hemlock', 'Hemlock', taxon),
                taxon = dplyr::if_else(taxon == 'maple', 'Maple', taxon),
                taxon = dplyr::if_else(taxon == 'oak', 'Oak', taxon),
                taxon = dplyr::if_else(taxon == 'conifer', 'Other Conifer', taxon),
                taxon = dplyr::if_else(taxon == 'hardwood', 'Other Hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'pine', 'Pine', taxon),
                taxon = dplyr::if_else(taxon == 'spruce', 'Spruce', taxon),
                taxon = dplyr::if_else(taxon == 'tamarack', 'Tamarack', taxon)) |>
  dplyr::rename(Taxon = taxon) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = tidytext::reorder_within(Taxon, val, covariate), y = val, color = Taxon, fill = Taxon), alpha = 0.8) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
  tidytext::scale_x_reordered(ggplot2::aes(x = tidytext::reorder_within(Taxon, val, covariate), y = val, color = Taxon)) +
  ggplot2::facet_wrap(~covariate, labeller = my_labeller, scales = 'free') +
  ggplot2::ylab('Coefficient estimate') +
  ggplot2::scale_color_viridis_d(option = 'H') +
  ggplot2::scale_fill_viridis_d(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12),
                 strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                 legend.title = ggplot2::element_text(size = 14),
                 legend.text = ggplot2::element_text(size = 12),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 12))

for_plotting |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = tidytext::reorder_within(Taxon, mean, covariate), ymin = lower, lower = mean - sd,
                                     middle = mean, upper = mean + sd, ymax = upper,
                                     color = Taxon), stat = 'identity') +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = 'darkgrey', linetype = 'dashed') +
  tidytext::scale_x_reordered(ggplot2::aes(x = tidytext::reorder_within(Taxon, mean, covariate), y = val, color = Taxon)) +
  ggplot2::facet_wrap(~covariate, labeller = my_labeller, scales = 'free') +
  ggplot2::ylab('Coefficient estimate') +
  ggplot2::scale_color_viridis_d(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12),
                 strip.text = ggplot2::element_text(size = 14, face = 'bold'),
                 legend.title = ggplot2::element_text(size = 14),
                 legend.text = ggplot2::element_text(size = 12),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 14),
                 axis.text.y = ggplot2::element_text(size = 12))

### Covariate sensitivity ###

# Do some cleaning of the sensitivity fSensGibbs
fSensGibbs_sum <- fSensGibbs |>
  dplyr::select(-iter)
sens_mean <- apply(fSensGibbs_sum, 2, mean, na.rm = TRUE)
sens_sd <- apply(fSensGibbs_sum, 2, stats::sd, na.rm = TRUE)
sens_lower <- apply(fSensGibbs_sum, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
sens_upper <- apply(fSensGibbs_sum, 2, stats::quantile, probs = 0.975, na.rm = TRUE)

sens <- rbind(sens_mean, sens_sd, sens_lower, sens_upper)
rownames(sens) <- c('mean', 'sd', 'lower', 'upper')
sens <- t(sens)
sens <- as.data.frame(sens)
sens <- sens |>
  tibble::rownames_to_column(var = 'covar')

sens |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = reorder(covar, mean, decreasing = FALSE),
                                     ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper,
                                     color = reorder(covar, mean, decreasing = TRUE)), stat = 'identity', show.legend = FALSE) +
  ggplot2::coord_flip() +
  ggplot2::xlab('') + ggplot2::ylab(expression(paste('Sensitivity (', hat(F), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_d(option = 'D', end = 0.9) +
  ggplot2::scale_x_discrete(labels = c('aat' = 'Average annual temperature',
                                       'prsd' = 'Precipitation seasonality',
                                       'tpr' = 'Total precipitation',
                                       'sand' = 'Soil % sand')) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 14),
                 axis.text = ggplot2::element_text(size = 12))

fSensGibbs |>
  dplyr::select(-iter) |>
  tidyr::pivot_longer(sand:prsd, names_to = 'covariate', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = reorder(covariate, val, decreasing = FALSE),
                                    y = val,
                                    color = reorder(covariate, val, decreasing = TRUE)), show.legend = FALSE) +
  ggplot2::coord_flip() +
  ggplot2::xlab('') + ggplot2::ylab(expression(paste('Sensitivity (', hat(F), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_d(option = 'D', end = 0.9) +
  ggplot2::scale_x_discrete(labels = c('aat' = 'Average annual temperature',
                                       'prsd' = 'Precipitation seasonality',
                                       'tpr' = 'Total precipitation',
                                       'sand' = 'Soil % sand')) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 14),
                 axis.text = ggplot2::element_text(size = 12))

### Correlations between taxa ###

# remove unnecessary columns
sgibbs_cor <- sgibbs |>
  dplyr::select(-iter)
# Get summary statistics
mean_sgibbs <- apply(sgibbs_cor, 2, mean)
sd_sgibbs <- apply(sgibbs_cor, 2, stats::sd)
lower_sgibbs <- apply(sgibbs_cor, 2, stats::quantile, probs = 0.025)
upper_sgibbs <- apply(sgibbs_cor, 2, stats::quantile, probs = 0.975)

# Need to put into the matrix format
# This gives the index for each entry of the matrix
ind <- rbind(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
             c(2, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
             c(3, 13, 22, 23, 24, 25, 26, 27, 28, 29, 30),
             c(4, 14, 23, 31, 32, 33, 34, 35, 36, 37, 38),
             c(5, 15, 24, 32, 39, 40, 41, 42, 43, 44, 45),
             c(6, 16, 25, 33, 40, 46, 47, 48, 49, 50, 51),
             c(7, 17, 26, 34, 41, 47, 52, 53, 54, 55, 56),
             c(8, 18, 27, 35, 42, 48, 53, 57, 58, 59, 60),
             c(9, 19, 28, 36, 43, 49, 54, 58, 61, 62, 63),
             c(10, 20, 29, 37, 44, 50, 55, 59, 62, 64, 65),
             c(11, 21, 30, 38, 45, 51, 56, 60, 63, 65, 66))

# Now format the output into a matrix
corr_mat <- mean_sgibbs[ind]
corr_mat <- matrix(corr_mat, nrow = sqrt(length(corr_mat)), ncol = sqrt(length(corr_mat)))
corr_mat <- stats::cov2cor(corr_mat)
colnames(corr_mat) <- rownames(corr_mat) <- 
  c('Beech', 'Birch', 'Elm', 'Hemlock', 'Maple', 'Oak', 'Other Conifer', 'Other Hardwood', 'Pine', 'Spruce', 'Tamarack')

# Specify color palette
pal <- RColorBrewer::brewer.pal(n = 9, name = 'RdBu')

# Upper and lower credible intervals
low_mat <- lower_sgibbs[ind]
low_mat <- matrix(low_mat, nrow = nrow(corr_mat), ncol = nrow(corr_mat))
low_mat <- stats::cov2cor(low_mat)
colnames(low_mat) <- rownames(low_mat) <- colnames(corr_mat)

upp_mat <- upper_sgibbs[ind]
upp_mat <- matrix(upp_mat, nrow = nrow(corr_mat), ncol = nrow(corr_mat))
upp_mat <- stats::cov2cor(upp_mat)
colnames(upp_mat) <- rownames(upp_mat) <- colnames(corr_mat)

# Plot with uncertainty
corrplot::corrplot(corr_mat, lowCI.mat = low_mat, uppCI.mat = upp_mat, plotCI = 'circle',
                   diag = FALSE, type = 'upper', col = pal, tl.col = 'black', tl.cex = 1.4)

# Without uncertainty
corrplot::corrplot(corr_mat, diag = FALSE, type = 'upper', col = pal, tl.col = 'black', tl.cex = 1.4)
