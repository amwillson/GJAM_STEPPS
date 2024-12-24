### STEP 3-5

## Plot output of GJAM fitted with mean relative abundance estimates from STEPPS

## Input: out/mean/processed_silt_aat_tpr_prsd.RData
## Input: out/mean/processed_sand_aat_tpr_prsd.RData
## Input: out/mean/processed_silt_aat_tsd_prsd.RData
## Input: out/mean/processed_sand_aat_tsd_prsd.RData
## One of these four RData files should be loaded.
## Each has dataframes for the five types of parameters estimated by the model
## (bFacGibbs, bgibbs, bgibbsUn, fSensGibbs, sgibbs)

## Output: none

rm(list = ls())

# Which model format?
# options:
# silt_aat_tpr_prsd
# sand_aat_tpr_prsd
# silt_aat_tsd_prsd
# sand_aat_tsd_prsd
# sand_aat_tpr_prsd_interaction
form <- 'sand_aat_tpr_prsd_interaction'

# Load formatted data
load(paste0('out/mean/processed_', form, '.RData'))

#### Trace plots ####

## Trace plots are used to make sure parameters converged in the MCMC algorithm
## If there are no clear trends across iterations, we infer good convergence
## Note traceplots don't work with interaction term

### bFacGibbs ###

## BEECH

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[1:4], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[1:4],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[5:8], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[5:8],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[9:12], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[9:12],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[13:16], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[13:16],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[17:20], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[17:20],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[21:24], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[21:24],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[25:28], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[25:28],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[29:32], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[29:32],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[33:36], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[33:36],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[37:40], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[37:40],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

bFacGibbs |>
  dplyr::select(c(colnames(bFacGibbs)[41:44], iter)) |>
  tidyr::pivot_longer(colnames(bFacGibbs)[41:44],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### bgibbs ###

## BEECH

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[1:5], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[1:5],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[6:10], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[6:10],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[11:15], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[11:15],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[16:20], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[16:20],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[21:25], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[21:25],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[26:30], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[26:30],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[31:35], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[31:35],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[36:40], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[36:40],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[41:45], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[41:45],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[46:50], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[46:50],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

bgibbs |>
  dplyr::select(c(colnames(bgibbs)[51:55], iter)) |>
  tidyr::pivot_longer(colnames(bgibbs)[51:55],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### bgibbsUn ###

## BEECH

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[1:5], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[1:5],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[6:10], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[6:10],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[11:15], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[11:15],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[16:20], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[16:20],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[21:25], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[21:25],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[26:30], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[26:30],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[31:35], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[31:35],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[36:40], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[36:40],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[41:45], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[41:45],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[46:50], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[46:50],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

bgibbsUn |>
  dplyr::select(c(colnames(bgibbsUn)[51:55], iter)) |>
  tidyr::pivot_longer(colnames(bgibbsUn)[51:55],
                      names_to = 'beta', values_to = 'estimate') |>
  dplyr::mutate(covar = sub(pattern = '.*_', '', beta)) |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~covar, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Coefficient Estimate') +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### fSensGibbs ###

fSensGibbs |>
  tidyr::pivot_longer(colnames(fSensGibbs)[1:4],
                      names_to = 'beta', values_to = 'estimate') |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::geom_line(show.legend = FALSE) +
  ggplot2::facet_wrap(~beta, scales = 'free') +
  ggplot2::xlab('Iteration') + ggplot2::ylab('Sensitivity Estimate') +
  ggplot2::theme_minimal()

### sgibbs ###

## BEECH

sgibbs |>
  dplyr::select(colnames(sgibbs)[1:11], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[1:11],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Beech') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

sgibbs |>
  dplyr::select(colnames(sgibbs)[12:21], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[12:21],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Birch') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

sgibbs |>
  dplyr::select(colnames(sgibbs)[22:30], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[22:30],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Elm') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

sgibbs |>
  dplyr::select(colnames(sgibbs)[31:38], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[31:38],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Hemlock') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

sgibbs |>
  dplyr::select(colnames(sgibbs)[39:45], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[39:45],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Maple') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

sgibbs |>
  dplyr::select(colnames(sgibbs)[46:51], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[46:51],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Oak') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

sgibbs |>
  dplyr::select(colnames(sgibbs)[52:56], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[52:56],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Other Conifer') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

sgibbs |>
  dplyr::select(colnames(sgibbs)[57:60], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[57:60],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Other Hardwood') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

sgibbs |>
  dplyr::select(colnames(sgibbs)[61:63], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[61:63],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Pine') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

sgibbs |>
  dplyr::select(colnames(sgibbs)[64:65], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[64:65],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Spruce') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

sgibbs |>
  dplyr::select(colnames(sgibbs)[66], iter) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[66],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::ggtitle('Covariance with Tamarack') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### Coefficients between taxa and environment ####

## What is the relationship between the environment and each taxon?
## Here, we plot the beta coefficient distributions across iterations

# Number of columns we're working with
cols <- ncol(bFacGibbs)

# Format for distributions
bFacGibbs_long <- bFacGibbs |>
  # Remove iteration column
  dplyr::select(-iter) |>
  # Pivot all remaining columns to long format
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var', values_to = 'val') |>
  # Extract taxon and covariate from the names outputed from GJAM
  dplyr::mutate(taxon = sub(pattern = '_.*', replacement = '', x = var),
                covariate = sub(pattern = '.*_', replacement = '', x = var))

# Remove unnecessary columns
bFacGibbs_corr <- dplyr::select(bFacGibbs, -iter)

# Generate summary statistics (mean, sd, and 95% CI)
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

# Color palette for plots
pal <- RColorBrewer::brewer.pal(n = length(unique(corr$taxon)), name = 'Set3')

# Formatting taxon names
for_plotting <- corr |>
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'beech', 'Beech', taxon),
                taxon = dplyr::if_else(taxon == 'birch', 'Birch', taxon),
                taxon = dplyr::if_else(taxon == 'elm', 'Elm', taxon),
                taxon = dplyr::if_else(taxon == 'hemlock', 'Hemlock', taxon),
                taxon = dplyr::if_else(taxon == 'maple', 'Maple', taxon),
                taxon = dplyr::if_else(taxon == 'oak', 'Oak', taxon),
                taxon = dplyr::if_else(taxon == 'oc', 'Other conifer', taxon),
                taxon = dplyr::if_else(taxon == 'oh', 'Other hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'pine', 'Pine', taxon),
                taxon = dplyr::if_else(taxon == 'spruce', 'Spruce', taxon),
                taxon = dplyr::if_else(taxon == 'tamarack', 'Tamarack', taxon)) |>
  dplyr::rename(Taxon = taxon)

# Formatting covariate names
my_labeller <- ggplot2::as_labeller(x = c(aat = '`Average annual temperature`',
                                          tpr = '`Total annual precipitation`',
                                          tsd = '`Temperature seasonality`',
                                          sand = '`Soil % sand`',
                                          silt = '`Soil % silt`',
                                          prsd = '`Precipitation seasonality`',
                                          `sand:tpr` = '`Soil % sand x precipitation`'),
                                    default = ggplot2::label_parsed)

# Violin plots of coefficient estimates
bFacGibbs_long |>
  # Formatting taxon names
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'beech', 'Beech', taxon),
                taxon = dplyr::if_else(taxon == 'birch', 'Birch', taxon),
                taxon = dplyr::if_else(taxon == 'elm', 'Elm', taxon),
                taxon = dplyr::if_else(taxon == 'hemlock', 'Hemlock', taxon),
                taxon = dplyr::if_else(taxon == 'maple', 'Maple', taxon),
                taxon = dplyr::if_else(taxon == 'oak', 'Oak', taxon),
                taxon = dplyr::if_else(taxon == 'oc', 'Other conifer', taxon),
                taxon = dplyr::if_else(taxon == 'oh', 'Other hardwood', taxon),
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
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 8),
                 strip.text = ggplot2::element_text(size = 10, face = 'bold'),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 axis.text.y = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = paste0('figures/mean/', form, '/coefficients_violin.png'),
                height = 12, width = 20, units = 'cm')

# Box plots of coefficient estimates using coefficient summary statistics
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
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 8),
                 strip.text = ggplot2::element_text(size = 10, face = 'bold'),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 10),
                 axis.text.y = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = paste0('figures/mean/', form, '/coefficients_boxplot.png'),
                height = 12, width = 18, units = 'cm')

#### Covariate sensitivity ####

## Joint sensitivity of all taxa to each covariate
## How much does relative abundance change when each covariate changes?

# Remove the iter column
fSensGibbs_sum <- dplyr::select(fSensGibbs, -iter)

# Generate summary statistics
sens_mean <- apply(fSensGibbs_sum, 2, mean, na.rm = TRUE)
sens_sd <- apply(fSensGibbs_sum, 2, stats::sd, na.rm = TRUE)
sens_lower <- apply(fSensGibbs_sum, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
sens_upper <- apply(fSensGibbs_sum, 2, stats::quantile, probs = 0.975, na.rm = TRUE)

# Format sensitivity summary statistics
sens <- rbind(sens_mean, sens_sd, sens_lower, sens_upper)
rownames(sens) <- c('mean', 'sd', 'lower', 'upper')
sens <- t(sens)
sens <- as.data.frame(sens)
sens <- tibble::rownames_to_column(sens, var = 'covar')

# Boxplots of sensitivity summary statistics
sens |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = reorder(covar, mean, decreasing = FALSE),
                                     ymin = lower, lower = mean - sd, middle = mean, upper = mean + sd, ymax = upper,
                                     color = reorder(covar, mean, decreasing = TRUE)),
                        stat = 'identity', show.legend = FALSE, alpha = 0.5, linewidth = 0.8) +
  ggplot2::coord_flip() +
  ggplot2::xlab('') + ggplot2::ylab(expression(paste('Sensitivity (', hat(F), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_d(option = 'D', end = 0.9) +
  ggplot2::scale_fill_viridis_d(option = 'D', end = 0.9) +
  ggplot2::scale_x_discrete(labels = c('aat' = 'Average annual temperature',
                                       'prsd' = 'Precipitation seasonality',
                                       'tpr' = 'Total precipitation',
                                       'sand' = 'Soil % sand',
                                       'silt' = 'Soil % silt',
                                       'tsd' = 'Temperature seasonality',
                                       'sand:tpr' = 'Soil % sand x precipitation')) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = paste0('figures/mean/', form, '/sensitivity_boxplot.png'),
                height = 8.5, width = 8.5, units = 'cm')

# Violin plots of sensitivity (distribution over Gibbs samples)
fSensGibbs |>
  dplyr::select(-iter) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = 'covariate', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = reorder(covariate, val, decreasing = FALSE),
                                    y = val,
                                    color = reorder(covariate, val, decreasing = TRUE),
                                    fill = reorder(covariate, val, decreasing = TRUE)), 
                       show.legend = FALSE, alpha = 0.5) +
  ggplot2::coord_flip() +
  ggplot2::xlab('') + ggplot2::ylab(expression(paste('Sensitivity (', hat(F), ')'))) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_d(option = 'D', end = 0.9) +
  ggplot2::scale_fill_viridis_d(option = 'D', end = 0.9) +
  ggplot2::scale_x_discrete(labels = c('aat' = 'Average annual temperature',
                                       'prsd' = 'Precipitation seasonality',
                                       'tpr' = 'Total precipitation',
                                       'sand' = 'Soil % sand',
                                       'silt' = 'Soil % silt',
                                       'tsd' = 'Temperature seasonality',
                                       'sand:tpr' = 'Soil % sand x precipitation')) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 10),
                 axis.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = paste0('figures/mean/', form, '/sensitivity_violin.png'),
                height = 8.5, width = 8.5, units = 'cm')

#### Correlations between taxa ####

## After accounting for their joint dependence on the environmental covariates
## included in the model, are there residual correlations between taxa?

# remove unnecessary columns
sgibbs_cor <- dplyr::select(sgibbs, -iter)

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
  c('Beech', 'Birch', 'Elm', 'Hemlock', 'Maple', 'Oak', 
    'Other conifer', 'Other hardwood', 'Pine', 'Spruce', 'Tamarack')

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

# Reset graphing window
# (needed if re-running this section)
dev.off()

png(filename = paste0('figures/mean/', form, '/corr.png'),
    width = 8.5, height = 8.5, units = 'cm', res = 600)

# Without uncertainty
corrplot::corrplot(corr_mat, 
                   diag = FALSE, 
                   type = 'upper', 
                   col = pal, 
                   tl.col = 'black', 
                   tl.cex = 0.7,
                   cl.cex = 0.7,
                   cl.ratio = 0.3)

dev.off()

png(filename = paste0('figures/mean/', form, '/corr_95CI.png'),
    width = 8.5, height = 8.5, units = 'cm', res = 600)

# Plot with uncertainty
corrplot::corrplot(corr_mat, 
                   lowCI.mat = low_mat, 
                   uppCI.mat = upp_mat, 
                   plotCI = 'circle',
                   diag = FALSE, 
                   type = 'upper', 
                   col = pal, 
                   tl.col = 'black', 
                   tl.cex = 0.7,
                   cl.cex = 0.7,
                   cl.ratio = 0.3)

dev.off()

# Plot with uncertainty
# Remove crossing 0
insig <- which(low_mat < 0 & upp_mat > 0)
corr_mat[insig] <- NA
low_mat[insig] <- NA
upp_mat[insig] <- NA

png(filename = paste0('figures/mean/', form, '/corr_95CI_insig.png'),
    width = 8.5, height = 8.5, units = 'cm', res = 600)

corrplot::corrplot(corr_mat,
                   lowCI.mat = low_mat,
                   uppCI.mat = upp_mat,
                   plotCI = 'circle',
                   diag = FALSE,
                   type = 'upper',
                   col = pal,
                   tl.col = 'black',
                   tl.cex = 0.7,
                   na.label = ' ',
                   cl.cex = 0.7,
                   cl.ratio = 0.3)

dev.off()
