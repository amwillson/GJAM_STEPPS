### STEP 4-12

## Plotting posterior parameter estimates from STEPPS posterior samples
## with 300 YBP used to fit the model
## Identical to step 4-7 but using 300 YBP time step
## 1. Trace plots
## 2. Environment-vegetation relationships
## 3. Vegetation-vegetation correlations

## Input: out/posteriors/sand_aat_tpr_prsd_300YBP/bFacGibbs.RDS
## Input: out/posteriors/sand_aat_tpr_prsd_300YBP/bgibbs.RDS
## Input: out/posteriors/sand_aat_tpr_prsd_300YBP/bgibbsUn.RDS
## Input: out/posteriors/sand_aat_tpr_prsd_300YBP/fSensGibbs.RDS
## Input: out/posteriors/sand_aat_tpr_prsd_300YBP/sgibbs.RDS
## Each parameter type is loaded separately in data frame format
## to facilitate easy loading/manipulation of the full chains
## Used for making trace plots
## These must be manually copied from VM because they are too large for Github

## Input: out/posteriors/sand_aat_tpr_prsd_300YBP/parameter_summaries.RData
## Summary statistics over full chains. All parameter types are saved together
## in different data frames
## Currently not used but could easily be used instead of full chains
## Included in repo

## Output: none

rm(list = ls())

#### Trace plots ####

# Load gibbs samples
bFacGibbs <- readRDS(file = 'out/posteriors/sand_aat_tpr_prsd_300YBP/bFacGibbs.RDS')
bgibbs <- readRDS(file = 'out/posteriors/sand_aat_tpr_prsd_300YBP/bgibbs.RDS')
bgibbsUn <- readRDS(file = 'out/posteriors/sand_aat_tpr_prsd_300YBP/bgibbsUn.RDS')
fSensGibbs <- readRDS(file = 'out/posteriors/sand_aat_tpr_prsd_300YBP/fSensGibbs.RDS')
sgibbs <- readRDS(file = 'out/posteriors/sand_aat_tpr_prsd_300YBP/sgibbs.RDS')

### bFacGibbs ###

## BEECH

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[1:4], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[1:4],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[5:8], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[5:8],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[9:12], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[9:12],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[13:16], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[13:16],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[17:20], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[17:20],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[21:24], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[21:24],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[25:28], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[25:28],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[29:32], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[29:32],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[33:36], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[33:36],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[37:40], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[37:40],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

bFacGibbs |>
  dplyr::select(colnames(bFacGibbs)[41:44], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[41:44],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### bgibbs ###

## BEECH

bgibbs |>
  dplyr::select(colnames(bgibbs)[1:5], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[1:5],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

bgibbs |>
  dplyr::select(colnames(bgibbs)[6:10], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[6:10],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

bgibbs |>
  dplyr::select(colnames(bgibbs)[11:15], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[11:15],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

bgibbs |>
  dplyr::select(colnames(bgibbs)[16:20], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[16:20],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

bgibbs |>
  dplyr::select(colnames(bgibbs)[21:25], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[21:25],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

bgibbs |>
  dplyr::select(colnames(bgibbs)[26:30], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[26:30],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

bgibbs |>
  dplyr::select(colnames(bgibbs)[31:35], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[31:35],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

bgibbs |>
  dplyr::select(colnames(bgibbs)[36:40], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[36:40],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

bgibbs |>
  dplyr::select(colnames(bgibbs)[41:45], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[41:45],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

bgibbs |>
  dplyr::select(colnames(bgibbs)[46:50], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[46:50],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

bgibbs |>
  dplyr::select(colnames(bgibbs)[51:55], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[51:55],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### bgibbsUn ###

## BEECH

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[1:5], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[1:5],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[6:10], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[6:10],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[11:15], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[11:15],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[16:20], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[16:20],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[21:25], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[21:25],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[25:30], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[25:30],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[31:35], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[31:35],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[36:40], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[36:40],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[41:45], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[41:45],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[46:50], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[46:50],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

bgibbsUn |>
  dplyr::select(colnames(bgibbsUn)[51:55], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[51:55],
                      names_to = 'taxon_param', values_to = 'estimate') |>
  dplyr::mutate(param = sub(pattern = '.*_', replacement = '', x = taxon_param)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### fSensGibbs ###

fSensGibbs |>
  tidyr::pivot_longer(cols = colnames(fSensGibbs)[1:4],
                      names_to = 'param', values_to = 'estimate') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~param, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Parameter Sensitivity') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

### sgibbs ###

## BEECH

sgibbs |>
  dplyr::select(colnames(sgibbs)[1:11], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[1:11],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Beech') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## BIRCH

sgibbs |>
  dplyr::select(colnames(sgibbs)[12:21], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[12:21],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Birch') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## ELM

sgibbs |>
  dplyr::select(colnames(sgibbs)[22:30], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[22:30],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Elm') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## HEMLOCK

sgibbs |>
  dplyr::select(colnames(sgibbs)[30:38], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[30:38],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Hemlock') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## MAPLE

sgibbs |>
  dplyr::select(colnames(sgibbs)[39:45], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[39:45],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Maple') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OAK

sgibbs |>
  dplyr::select(colnames(sgibbs)[46:51], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[46:51],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Oak') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER CONIFER

sgibbs |>
  dplyr::select(colnames(sgibbs)[52:56], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[52:56],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Other Conifer') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## OTHER HARDWOOD

sgibbs |>
  dplyr::select(colnames(sgibbs)[57:60], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[57:60],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Other Hardwood') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## PINE

sgibbs |>
  dplyr::select(colnames(sgibbs)[61:63], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[61:63],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Pine') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## SPRUCE

sgibbs |>
  dplyr::select(colnames(sgibbs)[64:65], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[64:65],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Spruce') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## TAMARACK

sgibbs |>
  dplyr::select(colnames(sgibbs)[66], iter, draw) |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[66],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = iter, y = estimate, color = draw)) +
  ggplot2::facet_wrap(~taxon2, scales = 'free') +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle('Covariance with Tamarack') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### Correlations between taxa and environment ####

# Number of columns we're working with
cols <- ncol(bFacGibbs)

# Format for distributions
bFacGibbs_long <- bFacGibbs |>
  dplyr::select(-iter, -draw) |>
  tidyr::pivot_longer(dplyr::everything(),
                      names_to = 'var', values_to = 'val') |>
  dplyr::mutate(taxon = sub(pattern = '_.*', replacement = '', x = var),
                covariate = sub(pattern = '.*_', replacement = '', x = var))

# Remove unnecessary columns and generate summary statistics
bFacGibbs_corr <- dplyr::select(bFacGibbs, -iter, -draw)
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
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'BEECH', 'Beech', taxon),
                taxon = dplyr::if_else(taxon == 'BIRCH', 'Birch', taxon),
                taxon = dplyr::if_else(taxon == 'ELM', 'Elm', taxon),
                taxon = dplyr::if_else(taxon == 'HEMLOCK', 'Hemlock', taxon),
                taxon = dplyr::if_else(taxon == 'MAPLE', 'Maple', taxon),
                taxon = dplyr::if_else(taxon == 'OAK', 'Oak', taxon),
                taxon = dplyr::if_else(taxon == 'OTHER.CONIFER', 'Other Conifer', taxon),
                taxon = dplyr::if_else(taxon == 'OTHER.HARDWOOD', 'Other Hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'PINE', 'Pine', taxon),
                taxon = dplyr::if_else(taxon == 'SPRUCE', 'Spruce', taxon),
                taxon = dplyr::if_else(taxon == 'TAMARACK', 'Tamarack', taxon)) |>
  dplyr::rename(Taxon = taxon)

my_labeller <- ggplot2::as_labeller(x = c(aat = '`Average annual temperature`',
                                          tpr = '`Total annual precipitation`',
                                          sand = '`Soil % sand`',
                                          prsd = '`Precipitation seasonality`',
                                          silt = '`Soil % silt`',
                                          tsd = '`Temperature seasonality`'),
                                    default = ggplot2::label_parsed)

bFacGibbs_long |>
  dplyr::mutate(taxon = dplyr::if_else(taxon == 'BEECH', 'Beech', taxon),
                taxon = dplyr::if_else(taxon == 'BIRCH', 'Birch', taxon),
                taxon = dplyr::if_else(taxon == 'ELM', 'Elm', taxon),
                taxon = dplyr::if_else(taxon == 'HEMLOCK', 'Hemlock', taxon),
                taxon = dplyr::if_else(taxon == 'MAPLE', 'Maple', taxon),
                taxon = dplyr::if_else(taxon == 'OAK', 'Oak', taxon),
                taxon = dplyr::if_else(taxon == 'OTHER.CONIFER', 'Other Conifer', taxon),
                taxon = dplyr::if_else(taxon == 'OTHER.HARDWOOD', 'Other Hardwood', taxon),
                taxon = dplyr::if_else(taxon == 'PINE', 'Pine', taxon),
                taxon = dplyr::if_else(taxon == 'SPRUCE', 'Spruce', taxon),
                taxon = dplyr::if_else(taxon == 'TAMARACK', 'Tamarack', taxon)) |>
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
  ggplot2::geom_boxplot(ggplot2::aes(x = tidytext::reorder_within(Taxon, mean, covariate),
                                     ymin = lower, lower = mean - sd,
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

#### Covariate sensitivity ####

# Do some cleaning of the sensitivity fSensGibbs
fSensGibbs_sum <- dplyr::select(fSensGibbs, -iter, -draw)
sens_mean <- apply(fSensGibbs_sum, 2, mean, na.rm = TRUE)
sens_sd <- apply(fSensGibbs_sum, 2, stats::sd, na.rm = TRUE)
sens_lower <- apply(fSensGibbs_sum, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
sens_upper <- apply(fSensGibbs_sum, 2, stats::quantile, probs = 0.975, na.rm = TRUE)

sens <- rbind(sens_mean, sens_sd, sens_lower, sens_upper)
rownames(sens) <- c('mean', 'sd', 'lower', 'upper')
sens <- t(sens)
sens <- as.data.frame(sens)
sens <- tibble::rownames_to_column(sens, var = 'covar')

sens |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = reorder(covar, mean, decreasing = FALSE),
                                     ymin = lower, lower = mean - sd, middle = mean,
                                     upper = mean + sd, ymax = upper,
                                     color = reorder(covar, mean, decreasing = TRUE),
                                     fill = reorder(covar, mean, decreasing = TRUE)),
                        stat = 'identity', show.legend = FALSE, alpha = 0.5) +
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
                                       'tsd' = 'Temperature seasonality')) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 14),
                 axis.text = ggplot2::element_text(size = 12))

fSensGibbs |>
  dplyr::select(-iter, -draw) |>
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
                                       'tsd', 'Temperature seasonality')) +
  ggplot2::theme(axis.title = ggplot2::element_text(size = 14),
                 axis.text = ggplot2::element_text(size = 12))

#### Correlations between taxa ####

# remove unnecessary columns
sgibbs_cor <- dplyr::select(sgibbs, -iter, -draw)

# Get summary statistics
mean_sgibbs <- apply(sgibbs_cor, 2, mean)
sd_sgibbs <- apply(sgibbs_cor, 2, stats::sd)
sgibbs_0.025 <- apply(sgibbs_cor, 2, stats::quantile, probs = 0.025)
sgibbs_0.975 <- apply(sgibbs_cor, 2, stats::quantile, probs = 0.975)
sgibbs_0.25 <- apply(sgibbs_cor, 2, stats::quantile, probs = 0.25)
sgibbs_0.75 <- apply(sgibbs_cor, 2, stats::quantile, probs = 0.75)

# Need to put into the matrix foramt
# This gives the index for each entry of the matrix
ind <- rbind(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ,11),
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
  c('Beech', 'Birch', 'Elm', 'Hemlock', 'Maple', 
    'Oak', 'Other Conifer', 'Other Hardwood',
    'Pine', 'Spruce', 'Tamarack')

# Specify color palette
pal <- RColorBrewer::brewer.pal(n = 9, name = 'RdBu')

# Upper and lower credible intervals
# Two different confidence levels
low_mat_95 <- sgibbs_0.025[ind]
low_mat_50 <- sgibbs_0.25[ind]
low_mat_95 <- matrix(low_mat_95, nrow = nrow(corr_mat), ncol = nrow(corr_mat))
low_mat_50 <- matrix(low_mat_50, nrow = nrow(corr_mat), ncol = nrow(corr_mat))
low_mat_95 <- stats::cov2cor(low_mat_95)
low_mat_50 <- stats::cov2cor(low_mat_50)
colnames(low_mat_95) <- colnames(low_mat_50) <-
  rownames(low_mat_95) <- rownames(low_mat_50) <-
  colnames(corr_mat)
# Force between 0 and 1
low_mat_95[low_mat_95 < -1] <- -1

upp_mat_95 <- sgibbs_0.975[ind]
upp_mat_50 <- sgibbs_0.75[ind]
upp_mat_95 <- matrix(upp_mat_95, nrow = nrow(corr_mat), ncol = nrow(corr_mat))
upp_mat_50 <- matrix(upp_mat_50, nrow = nrow(corr_mat), ncol = nrow(corr_mat))
upp_mat_95 <- stats::cov2cor(upp_mat_95)
upp_mat_50 <- stats::cov2cor(upp_mat_50)
colnames(upp_mat_95) <- colnames(upp_mat_50) <-
  rownames(upp_mat_95) <- rownames(upp_mat_50) <-
  colnames(corr_mat)

# +/- 1 SD
low_mat_sd <- mean_sgibbs - sd_sgibbs
upp_mat_sd <- mean_sgibbs + sd_sgibbs
low_mat_sd <- low_mat_sd[ind]
upp_mat_sd <- upp_mat_sd[ind]
low_mat_sd <- matrix(low_mat_sd, nrow = nrow(corr_mat), ncol = nrow(corr_mat))
upp_mat_sd <- matrix(upp_mat_sd, nrow = nrow(corr_mat), ncol = nrow(corr_mat))
low_mat_sd <- stats::cov2cor(low_mat_sd)
upp_mat_sd <- stats::cov2cor(upp_mat_sd)
colnames(low_mat_sd) <- colnames(upp_mat_sd) <- 
  rownames(low_mat_sd) <- rownames(upp_mat_sd) <-
  colnames(corr_mat)

# Plot without uncertainty
corrplot::corrplot(corr_mat, diag = FALSE, type = 'upper',
                   col = pal, tl.col = 'black', tl.cex = 1.4)

# Plot with 95% credible interval
corrplot::corrplot(corr_mat, lowCI.mat = low_mat_95, uppCI.mat = upp_mat_95,
                   plotCI = 'circle', diag = FALSE, type = 'upper',
                   col = pal, tl.col = 'black', tl.cex = 1.4)

# Plot with 50% credible interval
corrplot::corrplot(corr_mat, lowCI.mat = low_mat_50, uppCI.mat = upp_mat_50,
                   plotCI = 'circle', diag = FALSE, type = 'upper',
                   col = pal, tl.col = 'black', tl.cex = 1.4)

# Plot with +/- 1 SD
corrplot::corrplot(corr_mat, lowCI.mat = low_mat_sd, uppCI.mat = upp_mat_sd,
                   plotCI = 'circle', diag = FALSE, type = 'upper',
                   col = pal, tl.col = 'black', tl.cex = 1.4)
