#### STEP 6-2

## Plot sensitivites based on linear models in each grid cell

## Shows where taxon relative abundances are most sensitive
## to temporal changes in each climate condition

## Input: out/sensitivity/taxon_lm_sensitivity.RData
## Dataframe of linear model coefficient estimates
## relating the relationship between taxon relative abundance
## and environmental conditions over time at each grid cell

## Output: none, except figures saved at figures/sensitivity/
rm(list = ls())

# Helper funs
source('R/funs.R')

#### 1. Load outputs ####

# Load output from taxon and grid cell-level lms
load('out/sensitivity/taxon_lm_sensitivity.RData')

#### 2. Format outputs ####

# Add column names to all matrices
colnames(beech_sens) <- colnames(birch_sens) <-
  colnames(elm_sens) <- colnames(hemlock_sens) <-
  colnames(maple_sens) <- colnames(oak_sens) <-
  colnames(oc_sens) <- colnames(oh_sens) <-
  colnames(pine_sens) <- colnames(spruce_sens) <-
  colnames(tamarack_sens) <-
  c('x', 'y', 'aat', 'aat_sd', 'tpr', 'tpr_sd', 'prsd', 'prsd_sd')

# Convert to dataframes
beech_sens <- as.data.frame(beech_sens)
birch_sens <- as.data.frame(birch_sens)
elm_sens <- as.data.frame(elm_sens)
hemlock_sens <- as.data.frame(hemlock_sens)
maple_sens <- as.data.frame(maple_sens)
oak_sens <- as.data.frame(oak_sens)
oc_sens <- as.data.frame(oc_sens)
oh_sens <- as.data.frame(oh_sens)
pine_sens <- as.data.frame(pine_sens)
spruce_sens <- as.data.frame(spruce_sens)
tamarack_sens <- as.data.frame(tamarack_sens)

# Add taxon column to dataframes
beech_sens <- dplyr::mutate(beech_sens, taxon = 'Beech')
birch_sens <- dplyr::mutate(birch_sens, taxon = 'Birch')
elm_sens <- dplyr::mutate(elm_sens, taxon = 'Elm')
hemlock_sens <- dplyr::mutate(hemlock_sens, taxon = 'Hemlock')
maple_sens <- dplyr::mutate(maple_sens, taxon = 'Maple')
oak_sens <- dplyr::mutate(oak_sens, taxon = 'Oak')
oc_sens <- dplyr::mutate(oc_sens, taxon = 'Other conifer')
oh_sens <- dplyr::mutate(oh_sens, taxon = 'Other hardwood')
pine_sens <- dplyr::mutate(pine_sens, taxon = 'Pine')
spruce_sens <- dplyr::mutate(spruce_sens, taxon = 'Spruce')
tamarack_sens <- dplyr::mutate(tamarack_sens, taxon = 'Tamarack')

# Combine
sens <- rbind(beech_sens, birch_sens,
              elm_sens, hemlock_sens,
              maple_sens, oak_sens,
              oc_sens, oh_sens,
              pine_sens, spruce_sens,
              tamarack_sens)

#### 3. Sensitivity over space ####

# Map of study region
states <- map_states()

# Average annual temperature
sens |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::ggtitle('Sensitivity to mean annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/sensitivity/aat_sens_taxon_facets.png',
                height = 15, width = 20, units = 'cm')

# Total annual precipitation
sens |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::ggtitle('Sensitivity to total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/sensitivity/tpr_sens_taxon_facets.png',
                height = 15, width = 20, units = 'cm')

# Precipitation seasonality
sens |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::ggtitle('Sensitivity to precipitation seasonality') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

# Save
ggplot2::ggsave(plot = ggplot2::last_plot(),
                filename = 'figures/sensitivity/prsd_sens_taxon_facets.png',
                height = 15, width = 20, units = 'cm')
