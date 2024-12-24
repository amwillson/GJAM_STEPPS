## Plotting sensitivity of taxon relative abundances to
## changes in climate variables over space and time

rm(list = ls())

# Helper functions
source('R/funs.R')

#### 1. Load outputs ####

# Load average annual temperature sensitivity
load('out/sensitivity/aat_sensitivity.RData')
# Load total annual precipitation sensitivity
load('out/sensitivity/tpr_sensitivity.RData')
# Load temperature seasonality sensitivity
load('out/sensitivity/tsd_sensitivity.RData')
# Load precipitation seasonality sensitivity
load('out/sensitivity/prsd_sensitivity.RData')

#### 2. Format outputs ####

# Names of first 100 columns
cols_100 <- paste('draw', seq(1:100), sep = '_')

# All column names
all_cols <- c(cols_100, 'time', 'x', 'y')

# Add column names to all matrices
colnames(beech_aat) <- colnames(birch_aat) <- colnames(elm_aat) <-
  colnames(hemlock_aat) <- colnames(maple_aat) <- colnames(oak_aat) <-
  colnames(oc_aat) <- colnames(oh_aat) <- colnames(pine_aat) <-
  colnames(spruce_aat) <- colnames(tamarack_aat) <-
  colnames(beech_tpr) <- colnames(birch_tpr) <- colnames(elm_tpr) <-
  colnames(hemlock_tpr) <- colnames(maple_tpr) <- colnames(oak_tpr) <-
  colnames(oc_tpr) <- colnames(oh_tpr) <- colnames(pine_tpr) <-
  colnames(spruce_tpr) <- colnames(tamarack_tpr) <-
  colnames(beech_tsd) <- colnames(birch_tsd) <- colnames(elm_tsd) <-
  colnames(hemlock_tsd) <- colnames(maple_tsd) <- colnames(oak_tsd) <-
  colnames(oc_tsd) <- colnames(oh_tsd) <- colnames(pine_tsd) <-
  colnames(spruce_tsd) <- colnames(tamarack_tsd) <-
  colnames(beech_prsd) <- colnames(birch_prsd) <- colnames(elm_prsd) <-
  colnames(hemlock_prsd) <- colnames(maple_prsd) <- colnames(oak_prsd) <-
  colnames(oc_prsd) <- colnames(oh_prsd) <- colnames(pine_prsd) <-
  colnames(spruce_prsd) <- colnames(tamarack_prsd) <-
  all_cols

# Convert to dataframes
beech_aat <- as.data.frame(beech_aat)
birch_aat <- as.data.frame(birch_aat)
elm_aat <- as.data.frame(elm_aat)
hemlock_aat <- as.data.frame(hemlock_aat)
maple_aat <- as.data.frame(maple_aat)
oak_aat <- as.data.frame(oak_aat)
oc_aat <- as.data.frame(oc_aat)
oh_aat <- as.data.frame(oh_aat)
pine_aat <- as.data.frame(pine_aat)
spruce_aat <- as.data.frame(spruce_aat)
tamarack_aat <- as.data.frame(tamarack_aat)

beech_tpr <- as.data.frame(beech_tpr)
birch_tpr <- as.data.frame(birch_tpr)
elm_tpr <- as.data.frame(elm_tpr)
hemlock_tpr <- as.data.frame(hemlock_tpr)
maple_tpr <- as.data.frame(maple_tpr)
oak_tpr <- as.data.frame(oak_tpr)
oc_tpr <- as.data.frame(oc_tpr)
oh_tpr <- as.data.frame(oh_tpr)
pine_tpr <- as.data.frame(pine_tpr)
spruce_tpr <- as.data.frame(spruce_tpr)
tamarack_tpr <- as.data.frame(tamarack_tpr)

beech_tsd <- as.data.frame(beech_tsd)
birch_tsd <- as.data.frame(birch_tsd)
elm_tsd <- as.data.frame(elm_tsd)
hemlock_tsd <- as.data.frame(hemlock_tsd)
maple_tsd <- as.data.frame(maple_tsd)
oak_tsd <- as.data.frame(oak_tsd)
oc_tsd <- as.data.frame(oc_tsd)
oh_tsd <- as.data.frame(oh_tsd)
pine_tsd <- as.data.frame(pine_tsd)
spruce_tsd <- as.data.frame(spruce_tsd)
tamarack_tsd <- as.data.frame(tamarack_tsd)

beech_prsd <- as.data.frame(beech_prsd)
birch_prsd <- as.data.frame(birch_prsd)
elm_prsd <- as.data.frame(elm_prsd)
hemlock_prsd <- as.data.frame(hemlock_prsd)
maple_prsd <- as.data.frame(maple_prsd)
oak_prsd <- as.data.frame(oak_prsd)
oc_prsd <- as.data.frame(oc_prsd)
oh_prsd <- as.data.frame(oh_prsd)
pine_prsd <- as.data.frame(pine_prsd)
spruce_prsd <- as.data.frame(spruce_prsd)
tamarack_prsd <- as.data.frame(tamarack_prsd)

#### 3. Sensitivity over space and time ####

# Outline of states
states <- map_states()

# Time order
time_order <- c('1900-1800 YBP',
                '1800-1700 YBP',
                '1700-1600 YBP',
                '1600-1500 YBP',
                '1500-1400 YBP',
                '1400-1300 YBP',
                '1300-1200 YBP',
                '1200-1100 YBP',
                '1100-1000 YBP',
                '1000-900 YBP',
                '900-800 YBP',
                '800-700 YBP',
                '700-600 YBP',
                '600-500 YBP',
                '500-400 YBP',
                '400-300 YBP')

### Sensitivity to average annual temperature ###

## Beech
beech_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')')),
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Birch
birch_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')')),
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Elm
elm_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')')),
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Hemlock
hemlock_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')')),
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Maple
maple_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')')),
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Oak
oak_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')')),
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Other conifer
oc_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Other conifer taxa',
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Other hardwood
oh_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Other hardwood taxa',
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Pine
pine_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')')),
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Spruce
spruce_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')')),
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Tamarack
tamarack_aat |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')')),
                   subtitle = 'Senstivity to average annual temperature') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

### Sensitivity to total annual precipitation ###

## Beech
beech_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Beech (', italic('Fagus grandifolia'), ')')),
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Birch
birch_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Birch (', italic('Betula spp.'), ')')),
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Elm
elm_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')')),
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Hemlock
hemlock_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Hemlock (', italic('Tsuga canadensis'), ')')),
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Maple
maple_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')')),
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Oak
oak_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')')),
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Other conifer
oc_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Other conifer taxa',
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Other hardwood
oh_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle('Other hardwood taxa',
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Pine
pine_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')')),
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Spruce
spruce_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Spruce (', italic('Picea spp.'), ')')),
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))

## Tamarack
tamarack_tpr |>
  # Remove NAs from first time period
  # Since we're looking at differences between time periods, 
  # this has to be NA
  tidyr::drop_na() |>
  # Pivot draws longer
  tidyr::pivot_longer(cols = dplyr::all_of(cols_100),
                      names_to = 'draw',
                      values_to = 'sens') |>
  # Group by time and coordinates
  dplyr::group_by(x, y, time) |>
  # Find median and 20% credibility intervals
  dplyr::summarize(med = median(sens),
                   low = quantile(sens, probs = 0.2),
                   high = quantile(sens, probs = 0.8)) |>
  # Remove anything overlapping 0 at 80% credibility interval
  dplyr::mutate(med = dplyr::if_else(low < 0 & high > 0, med == NA, med)) |>
  # Change time names
  dplyr::mutate(time = as.character(time),
                time = dplyr::if_else(time == '18', '1900-1800 YBP', time),
                time = dplyr::if_else(time == '17', '1800-1700 YBP', time),
                time = dplyr::if_else(time == '16', '1700-1600 YBP', time),
                time = dplyr::if_else(time == '15', '1600-1500 YBP', time),
                time = dplyr::if_else(time == '14', '1500-1400 YBP', time),
                time = dplyr::if_else(time == '13', '1400-1300 YBP', time),
                time = dplyr::if_else(time == '12', '1300-1200 YBP', time),
                time = dplyr::if_else(time == '11', '1200-1100 YBP', time),
                time = dplyr::if_else(time == '10', '1100-1000 YBP', time),
                time = dplyr::if_else(time == '9', '1000-900 YBP', time),
                time = dplyr::if_else(time == '8', '900-800 YBP', time),
                time = dplyr::if_else(time == '7', '800-700 YBP', time),
                time = dplyr::if_else(time == '6', '700-600 YBP', time),
                time = dplyr::if_else(time == '5', '600-500 YBP', time),
                time = dplyr::if_else(time == '4', '500-400 YBP', time),
                time = dplyr::if_else(time == '3', '400-300 YBP', time)) |>
  # Plot over space and time
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = med)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_gradient2(name = 'Sensitivity',
                                na.value = '#00000000',
                                low = '#a0a2fe',
                                mid = '#ffffff',
                                high = '#baae02') +
  ggplot2::theme_void() +
  ggplot2::facet_wrap(~factor(time, levels = time_order)) +
  ggplot2::ggtitle(expression(paste('Tamarack (', italic('Larix laricina'), ')')),
                   subtitle = 'Senstivity to total annual precipitation') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                 strip.text = ggplot2::element_text(size = 10),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 10))
