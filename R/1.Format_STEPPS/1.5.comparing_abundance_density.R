## Mapping composition to density
## We want to see whether areas of high oak abundance
## are related to areas of low tree density basically

## NOTE that if you are including environment in the figure
## you actually have to run this AFTER step 3-11

rm(list = ls())

# Helper funs
source('R/funs.R')

#### STEPPS abundance data processing ####

# Load data
load('data/processed/mean_STEPPS.RData')

# Format into dataframes
ash_melt <- melt_array(taxon_mat = ash,
                       x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'ash'))
beech_melt <- melt_array(taxon_mat = beech,
                         x = x, y = y, time = time,
                         col_names = c('x', 'y', 'time', 'beech'))
birch_melt <- melt_array(taxon_mat = birch,
                         x = x, y = y, time = time,
                         col_names = c('x', 'y', 'time', 'birch'))
elm_melt <- melt_array(taxon_mat = elm,
                       x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'elm'))
hemlock_melt <- melt_array(taxon_mat = hemlock,
                           x = x, y = y, time = time,
                           col_names = c('x', 'y', 'time', 'hemlock'))
maple_melt <- melt_array(taxon_mat = maple,
                         x = x, y = y, time = time,
                         col_names = c('x', 'y', 'time', 'maple'))
oak_melt <- melt_array(taxon_mat = oak,
                       x = x, y = y, time = time,
                       col_names = c('x', 'y', 'time', 'oak'))
other_conifer_melt <- melt_array(taxon_mat = other_conifer,
                                 x = x, y = y, time = time,
                                 col_names = c('x', 'y', 'time', 'other_conifer'))
other_hardwood_melt <- melt_array(taxon_mat = other_hardwood,
                                  x = x, y = y, time = time,
                                  col_names = c('x', 'y', 'time', 'other_hardwood'))
pine_melt <- melt_array(taxon_mat = pine,
                        x = x, y = y, time = time,
                        col_names = c('x', 'y', 'time', 'pine'))
spruce_melt <- melt_array(taxon_mat = spruce,
                          x = x, y = y, time = time,
                          col_names = c('x', 'y', 'time', 'spruce'))
tamarack_melt <- melt_array(taxon_mat = tamarack,
                            x = x, y = y, time = time,
                            col_names = c('x', 'y', 'time', 'tamarack'))

# Combine individual taxon dataframes into one
taxon_melt <- ash_melt |>
  dplyr::left_join(y = beech_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = birch_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = elm_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = hemlock_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = maple_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = oak_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = other_conifer_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = other_hardwood_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = pine_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = spruce_melt, by = c('x', 'y', 'time')) |>
  dplyr::left_join(y = tamarack_melt, by = c('x', 'y', 'time'))

# Keep only 200 YBP time step because it's closest to PLS records
taxon_melt <- dplyr::filter(taxon_melt, time == 2)

# Map of study region
states <- map_states()

# Plot just to make sure spatial patterns look right, especially
# for oak being high abundance in area we think is open canopy
taxon_melt |>
  tidyr::pivot_longer(cols = ash:tamarack,
                      names_to = 'taxon',
                      values_to = 'abundance') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = abundance)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, na.value = 'white') +
  ggplot2::theme_void()

#### PLS density data processing ####

# Load PLS-era density data product
# This is from a previous PalEON project
# https://doi.org/10.6073/pasta/1b2632d48fc79b370740a7c20a70b4b0
pls_density <- ncdf4::nc_open('data/input/PLS_Density_Point_Level2_v1.0.nc')

# Get dimensions
pls_x <- ncdf4::ncvar_get(pls_density, 'x')
pls_y <- ncdf4::ncvar_get(pls_density, 'y')

# Get total density
pls_density <- ncdf4::ncvar_get(pls_density, 'Total')

# Apply coordinates to columns and rows
colnames(pls_density) <- pls_y
rownames(pls_density) <- pls_x

# Melt to datafrane
pls_density <- reshape2::melt(pls_density)

# Add column names
colnames(pls_density) <- c('x', 'y', 'total_density')

# Plot to see what we're working with
# We see that the spatial domain is larger
# Than what we need
pls_density |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = total_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1, na.value = 'white') +
  ggplot2::theme_void()

# Convert to spatial object
pls_density <- sf::st_as_sf(pls_density, coords = c('x', 'y'),
                            crs = 'EPSG:3175')

# Clip to extent of state map
pls_density_clip <- sf::st_intersection(pls_density, states)

# Convert back to dataframe
pls_density_clip <- sfheaders::sf_to_df(pls_density_clip, fill = TRUE)
# Remove unnecessary columns
pls_density_clip <- dplyr::select(pls_density_clip, total_density, x, y)

# Plot to make sure we clipped correctly
pls_density_clip |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = total_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                na.value = 'white') +
  ggplot2::theme_void()

# Convert stem density to ecosystem
# Following Goring et al. 2016, who followed Anderson and Anderson 1975
pls_density <- pls_density_clip |>
  dplyr::mutate(ecosystem = dplyr::if_else(total_density < 0.5, 'prairie', NA),
                ecosystem = dplyr::if_else(total_density >= 0.5 & total_density <= 47, 'savanna', ecosystem),
                ecosystem = dplyr::if_else(total_density > 47, 'forest', ecosystem)) |>
  tidyr::drop_na()

# Plot ecosystem classificaiton
pls_density |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ecosystem)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_d(option = 'B', begin = 0.8, end = 0.2,
                                limits = c('prairie', 'savanna', 'forest'),
                                labels = c('Prairie', 'Savanna', 'Forest')) +
  ggplot2::theme_void()

#### Covariates ####

# Load soil data
load('data/processed/gridded_soil.RData')

# Join STEPPS reconstructions and soil data
taxon_soil <- taxon_melt |>
  dplyr::left_join(y = dplyr::filter(soil_grid, time == 2),
                   by = c('x', 'y'))

# Plot
taxon_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                na.value = 'white',
                                name = 'Soil %\nsand',
                                direction = 1) +
  ggplot2::theme_void()

# Load climate data
load('data/processed/gridded_climate.RData')

# Plot
climate_grid |>
  dplyr::filter(time %in% 3:19) |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(mean = mean(tpr)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey50') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y, fill = mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'G',
                                name = 'Precipitation\n(mm/year)') +
  ggplot2::theme_void()

#### Plots ####

# STEPPS figures: selected taxa abundance at 200 YBP
p1 <- taxon_melt |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                na.value = '#00000000',
                                name = 'Fraction total\nstems',
                                limits = c(0, 1)) +
  ggplot2::ggtitle(expression(paste('Oak (', italic('Quercus spp.'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))
p1

p2 <- taxon_melt |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = elm)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                na.value = '#00000000',
                                name = 'Fraction total\nstems',
                                limits = c(0, 1)) +
  ggplot2::ggtitle(expression(paste('Elm (', italic('Ulmus spp.'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))
p2

p3 <- taxon_melt |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = pine)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                na.value = '#00000000',
                                name = 'Fraction total\nstems',
                                limits = c(0, 1)) +
  ggplot2::ggtitle(expression(paste('Pine (', italic('Pinus spp.'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))
p3

p4 <- taxon_melt |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = maple)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens', direction = 1,
                                na.value = '#00000000',
                                name = 'Fraction total\nstems',
                                limits = c(0, 1)) +
  ggplot2::ggtitle(expression(paste('Maple (', italic('Acer spp.'), ')'))) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))
p4

leg <- ggpubr::get_legend(p1)

up <- cowplot::plot_grid(p1 + ggplot2::theme(legend.position = 'none'),
                         p2 + ggplot2::theme(legend.position = 'none'),
                         leg,
                         nrow = 1, rel_widths = c(0.7, 0.7, 0.25))
down <- cowplot::plot_grid(p3 + ggplot2::theme(legend.position = 'none'),
                           p4 + ggplot2::theme(legend.position = 'none'),
                           leg,
                           nrow = 1, rel_widths = c(0.7, 0.7, 0.25))

# Combine STEPPS figures
pp1 <- cowplot::plot_grid(up, down, nrow = 2)
pp1

# PLS figures: total stem density and ecosystem classification
p5 <- pls_density |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = total_density)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller('Blues', direction = 1,
                                na.value = '#00000000',
                                name = 'Stems/ha') +
  ggplot2::ggtitle('PLSS tree stem density') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))
p5

p6 <- pls_density |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ecosystem)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_d(option = 'B',
                                na.value = '#00000000',
                                name = '',
                                begin = 0.8, end = 0.2,
                                limits = c('prairie', 'savanna', 'forest'),
                                labels = c('Prairie', 'Savanna', 'Forest')) +
  ggplot2::ggtitle('PLSS ecosystem') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))
p6

# Join PLS figures together
pp2 <- cowplot::plot_grid(p5, p6, nrow = 2)
pp2

# Environment figures: total annual precipitation and soil sand
# Explain some of the variation but not all
p7 <- climate_grid |>
  dplyr::filter(time == 2) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'G',
                                na.value = '#00000000',
                                name = 'mm/\nyear') +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))
p7

p8 <- taxon_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                na.value = '#00000000',
                                name = '% sand') +
  ggplot2::ggtitle('Soil texture') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))
p8

# Join environment figures
pp3 <- cowplot::plot_grid(p7, p8, nrow = 2)
pp3

# Join all together
ppp <- cowplot::plot_grid(pp1, pp2, pp3, nrow = 1, rel_widths = c(0.65, 0.4, 0.35))
ppp

ggplot2::ggsave(plot = ppp,
                filename = 'figures/data/plss_abundance_density.png',
                width = 11, height = 4.2, units = 'in', dpi = 600)
