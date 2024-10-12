#### Hemlock Mountain ####

# Maximum at x = 445000, y = 982000
# Surrounding x = 397000, 421000, 469000, 493000
# Surrounding y = 934000, 958000, 1006000, 1030000

## Note different size samples

#hemlock_mountain_locs <- expand.grid(x = c(373000, 397000, 421000,
#                                           445000, 469000, 493000,
#                                           517000),
#                                     y = c(910000, 934000, 958000,
#                                           982000, 1006000, 1030000,
#                                           1054000))
#hemlock_mountain_locs <- expand.grid(x = c(349000, 373000, 397000,
#                                           421000, 445000, 469000,
#                                           493000, 517000, 541000),
#                                     y = c(886000, 910000, 934000,
#                                           958000, 982000, 1006000,
#                                           1030000, 1054000, 1078000))
#hemlock_mountain_locs <- expand.grid(x = c(397000, 421000, 445000,
#                                           469000, 493000),
#                                     y = c(934000, 958000, 982000,
#                                           1006000, 1030000))
#hemlock_mountain_locs$loc <- paste0(hemlock_mountain_locs$x, '_', hemlock_mountain_locs$y)

# Subset data for only Hemlock Mountain region
#xdata_hm <- dplyr::filter(xdata, loc %in% hemlock_mountain_locs$loc)
#ydata_hm <- dplyr::filter(ydata, loc %in% hemlock_mountain_locs$loc)

xdata_hm <- xdata
ydata_hm <- ydata

# Map of study region
states <- map_states()

# Plot subsetted area
ydata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                limits = c(0, 1),
                                name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~time) +
  ggplot2::ggtitle('Hemlock relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Plot covariates
xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::theme_void()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::theme_void()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = aat, color = loc),
                     show.legend = FALSE) +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::theme_void()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = tpr, color = loc),
                     show.legend = FALSE) +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000') +
  ggplot2::theme_void()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = prsd, color = loc),
                     show.legend = FALSE) +
  ggplot2::theme_minimal()

# Standardize covariates
xdata_hm <- xdata_hm |>
  dplyr::mutate(sand_stand = sand / sd(sand),
                aat_stand = aat / sd(aat),
                tpr_stand = tpr / sd(tpr),
                prsd_stand = prsd / sd(prsd)) |>
  dplyr::select(-sand, -aat, -tpr, -prsd) |>
  dplyr::rename(sand = sand_stand,
                aat = aat_stand,
                tpr = tpr_stand,
                prsd = prsd_stand)

## Pivot data
ydata_wide <- ydata_hm |>
  dplyr::select(-stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = hemlock,
                     names_from = loc) |>
  dplyr::arrange(time)
sand_wide <- xdata_hm |>
  dplyr::select(-aat, -tpr, -prsd, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = sand,
                     names_from = loc) |>
  dplyr::arrange(time)
aat_wide <- xdata_hm |>
  dplyr::select(-sand, -tpr, -prsd, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = aat,
                     names_from = loc) |>
  dplyr::arrange(time)
tpr_wide <- xdata_hm |>
  dplyr::select(-sand, -aat, -prsd, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = tpr,
                     names_from = loc) |>
  dplyr::arrange(time)
prsd_wide <- xdata_hm |>
  dplyr::select(-sand, -aat, -tpr, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = prsd,
                     names_from = loc) |>
  dplyr::arrange(time)

# Identify which grid cells are part of hemlock mountain
hemlock_mountain_locs <- expand.grid(x = c(397000, 421000, 445000,
                                           469000, 493000),
                                     y = c(934000, 958000, 982000,
                                           1006000, 1030000))

hemlock_mountain_locs$loc <- paste0(hemlock_mountain_locs$x, '_', hemlock_mountain_locs$y)

# Make ID column to uniquely identify each cell in hemlock mountain
hemlock_mountain_locs2 <- hemlock_mountain_locs |>
  dplyr::select(loc) |>
  tibble::rownames_to_column(var = 'id')

# The ID for all other cells
background_id <- nrow(hemlock_mountain_locs2) + 1

# Add IDs to all grid cells
hm_cells <- ydata_hm |>
  dplyr::left_join(y = hemlock_mountain_locs2,
                   by = 'loc') |>
  dplyr::mutate(id = as.numeric(id),
                id = dplyr::if_else(is.na(id), background_id, id))

# Plot
hm_cells |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = id)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'F', name = 'Grid cell') +
  ggplot2::theme_void()

hm_cells_wide <- hm_cells |>
  dplyr::select(-stepps_x, -stepps_y, -hemlock) |>
  tidyr::pivot_wider(values_from = id,
                     names_from = loc) |>
  dplyr::arrange(time)

