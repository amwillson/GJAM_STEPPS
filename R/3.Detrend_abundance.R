## Detrending relative abundances in space
## Use the relative abundance of the same taxon at the four cells touching
## a given cell ("rook" adjacency) to predict the relative abundance
## of the same taxon at a given cell

rm(list = ls())

# Load helper functions
source('R/funs.R')

# Load taxon ararys
load('data/processed/mean_STEPPS.RData')

# Average relative abundanace in cells adjacent to each cell with
# abundance estimates
ash_adjacents <- average_adjacent_cells(taxon_mat = ash,
                                        ncell = ncell,
                                        x = x,
                                        y = y,
                                        time = time)
beech_adjacents <- average_adjacent_cells(taxon_mat = beech,
                                          ncell = ncell,
                                          x = x,
                                          y = y,
                                          time = time)
birch_adjacents <- average_adjacent_cells(taxon_mat = birch,
                                          ncell = ncell,
                                          x = x,
                                          y = y,
                                          time = time)
elm_adjacents <- average_adjacent_cells(taxon_mat = elm,
                                        ncell = ncell,
                                        x = x,
                                        y = y,
                                        time = time)
hemlock_adjacents <- average_adjacent_cells(taxon_mat = hemlock,
                                            ncell = ncell,
                                            x = x,
                                            y = y,
                                            time = time)
maple_adjacents <- average_adjacent_cells(taxon_mat = maple,
                                          ncell = ncell,
                                          x = x,
                                          y = y,
                                          time = time)
oak_adjacents <- average_adjacent_cells(taxon_mat = oak,
                                        ncell = ncell,
                                        x = x,
                                        y = y,
                                        time = time)
other_conifer_adjacents <- average_adjacent_cells(taxon_mat = other_conifer,
                                                  ncell = ncell,
                                                  x = x,
                                                  y = y,
                                                  time = time)
other_hardwood_adjacents <- average_adjacent_cells(taxon_mat = other_hardwood,
                                                   ncell = ncell,
                                                   x = x,
                                                   y = y,
                                                   time = time)
pine_adjacents <- average_adjacent_cells(taxon_mat = pine,
                                         ncell = ncell,
                                         x = x,
                                         y = y,
                                         time = time)
spruce_adjacents <- average_adjacent_cells(taxon_mat = spruce,
                                           ncell = ncell,
                                           x = x,
                                           y = y,
                                           time = time)
tamarack_adjacents <- average_adjacent_cells(taxon_mat = tamarack,
                                             ncell = ncell,
                                             x = x,
                                             y = y,
                                             time = time)

# Add dimensions to original arrays
dimnames(ash) <- dimnames(beech) <- dimnames(birch) <- dimnames(elm) <-
  dimnames(hemlock) <- dimnames(maple) <- dimnames(oak) <- dimnames(other_conifer) <-
  dimnames(other_hardwood) <- dimnames(pine) <- dimnames(spruce) <- dimnames(tamarack) <-
  list(x, y, time)

# Melt and join the original and adjacent arrays
ash_melt <- melt_join(taxon_mat = ash, adj_mat = ash_adjacents)
beech_melt <- melt_join(taxon_mat = beech, adj_mat = beech_adjacents)
birch_melt <- melt_join(taxon_mat = birch, adj_mat = birch_adjacents)
elm_melt <- melt_join(taxon_mat = elm, adj_mat = elm_adjacents)
hemlock_melt <- melt_join(taxon_mat = hemlock, adj_mat = hemlock_adjacents)
maple_melt <- melt_join(taxon_mat = maple, adj_mat = maple_adjacents)
oak_melt <- melt_join(taxon_mat = oak, adj_mat = oak_adjacents)
other_conifer_melt <- melt_join(taxon_mat = other_conifer, adj_mat = other_conifer_adjacents)
other_hardwood_melt <- melt_join(taxon_mat = other_hardwood, adj_mat = other_hardwood_adjacents)
pine_melt <- melt_join(taxon_mat = pine, adj_mat = pine_adjacents)
spruce_melt <- melt_join(taxon_mat = spruce, adj_mat = spruce_adjacents)
tamarack_melt <- melt_join(taxon_mat = tamarack, adj_mat = tamarack_adjacents)

# Linear models with cell being predicted by surrounding cells
ash_lm <- lm(formula = abundance ~ adjacent_abundance, data = ash_melt)
beech_lm <- lm(formula = abundance ~ adjacent_abundance, data = beech_melt)
birch_lm <- lm(formula = abundance ~ adjacent_abundance, data = birch_melt)
elm_lm <- lm(formula = abundance ~ adjacent_abundance, data = elm_melt)
hemlock_lm <- lm(formula = abundance ~ adjacent_abundance, data = hemlock_melt)
maple_lm <- lm(formula = abundance ~ adjacent_abundance, data = maple_melt)
oak_lm <- lm(formula = abundance ~ adjacent_abundance, data = oak_melt)
other_conifer_lm <- lm(formula = abundance ~ adjacent_abundance, data = other_conifer_melt)
other_hardwood_lm <- lm(formula = abundance ~ adjacent_abundance, data = other_hardwood_melt)
pine_lm <- lm(formula = abundance ~ adjacent_abundance, data = pine_melt)
spruce_lm <- lm(formula = abundance ~ adjacent_abundance, data = spruce_melt)
tamarack_lm <- lm(formula = abundance ~ adjacent_abundance, data = tamarack_melt)

# R2 for each model should be really high
paste0('Ash adjusted R2 = ', summary(ash_lm)$adj.r.squared)
paste0('Beech adjusted R2 = ', summary(beech_lm)$adj.r.squared)
paste0('Birch adjusted R2 = ', summary(birch_lm)$adj.r.squared)
paste0('Elm adjusted R2 = ', summary(elm_lm)$adj.r.squared)
paste0('Hemlock adjusted R2 = ', summary(hemlock_lm)$adj.r.squared)
paste0('Maple adjusted R2 = ', summary(maple_lm)$adj.r.squared)
paste0('Oak adjusted R2 = ', summary(oak_lm)$adj.r.squared)
paste0('Other conifer adjusted R2 = ', summary(other_conifer_lm)$adj.r.squared)
paste0('Other hardwood adjusted R2 = ', summary(other_hardwood_lm)$adj.r.squared)
paste0('Pine adjusted R2 = ', summary(pine_lm)$adj.r.squared)
paste0('Spruce adjusted R2 = ', summary(spruce_lm)$adj.r.squared)
paste0('Tamarack adjusted R2 = ', summary(tamarack_lm)$adj.r.squared)

# Get residuals
ash_resid <- as.data.frame(residuals(ash_lm))
beech_resid <- as.data.frame(residuals(beech_lm))
birch_resid <- as.data.frame(residuals(birch_lm))
elm_resid <- as.data.frame(residuals(elm_lm))
hemlock_resid <- as.data.frame(residuals(hemlock_lm))
maple_resid <- as.data.frame(residuals(maple_lm))
oak_resid <- as.data.frame(residuals(oak_lm))
other_conifer_resid <- as.data.frame(residuals(other_conifer_lm))
other_hardwood_resid <- as.data.frame(residuals(other_hardwood_lm))
pine_resid <- as.data.frame(residuals(pine_lm))
spruce_resid <- as.data.frame(residuals(spruce_lm))
tamarack_resid <- as.data.frame(residuals(tamarack_lm))

# Add row numbers as column
ash_resid <- tibble::rownames_to_column(ash_resid, var = 'id')
beech_resid <- tibble::rownames_to_column(beech_resid, var = 'id')
birch_resid <- tibble::rownames_to_column(birch_resid, var = 'id')
elm_resid <- tibble::rownames_to_column(elm_resid, var = 'id')
hemlock_resid <- tibble::rownames_to_column(hemlock_resid, var = 'id')
maple_resid <- tibble::rownames_to_column(maple_resid, var = 'id')
oak_resid <- tibble::rownames_to_column(oak_resid, var = 'id')
other_conifer_resid <- tibble::rownames_to_column(other_conifer_resid, var = 'id')
other_hardwood_resid <- tibble::rownames_to_column(other_hardwood_resid, var = 'id')
pine_resid <- tibble::rownames_to_column(pine_resid, var = 'id')
spruce_resid <- tibble::rownames_to_column(spruce_resid, var = 'id')
tamarack_resid <- tibble::rownames_to_column(tamarack_resid, var = 'id')

# Join with lat, long and time
resids <- ash_melt |>
  dplyr::select(x, y, time) |>
  tibble::rownames_to_column(var = 'id') |>
  dplyr::left_join(y = ash_resid, by = 'id') |>
  dplyr::left_join(y = beech_resid, by = 'id') |>
  dplyr::left_join(y = birch_resid, by = 'id') |>
  dplyr::left_join(y = elm_resid, by = 'id') |>
  dplyr::left_join(y = hemlock_resid, by = 'id') |>
  dplyr::left_join(y = maple_resid, by = 'id') |>
  dplyr::left_join(y = oak_resid, by = 'id') |>
  dplyr::left_join(y = other_conifer_resid, by = 'id') |>
  dplyr::left_join(y = other_hardwood_resid, by = 'id') |>
  dplyr::left_join(y = pine_resid, by = 'id') |>
  dplyr::left_join(y = spruce_resid, by = 'id') |>
  dplyr::left_join(y = tamarack_resid, by = 'id') |>
  dplyr::select(-id)

# Add column names
colnames(resids) <- c('x', 'y', 'time', 'ash', 'beech', 'birch',
                      'elm', 'hemlock', 'maple', 'oak', 'other_conifer',
                      'other_hardwood', 'pine', 'spruce', 'tamarack')

# Save
save(resids, file = 'data/processed/mean_residuals.RData')
