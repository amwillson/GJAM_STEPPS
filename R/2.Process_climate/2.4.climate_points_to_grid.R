### STEP 2-4

## Second part of spatially matching climate to STEPPS 
## Matches climate grid to point-level PLS data
## NOTE: This is separate because this part must be run on VM due to memory constraints
## Requires approximately 40 GB RAM as written

## Input: data/intermediate/clipped_clim_50.RData
## Climate reconstructions for one period of time, with all spatial locations

## Input: .RData files in data/input/PLS_point/
## Contain dataframes with point-level observations of taxon occurrences from the
## Public Land Survey (PLS). The STEPPS model was calibrated on the PLS. From a previous
## project, we have the PLS points that are in each STEPPS grid cell.
## We can use this relationship for the climate data as follows:
## 1. Find which PLS point each climate reconstruction is closest to
## 2. Identify which grid cell each PLS point falls within
## 3. Average over all climate reconstructions in the same grid cell according to which PLS point it is closest to

## Output: data/intermediate/clipped_pls.RData
## PLS data frame with clipped spatial extent (removes lower Michigan since it is not within our domain)
## Only kept so that this step does not need to be run again. Not used in another step

## Output: data/intermediate/point_matched_clim.RData
## Dataframe with contents of clipped_clim_50.RData plus the coordinates of the
## closest PLS point
## Used in 2.5.climate_aggregate_to_grid.R

#### Match points to PLS points ####

# Helper funs
source('R/funs.R')

# Load intermediate step
load('data/intermediate/clipped_clim_50.RData')

# Load PLS point level data for area of interest
load('data/input/PLS_point/minnesota_process.RData')
load('data/input/PLS_point/upmichigan_process.RData')
load('data/input/PLS_point/wisconsin_process.RData')

# Combine all PLS data
pls <- rbind(minnesota, upmichigan, wisconsin)

# Transform so that each point has one line
pls <- pls |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x),
                keep_x = x,
                keep_y = y)

# Define coordinate system
pls <- sf::st_as_sf(pls,
                    coords = c('x', 'y'),
                    crs = 'EPSG:4326')

# Convert coordinate system (because of planar assumption)
pls <- sf::st_transform(pls, crs = 'EPSG:3175')

# Map of study region
states <- map_states()

# Keep only what's in our study region (removes part of lower peninsula)
pls <- sf::st_intersection(pls, states)
# Convert back to data frame
pls <- sfheaders::sf_to_df(pls, fill = TRUE)
# Keep only necessary columns
pls <- dplyr::select(pls, keep_x, keep_y, uniqueID)

save(pls, file = 'data/intermediate/clipped_pls.RData')

# Define coordinate system of climate data
# (we haad transformed earlier)
ey.hat_50 <- sf::st_as_sf(ey.hat_50,
                          coords = c('x', 'y'),
                          crs = 'EPSG:3175')
unbias_50 <- sf::st_as_sf(unbias_50,
                          coords = c('x', 'y'),
                          crs = 'EPSG:3175')
# Transform back to keep things consistent across workflows
ey.hat_50 <- sf::st_transform(ey.hat_50,
                              crs = 'EPSG:4326')
unbias_50 <- sf::st_transform(unbias_50,
                              crs = 'EPSG:4326')
# Convert back to data frame
ey.hat_50 <- sfheaders::sf_to_df(ey.hat_50, fill = TRUE)
unbias_50 <- sfheaders::sf_to_df(unbias_50, fill = TRUE)
# Remove unnecessary columns
ey.hat_50 <- dplyr::select(ey.hat_50, -sfg_id, -point_id)
unbias_50 <- dplyr::select(unbias_50, -sfg_id, -point_id)

# Change name of pls coordinates
pls <- dplyr::rename(pls, x = keep_x, y = keep_y)

# Make two subsets of pls
pls1 <- pls[1:253536,]
pls2 <- pls[253537:nrow(pls),]

# Select coordinates
coords_pls1 <- dplyr::select(pls1, y, x)
coords_pls2 <- dplyr::select(pls2, y, x)
coords_ey.hat <- dplyr::select(ey.hat_50, y, x)
coords_unbias <- dplyr::select(unbias_50, y, x)

# Check that the 2 climate dataframes are identical
identical(coords_ey.hat$y, coords_unbias$y)
identical(coords_ey.hat$x, coords_unbias$x)

### Subset 1 ###

# Find the distance between each pair of points in the pls (1) and climate (2) dataframes
dists <- fields::rdist(coords_pls1, coords_ey.hat)
# Find the closest climate point to each PLS point
# (should have length = nrow(pls subset))
closest_point <- apply(dists, 1, which.min)
# Immediately remove dists because it's huge
rm(dists)

select_ey.hat <- dplyr::slice(ey.hat_50, closest_point)

clim_pls <- cbind(select_ey.hat, pls1)

rm(closest_point, coords_pls1, select_ey.hat, pls1)

### Subset 2 ###

dists <- fields::rdist(coords_pls2, coords_ey.hat)
closest_point <- apply(dists, 1, which.min)
rm(dists)

select_ey.hat <- dplyr::slice(ey.hat_50, closest_point)

temp <- cbind(select_ey.hat, pls2)
clim_pls <- rbind(clim_pls, temp)

rm(closest_point, coords_pls2, select_ey.hat, pls2)

# Update column names
colnames(clim_pls) <- c('time', 'aat', 'tpr', 'tsd', 'prsd', 'spatID', 'clim_x', 'clim_y', 'pls_x', 'pls_y', 'uniqueID')

#### Data checks ####

# Check that coordinates match up
clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = clim_x)) +
  ggplot2::geom_abline()

clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_y, y = clim_y)) +
  ggplot2::geom_abline()

# Check that spatial patterns make sense
clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = aat)) +
  ggplot2::facet_wrap(~time)

clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = tpr)) +
  ggplot2::facet_wrap(~time)

clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = tsd)) +
  ggplot2::facet_wrap(~time)

clim_pls |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = prsd)) +
  ggplot2::facet_wrap(~time)

# Save
save(clim_pls, file = 'data/intermediate/point_matched_clim.RData')
