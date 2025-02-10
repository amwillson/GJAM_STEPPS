#### STEP 7-4

## Out of sample prediction in space and time (using all spatiotemporal
## locations not used to fit the model)

## This script is identical to step 4-12 (alternative option)

## Input: 

rm(list = ls())

# Load fitted model
load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP_lagged.RData')

# Load data
load('data/processed/post_stepps_soil_clim.RData')

# Remove NAs
post_insample_all <- tidyr::drop_na(post_insample_all)
post_oos_all <- tidyr::drop_na(post_oos_all)

# Combine
data1 <- rbind(post_insample_all, post_oos_all)

# Remove
rm(post_insample_all, post_oos_all)

# Load data
load('data/processed/post_stepps_full_oos.RData')

# Remove NAs
post_oos_all <- tidyr::drop_na(post_oos_all)

# Combine
all_data <- rbind(data1, post_oos_all)

# Create lagged climate variables
all_data <- all_data |>
  dplyr::group_by(draw, x, y) |>
  dplyr::arrange(desc(time)) |>
  dplyr::mutate(aatLag = dplyr::lag(aat),
                tprLag = dplyr::lag(tpr),
                prsdLag = dplyr::lag(prsd))

# Load oos data again
load('data/processed/post_stepps_full_oos.RData')

# Add lagged climate variables to oos data
post_oos_all <- post_oos_all |>
  dplyr::left_join(y = dplyr::select(all_data,
                                     x, y, draw, time,
                                     aatLag, tprLag, prsdLag),
                   by = c('x', 'y', 'draw', 'time'))

