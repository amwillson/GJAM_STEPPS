### STEP 7-2

## Out of sample prediction in time (using last time step)
## Using STEPPS posterior draws

## Input: out/posteriors/sand_aat_tpr_prsd/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData
## Output from GJAM with sand, aat, tpr, prsd covariates with posterior draws

## Input: data/processed/post_stepps_soil_clim.RData
## OOS data

## Output: out/posteriors/oos_prediction_nonconditional_time.RData
## Output: out/posteriors/oos_prediction_conditionaloak_time.RData
## Out of sample predictions for non-conditional and conditional only on
## oak relative abundances
## Out of sample predictions are made for the 300 YBP time period across the same
## spatial locations as used to fit the model
## Used in 7.5.compare_predictions.R

rm(list = ls())

# Load fitted models
load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/GJAM_STEPPS_post_sand_aat_tpr_prsd_lagged.RData')

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

# Created lagged climate variables
all_data <- all_data |>
  dplyr::group_by(draw, x, y) |>
  dplyr::arrange(desc(time)) |>
  dplyr::mutate(aatLag = dplyr::lag(aat),
                tprLag = dplyr::lag(tpr),
                prsdLag = dplyr::lag(prsd))

# Load original data again
load('data/processed/post_stepps_soil_clim.RData')

# Add lagged climate variable to oos data
post_oos_all <- post_oos_all |>
  dplyr::left_join(y = dplyr::select(all_data,
                                     x, y, draw, time,
                                     aatLag, tprLag, prsdLag),
                   by = c('x', 'y', 'draw', 'time'))

# Storage
pred <- list()

# Loop over posterior draws
for(i in 1:100){
  # Subset for one posterior draw
  sub <- dplyr::filter(post_oos_all, draw == i)
  # Subset for one GJAM Fit
  out <- output[[i]]
  
  # Format xdata (keep all columns to match format from step 4-3)
  xdata <- sub |>
    dplyr::ungroup() |>
    tidyr::drop_na() |>
    dplyr::select(clay:prsdLag)
  
  # New data list
  new_datalist <- list(xdata = xdata,
                       nsim = 10000)
  
  pred[[i]] <- gjam::gjamPredict(output = out,
                                 newdata = new_datalist)
  print(i)
}

# Save
save(pred,
     file = '/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/oos_prediction_nonconditional_time_lagged.RData')
