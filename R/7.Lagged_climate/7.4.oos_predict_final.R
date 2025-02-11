#### STEP 7-4

## Out of sample prediction in space and time (using all spatiotemporal
## locations not used to fit the model)

## This script is identical to step 4-12 (alternative option)

## Input: 

rm(list = ls())

# Load fitted model
#load('/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP_lagged.RData')
load('D:/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP_lagged.RData')

# Loop through fitted model outputs
for(i in 1:length(output)){
  # Save one model run at a time
  out <- output[[i]]
  save(out, file = paste('D:/gjam_out_', i, '.RData'))
  print(i)
}

# Remove total output
rm(output, out)

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

# Loop over posterior draws
for(i in 1:100){
  # Subset for one posterior draw
  sub <- post_oos_all |>
    dplyr::filter(draw == i) |>
    tidyr::drop_na()
  
  # Subset for one GJAM fit
  load(paste0('D:/gjam_out_', i, '.RData'))
  
  # Format xdata (keep all columns to match format from step 7-3)
  xdata <- dplyr::select(sub, clay:prsdLag)
  
  # New data list
  # Number of simulations is dramatically reduced due to extreme
  # computation time. 500 is still a large posterior ensemble,
  # especially since we are only using the predictive mean (no
  # need to characterize the full distribution)
  new_datalist <- list(xdata = xdata,
                       nsim = 500)
  
  pred <- gjam::gjamPredict(output = out,
                            newdata = new_datalist)
  
  save(pred, file = paste0('D:/pred_', i, '.RData'))
  
  print(i)
}

# Save object
pred_all <- list()

# Loop through all the predictions
for(i in 1:100){
  # Open one prediction run
  load(paste0('D:/pred_', i, '.RData'))
  pred_all[[i]] <- pred
  print(i)
}

# Save
save(pred_all,
     file = 'D:/oos_prediction_nonconditional_final_lagged.RData')
