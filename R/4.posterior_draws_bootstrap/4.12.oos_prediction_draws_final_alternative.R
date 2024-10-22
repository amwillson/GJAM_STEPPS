### STEP 4-12 (alternative option)

## Out of sample prediction in space and time (using all spatiotemporal
## locations not used to fit the model)
## This script offers an alternative method for running step 4-12
## because the original method requires so much RAM and SWAP.
## This method leverages a local machine's storage (I used an external SSD)
## and separates the output from GJAM into 100 separate objects, which
## are loaded into the local environment one at a time instead of all at once
## Similarly, predictions for each run are saved one at a time, and then
## combined into one object afterwards. The same products are ultimately
## saved in this version as in the original version of step 4-12, but
## there are hundreds of intermediate outputs to reduce RAM usage
## You may need to change file paths here if you are saving files to an
## external drive instead of directories inside this repository
## For simplicity, I have saved intermediate products on my SSD and
## the final products inside this repository

## Input: out/posteriors/sand_aat_tpr_prsd_300YBP/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP.RData
## Output from GJAM with sand, aat, tpr, prsd covariates with posterior draws including 300 YBP time step

## Input: data/processed/post_stepps_full_oos.RData
## OOS data for everything not used to fit the model

## Output: out/posteriors/oos_prediction_nonconditional_final.RData
## Output: out/posteriors/oos_prediction_conditionaloak_final.RData
## Out of sample predictions for non-conditional, and conditional only on
## oak relative abundances
## Out of sample predictions are made for all spatiotemporal locations not used
## to fit the model
## Used in 4.13.plot_oos_prediction_draws_final.R

rm(list = ls())

# Load fitted model
load('out/posteriors/sand_aat_tpr_prsd_300YBP/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP.RData')

# Loop through fitted model outputs
for(i in 1:length(output)){
  # Save one model run at a time
  out <- output[[i]]
  save(out, file = paste0('D:/gjam_out_', i, '.RData'))
  print(i)
}

# Remove total output
rm(output, out)

# Load out-of-sample data
load('data/processed/post_stepps_full_oos.RData')

#### Non-conditional ####

# Loop over posterior draws
for(i in 1:100){
  # Subset for one posterior draw
  sub <- post_oos_all |> 
    dplyr::filter(draw == i) |>
    tidyr::drop_na()
  
  # Subset for one GJAM fit
  load(paste0('D:/gjam_out_', i, '.RData'))
  
  # Format xdata (keep all columns to match format from step 4-5)
  xdata <- dplyr::select(sub, clay:prsd)
  
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
     file = 'out/posteriors/oos_prediction_nonconditional_final.RData')

#### Conditional on oak ####

rm(pred)

oak_cond_pred <- list()

# Loop over posterior draws
for(i in 1:100){
  sub <- post_oos_all |>
    dplyr::filter(draw == i) |>
    tidyr::drop_na()
  
  # Subset for one GJAM fit
  load(paste0('D:/gjam_out_', i, '.RData'))
  
  # Format xdata (keep all columns to match format from step 4-5)
  xdata <- dplyr::select(sub, clay:prsd)
  
  # Format ydata
  ydata_cond <- dplyr::select(sub, OAK)
  
  new_datalist <- list(ydataCond = ydata_cond,
                       xdata = xdata,
                       nsim = 500)
  
  pred <- gjam::gjamPredict(output = out,
                            newdata = new_datalist)
  
  save(pred, file = paste0('D:/cond_pred_', i, '.RData'))
  
  print(i)
}

# Save object
oak_cond_pred <- list()

# Loop through all the predictions
for(i in 1:100){
  # Open one prediction run
  load(paste0('D:/cond_pred_', i, '.RData'))
  oak_cond_pred[[i]] <- pred
  print(i)
}

# Save
save(oak_cond_pred,
     file = 'out/posteriors/oos_prediction_conditionoak_final.RData')
