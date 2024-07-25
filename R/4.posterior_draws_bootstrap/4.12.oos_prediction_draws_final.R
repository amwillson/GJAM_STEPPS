### STEP 4-13

## Out of sample prediction in space and time (using all spatiotemporal
## locations not used to fit the model)
## This step must be run on VM because of memory constraints
## Requires ~50 GB RAM
## NOTE that this step requires you to change the file path according to what
## machine you're working on
## This is because on a local machine, the files must be saved to an external drive because
## of storage constraints, but on the VM, the files cannot be on an external drive and must
## be saved somewhere accessible.
## On local machine, I saved the files at /Volumes/FileBackup/GJAM_STEPPS_output/
## On VM, I saved/loaded the files at out/posteriors/
## You must change the file path according to what type of machine you're working on

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

# Load out-of-sample data
load('data/processed/post_stepps_full_oos.RData')

#### Non-conditional ####

# Storage
pred <- list()

# Loop over posterior draws
for(i in 1:100){
  # Subset for one posterior draw
  sub <- dplyr::filter(post_oos_all, draw == i)
  # Subset for one GJAM fit
  out <- output[[i]]
  
  # Format xdata (keep all columns to match format from step 4-5)
  xdata <- dplyr::select(sub, clay:prsd)
  
  # New data list
  # Number of simulations is dramatically reduced due to extreme
  # computation time. 500 is still a large posterior ensemble,
  # especially since we are only using the predictive mean (no
  # need to characterize the full distribution)
  new_datalist <- list(xdata = xdata,
                       nsim = 500)
  
  pred[[i]] <- gjam::gjamPredict(output = out,
                                 newdata = new_datalist)
  print(i)
}

# Save
save(pred,
     file = 'out/posteriors/oos_prediction_nonconditional_final.RData')

#### Conditional on oak ####

rm(pred)

oak_cond_pred <- list()

# Loop over posterior draws
for(i in 1:100){
  sub <- dplyr::filter(post_oos_all, draw == i)
  out <- output[[i]]
  
  # Format xdata (keep all columns to match format from step 4-5)
  xdata <- dplyr::select(sub, clay:prsd)
  
  # Format ydata
  ydata_cond <- dplyr::select(sub, OAK)
  
  new_datalist <- list(ydataCond = ydata_cond,
                       xdata = xdata,
                       nsim = 500)
  
  oak_cond_pred[[i]] <- gjam::gjamPredict(output = out,
                                          newdata = new_datalist)
  
  print(i)
}

# Save
save(oak_cond_pred,
     file = 'out/posteriors/oos_prediction_conditionoak_final.RData')
