### STEP 4-6

## Out of sample prediction in time (using last time step)
## Using STEPPS posterior draws
## This step must be run on VM because of memory constraints
## Requires ~ 30 GB RAM
## NOTE that this step requires you to change the file path according to what
## machine you're working on
## This is because on a local machine, the files must be saved to an external drive because
## of storage constraints, but on the VM, the files cannot be on an external drive and must
## be saved somewhere accessible.
## On local machine, I saved the files at /Volumes/FileBackup/GJAM_STEPPS_output/
## On VM, I saved/loaded the files at out/posteriors/
## You must change the file path according to what type of machine you're working on

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
## Used in 4.7.plot_oos_prediction_draws.R

rm(list = ls())

# Load fitted models
load('out/posteriors/sand_aat_tpr_prsd/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData')

# Load out-of-sample data
load('data/processed/post_stepps_soil_clim.RData')

#### Non conditional ####

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
    tidyr::drop_na() |>
    dplyr::select(clay:prsd)
  
  # New data list
  new_datalist <- list(xdata = xdata,
                       nsim = 10000)
  
  pred[[i]] <- gjam::gjamPredict(output = out,
                                 newdata = new_datalist)
  print(i)
}

# Save
save(pred,
     file = 'out/posteriors/oos_prediction_nonconditional_time.RData')

#### Conditional on oak ####

rm(pred)

oak_cond_pred <- list()

# Loop over posterior draws
for(i in 1:100){
  sub <- dplyr::filter(post_oos_all, draw == i)
  out <- output[[i]]
  
  xdata <- sub |>
    tidyr::drop_na() |>
    dplyr::select(clay:prsd)
  
  ydata_cond <- sub |>
    tidyr::drop_na() |>
    dplyr::select(OAK)
  
  # nsim must be lower to get the model to run
  new_datalist <- list(ydataCond = ydata_cond,
                       xdata = xdata,
                       nsim = 1000)
  
  oak_cond_pred[[i]] <- gjam::gjamPredict(output = out,
                                          newdata = new_datalist)
  print(i)
}

# Save
save(oak_cond_pred,
     file = 'out/posteriors/oos_prediction_conditionoak_time.RData')
