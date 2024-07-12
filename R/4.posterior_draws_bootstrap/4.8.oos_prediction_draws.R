### STEP 4-8

## Out of sample prediction in time (using last time step)
## Using STEPPS posterior draws
## This step must be run on VM because of memory constraints
## Requires ~ 30 GB RAM
## NOTE that this step requires you to change the file path according to what
## machine you're working on
## This is because on a local machine, the files must be saved to an external drive because
## of storage constraints, but on the VM, the files cannot be on an external drive and must
## be saved somewhere accessible.
## On local machine, I saved the files at /Volumes/FileBackup/
## On VM, I saved/loaded the files at out/posteriors/
## You must change the file path according to what type of machine you're working on

## Input: out/posteriors/sand_aat_tpr_prsd/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData
## Output from GJAM with sand, aat, tpr, prsd covariates with posterior draws

## Input: data/processed/mean_stepps_soil_clim.RData
## OOS data

## Output: out/posteriors/oos_prediction_time.RData
## Out of sample predictions for non-conditional, conditional, and conditional only on
## oak predictions
## Out of sample predictions are made for the 300 YBP time period across the same
## spatial locations as used to fit the model

rm(list = ls())

# Load fitted models
load('out/posteriors/sand_aat_tpr_prsd/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData')

# Load out-of-sample data
load('data/processed/mean_stepps_soil_clim.RData')

# Format ydata
new_ydata <- taxon_oos_all |>
  dplyr::select(beech:tamarack) |>
  dplyr::rename(oc = other_conifer,
                oh = other_hardwood)

# Format xdata (keep all columns to match format from step 4-5)
xdata <- dplyr::select(taxon_oos_all, clay:prsd)

#### Non conditional ####

# New data list
new_datalist <- list(xdata = xdata,
                     nsim = 10000)

# Storage
pred <- list()

# Loop over posterior draws
for(i in 1:100){
  out <- output[[i]]
  pred[[i]] <- gjam::gjamPredict(output = out,
                                 newdata = new_datalist)
  print(i)
}

# Save
save(pred,
     file = 'out/posteriors/oos_prediction_nonconditional_time.RData')

#### Conditional on oak ####

rm(pred)

ydata_cond <- new_ydata |>
  dplyr::select(oak) |>
  dplyr::rename(OAK = oak)

new_datalist <- list(ydataCond = ydata_cond,
                     xdata = xdata,
                     nsim = 10000)

oak_cond_pred <- list()

# Loop over posterior draws
for(i in 1:100){
  out <- output[[i]]
  oak_cond_pred[[i]] <- gjam::gjamPredict(output = out,
                                          newdata = new_datalist)
  print(i)
}

# Save
save(oak_cond_pred,
     file = 'out/posteriors/oos_prediction_conditionoak_time.RData')
