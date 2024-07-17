### STEP 4-13

## Out of sample prediction in space and time (using all spatiotemporal
## locations not used to fit the model)
## This step must be run on VM because of memory constraints
## Requires ~40 GB RAM
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

## Input: data/processed/mean_stepps_full_oos.RData
## OOS data for everything not used to fit the model

## Output: out/posteriors/oos_prediction_all.RData
## Out of sample predictions for non-conditional, conditional, and conditional only on
## oak relative abundances
## Out of sample predictions are made for all spatiotemporal locations not used
## to fit the model

rm(list = ls())

# Load fitted model
load('out/posteriors/sand_aat_tpr_prsd_300YBP/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP.RData')

# Load out-of-sample data
load('data/processed/mean_stepps_full_oos.RData')

# Format ydata
new_ydata <- taxon_oos_all |>
  dplyr::select(beech:tamarack) |>
  dplyr::rename(oc = other_conifer,
                oh = other_hardwood)

# Format xdata (keep all columns to match format from step 4-5)
xdata <- dplyr::select(taxon_oos_all, clay:prsd)

#### Non-conditional ####

# New data list
# Number of simulations is dramatically reduced due to extreme
# computation time. 500 is still a large posterior ensemble,
# especially since we are only using the predictive mean (no
# need to characterize the full distribution)
new_datalist <- list(xdata = xdata,
                     nsim = 500)

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
     file = 'out/posteriors/oos_prediction_nonconditional_final.RData')