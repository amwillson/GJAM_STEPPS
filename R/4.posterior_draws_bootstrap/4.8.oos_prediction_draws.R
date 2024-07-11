### STEP 4-8

## Out of sample prediction in time (using last time step)
## Using STEPPS posterior draws
## This step must be run on VM

rm(list = ls())

# Load fitted models
load('/Volumes/FileBackup/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData')

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
}