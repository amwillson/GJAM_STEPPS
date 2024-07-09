### STEP 3-6

## Out-of-sample prediction in time (using last time step)
## Using mean STEPPS relative abundances

rm(list = ls())

# Load fitted model
load('out/mean/mean_sand_aat_tpr_prsd.RData')

# Load out-of-sample data
load('data/processed/mean_stepps_soil_clim.RData')

# Format ydata
new_ydata <- taxon_oos_all |>
  dplyr::select(beech:tamarack) |>
  dplyr::rename(oc = other_conifer,
                oh = other_hardwood)

# Format xdata (keep all columns to match format from step 3-3)
xdata <- dplyr::select(taxon_oos_all, clay:prsd)

#### Non conditional ####

# New data list
new_datalist <- list(xdata = xdata,
                     nsim = 10000)

# Non-conditional prediction
pred <- gjam::gjamPredict(output = out,
                          newdata = new_datalist)

#### Conditional ####

cond_pred <- list()

names <- colnames(new_ydata)

for(s in 1:ncol(new_ydata)){
  # Remove one taxon
  notin <- names[s]
  # Keep all the others
  yesin <- names[-s]
  # Remove from ydata
  ydata_cond <- new_ydata[,yesin]
  # Make new data list for gjamPredict function
  new_datalist <- list(ydataCond = ydata_cond,
                       xdata = xdata,
                       nsim = 10000)
  # make prediction
  cond_pred[[s]] <- gjam::gjamPredict(output = out,
                                      newdata = new_datalist)
  print(s)
}

#### Conditional on oak ####

ydata_cond <- dplyr::select(new_ydata, oak)

new_datalist <- list(ydataCond = ydata_cond,
                     xdata = xdata,
                     nsim = 10000)

oak_cond_pred <- gjam::gjamPredict(output = out,
                                   newdata = new_datalist)

# Save
save(pred, cond_pred, oak_cond_pred,
     file = 'out/mean/oos_prediction_time.RData')
