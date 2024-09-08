### STEP 3-6

## Out-of-sample prediction in time (using last time step)
## Using mean STEPPS relative abundances

## Input: out/mean/mean_sand_aat_tpr_prsd.RData
## Output from GJAM with sand, aat, tpr, prsd covariates with mean relative abundance

## Input: data/processed/mean_stepps_soil_clim.RData
## OOS data

## Output: out/mean/oos_prediction_time.RData
## Out of sample predictions for non-conditional, conditional, and conditional only on
## oak predictions
## Out of sample predictions are made for the 300 YBP time period across the same
## spatial locations as used to fit the model

rm(list = ls())

# Load fitted model
load('out/mean/mean_sand_aat_tpr_prsd.RData')

# Load out-of-sample data
load('data/processed/mean_stepps_soil_clim.RData')

# Format ydata
new_ydata <- taxon_oos_all |>
  tidyr::drop_na() |>
  dplyr::select(beech:tamarack) |>
  dplyr::rename(oc = other_conifer,
                oh = other_hardwood)

# Format xdata (keep all columns to match format from step 3-3)
xdata <- taxon_oos_all |>
  tidyr::drop_na() |>
  dplyr::select(clay:prsd)

#### Non conditional ####

## Predict relative abundance based on the environment
## and parameter estimates

# New data list
new_datalist <- list(xdata = xdata,
                     nsim = 10000)

# Non-conditional prediction
pred <- gjam::gjamPredict(output = out,
                          newdata = new_datalist)

#### Conditional ####

## Predict relative abundance of one taxon at a time
## based on the environment, parameter estimates, and the
## observed relative abundance of all other taxa

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

## Predict relative abundance of all taxa except for oak
## based on the environment, parameter estimates, and the
## observed relative abundance of oak

# Conditional response data is just oak
ydata_cond <- dplyr::select(new_ydata, oak)

# Data list
new_datalist <- list(ydataCond = ydata_cond,
                     xdata = xdata,
                     nsim = 10000)

# Prediction conditional only on oak
oak_cond_pred <- gjam::gjamPredict(output = out,
                                   newdata = new_datalist)

# Save
save(pred, cond_pred, oak_cond_pred,
     file = 'out/mean/oos_prediction_time.RData')
