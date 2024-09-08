### STEP 3-12

## Out-of-sample prediction in space and time (using all spatiotemporal
## locations not used to fit the model)

## Input: out/mean/mean_sand_aat_tpr_prsd_300YBP.RData
## Output from GJAM fitted with 300 YBP

## Input: data/processed/mean_stepps_full_oos.RData
## Dataframe with climate and soil data for all OOS
## locations in space and time

## Output: out/mean/oos_prediction_all.RData
## GJAM output for non-conditional, full conditional,
## and conditional on oak predictions for all
## withheld spatiotemporal locations
## Used in 3.13.plot_oos_prediction.R

rm(list = ls())

# Load fitted model
load('out/mean/mean_sand_aat_tpr_prsd_300YBP.RData')

# Load out-of-sample data
load('data/processed/mean_stepps_full_oos.RData')

# Format ydata
new_ydata <- taxon_oos_all |>
  tidyr::drop_na() |>
  dplyr::select(beech:tamarack) |>
  dplyr::rename(oc = other_conifer,
                oh = other_hardwood)

# Format xdata (keep all columsn to match format from step 3-8)
xdata <- taxon_oos_all |>
  tidyr::drop_na() |>
  dplyr::select(clay:prsd)

#### Non conditional ####

# New  data list
new_datalist <- list(xdata = xdata,
                     nsim = 10000)

# Non-conditional prediction
pred <- gjam::gjamPredict(output = out,
                          newdata = new_datalist)

#### Conditional ####

cond_pred <- list()

names <- colnames(new_ydata)

for(s in 1:ncol(new_ydata)){
  # remove one taxon
  notin <- names[s]
  # keep all the others
  yesin <- names[-s]
  # remove from ydata
  ydata_cond <- new_ydata[,yesin]
  # make new data list for gjamPredict function
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
     file = 'out/mean/oos_prediction_all.RData')
