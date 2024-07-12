### STEP 3-8

## Running GJAM with mean STEPPS data INCLUDING 300 YBP

## First combining original insample and oos datasets because we already performed
## oos validation

## Then formatting ydata for GJAM
## Same as for step 3-3

## Then running GJAM

## Input: data/processed/mean_stepps_soil_clim.RData
## Dataframe with co-located reconstructions of 12 taxa's relative abundances,
## soil varaibles, and climate variables
## Includes both in sample and oos data

## Output: out/mean/mean_sand_aat_tpr_prsd_300YBP.RData
## "out" object from fitting GJAM with 300 YBP data
## Used in 3.9.plot_output_mean_300YBP.R and 3.10.oos_prediction_all.R

rm(list = ls())

#### Combining data ####

# Load data
load('data/processed/mean_stepps_soil_clim.RData')

# Combine
taxon_all <- rbind(taxon_insample_all, taxon_oos_all)

# Format xdata
xdata <- dplyr::select(taxon_all, clay:prcv)

#### Format ydata ####

# Format ydata
ydata <- taxon_all |>
  dplyr::select(beech:tamarack) |>
  dplyr::rename(oc = other_conifer,
                oh = other_hardwood)

#### Run GJAM ####

# Define variables for gjam
niter <- 20000
nburn <- 5000
typeNames <- 'FC'

# model list
ml <- list(ng = niter, burnin = nburn, typeNames = typeNames)

# run model
out <- gjam::gjam(formula = ~ sand + aat + tpr + prsd,
                  xdata = xdata, ydata = ydata,
                  modelList = ml)

# simple plots
gjam::gjamPlot(out)

# Save output
save(out, file = 'out/mean/mean_sand_aat_tpr_prsd_300YBP.RData')
