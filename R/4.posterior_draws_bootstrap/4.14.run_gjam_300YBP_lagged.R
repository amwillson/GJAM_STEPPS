### STEP 4-14

## Running GJAM for STEPPS posterior draws including 300 YBP
## AND using lagged climate variables

## This is used as a test of whether vegetation change can be explained
## by previous (lagged) climate change

## The formula is exactly the same as for step 4-8, but including
## lag-1 climate variables. This is directly comparable to our final
## model including average annual temperature, total annual precipitation
## and precipitation seasonality

## First combining original insample and oos datasets because we already performed
## oos validation

## Then formatting lag-1 climate variables

## Then running GJAM

## Looping through each posterior draw
## 1. Formatting xdata
## 2. Formatting ydata
## 3. Running GJAM
## 4. Saving output

## Input: data/processed/post_stepps_soil_clim.RData
## Dataframe with co-located reconstructions of 12 taxa's relative abundances,
## soil variables, and climate variables

## Output: /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP_lags.RData
## Output from fitting GJAM with 300 YBP data
## Used in 4.15.process_out_gjam_draws_300YBP_lags.R and 4.16.oos_prediction_draws_final_lags.R

rm(list = ls())

#### Combining data ####

# Load data
load('data/processed/post_stepps_soil_clim.RData')

# Combine
post_all <- rbind(post_insample_all, post_oos_all)

#### Format lagged climate variables ####

post_all <- post_all |>
  dplyr::group_by(x, y, draw) |>
  dplyr::arrange(desc(time)) |>
  dplyr::mutate(aatLag = dplyr::lag(aat),
                tprLag = dplyr::lag(tpr),
                prsdLag = dplyr::lag(prsd))

#### Run GJAM ####

# Define variables for GJAM
niter <- 10000
nburn <- 2000
typeNames <- 'FC'

### Main loop ###

# Number of draws
ndraw <- length(unique(post_all$draw))

# Storage for output
output <- list()

for(i in 1:ndraw){
  # Subset for one posterior draw
  draw <- post_all |>
    tidyr::drop_na() |>
    dplyr::filter(draw == i)
  
  ### Format xdata ###
  
  xdata <- draw |>
    dplyr::ungroup() |>
    dplyr::select(clay:prsdLag)
  
  ### Format ydata ###
  
  ydata <- draw |>
    dplyr::ungroup() |>
    # Remove ASH (results in design matrix not being invertible
    # because of strong correlations. I believe it is because
    # ASH is always approximately 0 while other taxa are more
    # variable)
    dplyr::select(BEECH:TAMARACK) |>
    # Rename because if "other" is in the name,
    # GJAM will remove those variables
    dplyr::rename(OC = OTHER.CONIFER,
                  OH = OTHER.HARDWOOD)
  
  ### Run GJAM ###
  
  # model list
  ml <- list(ng = niter, burnin = nburn, typeNames = typeNames)
  
  # run model
  out <- gjam::gjam(formula = ~ sand + aat + tpr + prsd + 
                      aatLag + tprLag + prsdLag,
                    xdata = xdata, ydata = ydata,
                    modelList = ml)
  
  ### Save ###
  output[[i]] <- out
  
  # Progress
  print(i)
}

# Save
# Should be saved to output directory but it's really big
# so I'm saving to an external drive
save(output, file = '/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP_lagged.RData')
