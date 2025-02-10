### STEP 7-1

## This is identical to step 4-3 but only for the first formula
## and including lagged climate variables

## Lagged climate was created by using the climate at the previous,
## subsampled time step, i.e., 100 years before the current time step

## Looping through each posterior draw
## 1. Formatting xdata
## 2. Formatting ydata
## 3. Running GJAM
## 4. Saving output

## Input: data/processed/post_stepps_soil_clim.RData
## Dataframe with co-located reconstructions of 12 taxa's relative abundances,
## Soil variables, and climate variables

## Output: /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_sand_aat_tpr_prsd_lagged.RData
## The model is run with aat, tpr, prsd and lagged climate variables
## Used in 7.2.oos_prediction_draws.R

rm(list = ls())

# Load data
load('data/processed/post_stepps_soil_clim.RData')

# Remove NAs
post_insample_all <- tidyr::drop_na(post_insample_all)
post_oos_all <- tidyr::drop_na(post_oos_all)

# Combine
data1 <- rbind(post_insample_all, post_oos_all)

# Remove
rm(post_insample_all, post_oos_all)

# Load data
load('data/processed/post_stepps_full_oos.RData')

# Remove NAs
post_oos_all <- tidyr::drop_na(post_oos_all)

# Combine
all_data <- rbind(data1, post_oos_all)

# Create lagged climate variables
all_data <- all_data |>
  dplyr::group_by(draw, x, y) |>
  dplyr::arrange(desc(time)) |>
  dplyr::mutate(aatLag = dplyr::lag(aat),
                tprLag = dplyr::lag(tpr),
                prsdLag = dplyr::lag(prsd))

# Load original data again
load('data/processed/post_stepps_soil_clim.RData')

# Add lagged climate variables to in sample data
post_insample_all <- post_insample_all |>
  dplyr::left_join(y = dplyr::select(all_data,
                                     x, y, draw, time,
                                     aatLag, tprLag, prsdLag),
                   by = c('x', 'y', 'draw', 'time'))

# Define variables for GJAM
niter <- 10000
nburn <- 2000
typeNames <- 'FC'

### Main loop ###

# Number of draws
ndraw <- length(unique(post_insample_all$draw))

# Storage for output
output <- list()

for(i in 1:ndraw){
  # Subset for one posterior draw
  draw <- dplyr::filter(post_insample_all, draw == i)
  
  ### Format xdata ###
  
  xdata <- draw |>
    dplyr::ungroup() |>
    dplyr::filter(time != 19) |>
    tidyr::drop_na() |>
    dplyr::select(clay:prsdLag)
  
  ### Format ydata ###
  
  ydata <- draw |>
    dplyr::ungroup() |>
    dplyr::filter(time != 19) |>
    tidyr::drop_na() |>
    # Remove ASH (results in design matrix not being inverible
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
save(output, file = '/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/GJAM_STEPPS_post_sand_aat_tpr_prsd_lagged.RData')

