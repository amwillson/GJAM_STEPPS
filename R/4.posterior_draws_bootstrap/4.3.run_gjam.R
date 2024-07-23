### STEP 4-3

## Running GJAM for STEPPS posterior draws

## Using outcome of variable selection from mean relative abundances from STEPPS

## Looping through each posterior draw
## 1. Formatting xdata
## 2. Formatting ydata
## 3. Running GJAM
## 4. Saving output

## Input: data/processed/post_stepps_soil_clim.RData
## Dataframe with co-located reconstructions of 12 taxa's relative abundances,
## Soil variables, and climate variables

## Output: /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData
## Output: /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_silt_aat_tpr_prsd.RData
## Output: /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_sand_aat_tsd_prsd.RData
## Output: /Volumes/FileBackup/GJAM_STEPPS_output/GJAM_STEPPS_post_silt_aat_tsd_prsd.RData
## The model is run with 4 different sets of uncorrelated covariates
## Each configuration is saved separately
## Used in 4.4.process_out_gjam_draws.R and 4.6.oos_prediction_draws.R

rm(list = ls())

# Load data
load('data/processed/post_stepps_soil_clim.RData')

# Define variables for GJAM
niter <- 10000
nburn <- 2000
typeNames <- 'FC'

#### sand + aat + tpr + prsd ####

### Main loop ###

# Number of draws
ndraw <- length(unique(post_insample_all$draw))

# Storage for output
output <- list()

for(i in 1:ndraw){
  # Subset for one posterior draw
  draw <- dplyr::filter(post_insample_all, draw == i)
  
  ### Format xdata ###
  
  xdata <- dplyr::select(draw, clay:prsd)
  
  ### Format ydata ###
  
  ydata <- draw |>
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
  out <- gjam::gjam(formula = ~ sand + aat + tpr + prsd,
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
save(output, file = '/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData')

#### silt + aat + tpr + prsd ####

### Main loop ###

# Number of draws
ndraw <- length(unique(post_insample_all$draw))

# Storage for output
output <- list()

for(i in 1:ndraw){
  # Subset for one posterior draw
  draw <- dplyr::filter(post_insample_all, draw == i)
  
  ### Format xdata ###
  
  xdata <- dplyr::select(draw, clay:prsd)
  
  ### Format ydata ###
  
  ydata <- draw |>
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
  out <- gjam::gjam(formula = ~ silt + aat + tpr + prsd,
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
save(output, file = '/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/GJAM_STEPPS_post_silt_aat_tpr_prsd.RData')

#### sand + aat + tsd + prsd ####

### Main loop ###

# Number of draws
ndraw <- length(unique(post_insample_all$draw))

# Storage for output
output <- list()

for(i in 1:ndraw){
  # Subset for one posterior draw
  draw <- dplyr::filter(post_insample_all, draw == i)
  
  ### Format xdata ###
  
  xdata <- dplyr::select(draw, clay:prsd)
  
  ### Format ydata ###
  
  ydata <- draw |>
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
  out <- gjam::gjam(formula = ~ sand + aat + tsd + prsd,
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
save(output, file = '/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/GJAM_STEPPS_post_sand_aat_tsd_prsd.RData')

#### silt + aat + tsd + prsd ####

### Main loop ###

# Number of draws
ndraw <- length(unique(post_insample_all$draw))

# Storage for output
output <- list()

for(i in 1:ndraw){
  # Subset for one posterior draw
  draw <- dplyr::filter(post_insample_all, draw == i)
  
  ### Format xdata ###
  
  xdata <- dplyr::select(draw, clay:prsd)
  
  ### Format ydata ###
  
  ydata <- draw |>
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
  out <- gjam::gjam(formula = ~ silt + aat + tsd + prsd,
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
save(output, file = '/Volumes/FileBackup/GJAM_STEPPS_output/posteriors/GJAM_STEPPS_post_silt_aat_tsd_prsd.RData')
