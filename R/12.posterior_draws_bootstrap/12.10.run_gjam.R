## Running GJAM for STEPPS posterior draws

## Using outcome of variable selection for mean

## Looping through each posterior draw
### 1. Formatting xdata
### 2. Formatting ydata
### 3. Running GJAM
### 4. Saving output

rm(list = ls())

# Load data
load('data/processed/post_stepps_soil_clim.RData')

# Set seed
set.seed(1996)

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
  
  #### Format xdata ####
  
  xdata <- dplyr::select(draw, clay:prsd)
  
  #### Format ydata ####
  
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
    
  #### Run GJAM ####
  
  # model list
  ml <- list(ng = niter, burnin = nburn, typeNames = typeNames)
  
  # run model
  out <- gjam::gjam(formula = ~ sand + aat + tpr + prsd,
                    xdata = xdata, ydata = ydata,
                    modelList = ml)
  
  #### Save ####
  output[[i]] <- out
  
  # Progress
  print(i)
}

# Save
# Should be saved to output directory but it's really big
# so I'm saving to an external drive
save(output, file = '/Volumes/FileBackup/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData')
