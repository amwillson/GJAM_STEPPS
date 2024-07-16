### STEP 3-9

## Processing output from GJAM model fit with STEPPS mean relative abundances
## including 300 YBP time step
## Identical to step 3-4 but including 300 YBP
## Also only including the model with the sand, aat, tpr, prsd
## covariates because this was determined to be the most ecologically
## relevant model in steps 3-3 to 3-7

## Input: out/mean/mean_sand_aat_tpr_prsd_300YBP.RData
## Output from GJAM including 300 YBP should be read in and processed
## for plotting

## Output: out/mean/processed_sand_aat_tpr_prsd_300YBP.RData
## Output for plotting
## Used in 3.10.plot_output_300YBP.R

rm(list = ls())

# Load model output
load('out/mean/mean_sand_aat_tpr_prsd_300YBP.RData')

# parameters
niter <- 20000
burnin <- 5000

# Save iterations in separate objects
bFacGibbs <- out$chains$bFacGibbs
bgibbs <- out$chains$bgibbs
bgibbsUn <- out$chains$bgibbsUn
fSensGibbs <- out$chains$fSensGibbs
sgibbs <- out$chains$sgibbs

# Formatting
bFacGibbs <- as.data.frame(bFacGibbs)
bgibbs <- as.data.frame(bgibbs)
bgibbsUn <- as.data.frame(bgibbsUn)
fSensGibbs <- as.data.frame(fSensGibbs)
sgibbs <- as.data.frame(sgibbs)

# Add iteration index to dataframes
bFacGibbs$iter <- bgibbs$iter <- bgibbsUn$iter <- fSensGibbs$iter <- sgibbs$iter <- seq(from = 1, to = niter, by = 1)

# Remove burnin
bFacGibbs <- dplyr::filter(bFacGibbs, iter > burnin)
bgibbs <- dplyr::filter(bgibbs, iter > burnin)
bgibbsUn <- dplyr::filter(bgibbsUn, iter > burnin)
fSensGibbs <- dplyr::filter(fSensGibbs, iter > burnin)
sgibbs <- dplyr::filter(sgibbs, iter > burnin)

# Save
save(bFacGibbs, bgibbs, bgibbsUn,
     fSensGibbs, sgibbs,
     file = 'out/mean/processed_sand_aat_tpr_prsd_300YBP.RData')
