### STEP 3-4

## Processing output from GJAM model fit with STEPPS mean relative abundances

## Input: out/mean/mean_silt_aat_tpr_prsd.RData
## Input: out/mean/mean_sand_aat_tpr_prsd.RData
## Input: out/mean/mean_silt_aat_tsd_prsd.RData
## Input: out/mean/mean_sand_aat_tsd_prsd.RData
## One of the four outputs from GJAM should be read in and processed for plotting

## Output: out/mean/processed_silt_aat_tpr_prsd.RData
## Output: out/mean/processed_sand_aat_tpr_prsd.RData
## Output: out/mean/processed_silt_aat_tsd_prsd.RData
## Output: out/mean/processed_sand_aat_tsd_prsd.RData
## Depending on the input, one of these four outputs is made
## Used in 3.5.plot_output_mean.R

rm(list = ls())

# Which model format?
# Options:
# silt_aat_tpr_prsd
# sand_aat_tpr_prsd
# silt_aat_tsd_prsd
# sand_aat_tsd_prsd
form <- 'sand_aat_tsd_prsd'

# Load model output
load(paste0('out/mean/mean_', form, '.RData'))

# parameters
niter <- 10000
burnin <- 2000

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
     file = paste0('out/mean/processed_', form, '.RData'))
