## Processing output from mean model run

rm(list = ls())

# Load model output
load('out/mean_silt_aat_tpr_prsd.RData')

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
     file = 'out/processed_out_mean_silt_aat_tpr_prsd.RData')
