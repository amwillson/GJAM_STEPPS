## Processing output from posterior draws model runs

rm(list = ls())

# Load model output
load('/Volumes/FileBackup/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData')

# Number of draws
ndraw <- length(output)

# GJAM parameters
niter <- 10000
burnin <- 2000

# Loop over draws
for(i in 1:ndraw){
  # Get output for one iteration
  out <- output[[i]]
  
  # Save iterations in separate objects
  temp_bFacGibbs <- out$chains$bFacGibbs
  temp_bgibbs <- out$chains$bgibbs
  temp_bgibbsUn <- out$chains$bgibbsUn
  temp_fSensGibbs <- out$chains$fSensGibbs
  temp_sgibbs <- out$chains$sgibbs
  
  # Formatting
  temp_bFacGibbs <- as.data.frame(temp_bFacGibbs)
  temp_bgibbs <- as.data.frame(temp_bgibbs)
  temp_bgibbsUn <- as.data.frame(temp_bgibbsUn)
  temp_fSensGibbs <- as.data.frame(temp_fSensGibbs)
  temp_sgibbs <- as.data.frame(temp_sgibbs)
  
  # Add iteration index to dataframes
  temp_bFacGibbs$iter <- temp_bgibbs$iter <- temp_bgibbsUn$iter <-
    temp_fSensGibbs$iter <- temp_sgibbs$iter <- 
    seq(from = 1, to = niter, by = 1)
  
  # Add posterior draw number to dataframes
  temp_bFacGibbs$draw <- temp_bgibbs$draw <- temp_bgibbsUn$draw <-
    temp_fSensGibbs$draw <- temp_sgibbs$draw <-
    rep(i, times = niter)
  
  # Remove burnin
  temp_bFacGibbs <- dplyr::filter(temp_bFacGibbs, iter > burnin)
  temp_bgibbs <- dplyr::filter(temp_bgibbs, iter > burnin)
  temp_bgibbsUn <- dplyr::filter(temp_bgibbsUn, iter > burnin)
  temp_fSensGibbs <- dplyr::filter(temp_fSensGibbs, iter > burnin)
  temp_sgibbs <- dplyr::filter(temp_sgibbs, iter > burnin)
  
  # If it's the first draw, rename
  if(i == 1){
    bFacGibbs <- temp_bFacGibbs
    bgibbs <- temp_bgibbs
    bgibbsUn <- temp_bgibbsUn
    fSensGibbs <- temp_fSensGibbs
    sgibbs <- temp_sgibbs
    # or add to the other draws
  }else{
    bFacGibbs <- rbind(bFacGibbs, temp_bFacGibbs)
    bgibbs <- rbind(bgibbs, temp_bgibbs)
    bgibbsUn <- rbind(bgibbsUn, temp_bgibbsUn)
    fSensGibbs <- rbind(fSensGibbs, temp_fSensGibbs)
    sgibbs <- rbind(sgibbs, temp_sgibbs)
  }
}
