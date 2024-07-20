### STEP 4-6

## Processing output from GJAM model fit with STEPPS relative abundance posterior draws
## This step must be run on a VM because of memory constraints
## Requires ~ 40 GB RAM
## NOTE that this step requires you to change the file path according to what
## machine you're working on
## This is because on a local machine, the files must be saved to an external drive because
## of storage constraints, but on the VM, the files cannot be on an external drive and must
## be saved somewhere accessible.
## On local machine, I saved the files at /Volumes/FileBackup/GJAM_STEPPS_output/
## On VM, I saved/loaded the files at out/posteriors/
## You must change the file path according to what type of machine you're working on

## Input: out/posteriors/silt_aat_tpr_prsd/GJAM_STEPPS_post_silt_aat_tpr_prsd.RData
## Input: out/posteriors/sand_aat_tpr_prsd/GJAM_STEPPS_post_sand_aat_tpr_prsd.RData
## Input: out/posteriors/silt_aat_tsd_prsd/GJAM_STEPPS_post_silt_aat_tsd_prsd.RData
## Input: out/posteriors/sand_aat_tsd_prsd/GJAM_STEPPS_post_sand_aat_tsd_prsd.RData
## Depending on which model format, you load one of these files, which has the posterior
## estimates for a given configuration of GJAM

## Output: out/posteriors/silt_aat_tpr_prsd/bFacGibbs.RDS
## Output: out/posteriors/silt_aat_tpr_prsd/bgibbs.RDS
## Output: out/posteriors/silt_aat_tpr_prsd/bgibbsUn.RDS
## Output: out/posteriors/silt_aat_tpr_prsd/fSensGibbs.RDS
## Output: out/posteriors/silt_aat_tpr_prsd/sgibbs.RDS
## Output: out/posteriors/sand_aat_tpr_prsd/bFacGibbs.RDS
## Output: out/posteriors/sand_aat_tpr_prsd/bgibbs.RDS
## Output: out/posteriors/sand_aat_tpr_prsd/bgibbsUn.RDS
## Output: out/posteriors/sand_aat_tpr_prsd/fSensGibbs.RDS
## Output: out/posteriors/sand_aat_tpr_prsd/sgibbs.RDS
## Output: out/posteriors/silt_aat_tsd_prsd/bFacGibbs.RDS
## Output: out/posteriors/silt_aat_tsd_prsd/bgibbs.RDS
## Output: out/posteriors/silt_aat_tsd_prsd/bgibbsUn.RDS
## Output: out/posteriors/silt_aat_tsd_prsd/fSensGibbs.RDS
## Output: out/posteriors/silt_aat_tsd_prsd/sgibbs.RDS
## Output: out/posteriors/sand_aat_tsd_prsd/bFacGibbs.RDS
## Output: out/posteriors/sand_aat_tsd_prsd/bgibbs.RDS
## Output: out/posteriors/sand_aat_tsd_prsd/bgibbsUn.RDS
## Output: out/posteriors/sand_aat_tsd_prsd/fSensGibbs.RDS
## Output: out/posteriors/sand_aat_tsd_prsd/sgibbs.RDS
## Each parameter type for each model run is saved separately in data frame format
## to faciliate easy loading/manipulation of the full chains
## Used in 4.7.gjam_draws_figures.R

## Output: out/posteriors/silt_aat_tpr_prsd/parameter_summaries.RData
## Output: out/posteriors/sand_aat_tpr_prsd/parameter_summaries.RData
## Output: out/posteriors/silt_aat_tsd_prsd/parameter_summaries.RData
## Output: out/posteriors/sand_aat_tsd_prsd/parameter_summaries.RData
## Summary statistics over full chains for each model type. All parameter
## types are saved together in different data frames
## Currently not used but could be used for plotting

rm(list = ls())

#### Extracting parameter estimates ####

# Which model format?
# Options:
# silt_aat_tpr_prsd
# sand_aat_tpr_prsd
# silt_aat_tsd_prsd
# sand_aat_tsd_prsd
form <- 'sand_aat_tsd_prsd'

# Load model output
load(paste0('out/posteriors/', form, '/GJAM_STEPPS_post_', form, '.RData'))

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
  print(i)
}

# Save
# Saving as RDS to hopefully make it easier to load these things
saveRDS(object = bFacGibbs,
        file = paste0('out/posteriors/', form, '/bFacGibbs.RDS'))
saveRDS(object = bgibbs,
        file = paste0('out/posteriors/', form, '/bgibbs.RDS'))
saveRDS(object = bgibbsUn,
        file = paste0('out/posteriors/', form, '/bgibbsUn.RDS'))
saveRDS(object = fSensGibbs,
        file = paste0('out/posteriors/', form, '/fSensGibbs.RDS'))
saveRDS(object = sgibbs,
        file = paste0('out/posteriors/', form, '/sgibbs.RDS'))

# Remove extra objects
rm(out, output,
   temp_bFacGibbs, temp_bgibbs, temp_bgibbsUn,
   temp_fSensGibbs, temp_sgibbs)

#### Parameter summaries ####

## Want something smaller we can use for most plots

### bFacGibbs ###

summ_bFacGibbs <- bFacGibbs |>
  tidyr::pivot_longer(cols = colnames(bFacGibbs)[1:44],
                      names_to = 'taxon_var', values_to = 'estimate') |>
  dplyr::mutate(taxon = sub(pattern = '_.*', replacement = '', x = taxon_var),
                var = sub(pattern = '.*_', replacement = '', x = taxon_var)) |>
  dplyr::select(-taxon_var) |>
  dplyr::group_by(taxon, var) |>
  dplyr::summarize(mean = mean(estimate),
                   sd = sd(estimate),
                   CI_2.5 = quantile(estimate, probs = 0.025),
                   CI_25 = quantile(estimate, probs = 0.25),
                   CI_50 = median(estimate),
                   CI_75 = quantile(estimate, probs = 0.75),
                   CI_97.5 = quantile(estimate, probs = 0.975))

### bgibbs ###

summ_bgibbs <- bgibbs |>
  tidyr::pivot_longer(cols = colnames(bgibbs)[1:55],
                      names_to = 'taxon_var', values_to = 'estimate') |>
  dplyr::mutate(taxon = sub(pattern = '_.*', replacement = '', x = taxon_var),
                var = sub(pattern = '.*_', replacement = '', x = taxon_var)) |>
  dplyr::select(-taxon_var) |>
  dplyr::group_by(taxon, var) |>
  dplyr::summarize(mean = mean(estimate),
                   sd = sd(estimate),
                   CI_2.5 = quantile(estimate, probs = 0.025),
                   CI_25 = quantile(estimate, probs = 0.25),
                   CI_50 = median(estimate),
                   CI_75 = quantile(estimate, probs = 0.75),
                   CI_97.5 = quantile(estimate, probs = 0.975))

### bgibbsUn ###

summ_bgibbsUn <- bgibbsUn |>
  tidyr::pivot_longer(cols = colnames(bgibbsUn)[1:55],
                      names_to = 'taxon_var', values_to = 'estimate') |>
  dplyr::mutate(taxon = sub(pattern = '_.*', replacement = '', x = taxon_var),
                var = sub(pattern = '.*_', replacement = '', x = taxon_var)) |>
  dplyr::select(-taxon_var) |>
  dplyr::group_by(taxon, var) |>
  dplyr::summarize(mean = mean(estimate),
                   sd = sd(estimate),
                   CI_2.5 = quantile(estimate, probs = 0.025),
                   CI_25 = quantile(estimate, probs = 0.25),
                   CI_50 = median(estimate),
                   CI_75 = quantile(estimate, probs = 0.75),
                   CI_97.5 = quantile(estimate, probs = 0.975))

### fSensGibbs ###

summ_fSensGibbs <- fSensGibbs |>
  tidyr::pivot_longer(cols = colnames(fSensGibbs)[1:4],
                      names_to = 'var', values_to = 'estimate') |>
  dplyr::group_by(var) |>
  dplyr::summarize(mean = mean(estimate),
                   sd = sd(estimate),
                   CI_2.5 = quantile(estimate, probs = 0.025),
                   CI_25 = quantile(estimate, probs = 0.25),
                   CI_50 = median(estimate),
                   CI_75 = quantile(estimate, probs = 0.75),
                   CI_97.5 = quantile(estimate, probs = 0.975))

 
### sgibbs ###

summ_sgibbs <- sgibbs |>
  tidyr::pivot_longer(cols = colnames(sgibbs)[1:66],
                      names_to = 'taxon_taxon', values_to = 'estimate') |>
  dplyr::mutate(taxon1 = sub(pattern = '_.*', replacement = '', x = taxon_taxon),
                taxon2 = sub(pattern = '.*_', replacement = '', x = taxon_taxon)) |>
  dplyr::select(-taxon_taxon) |>
  dplyr::group_by(taxon1, taxon2) |>
  dplyr::summarize(mean = mean(estimate),
                   sd = sd(estimate),
                   CI_2.5 = quantile(estimate, probs = 0.025),
                   CI_25 = quantile(estimate, probs = 0.25),
                   CI_50 = median(estimate),
                   CI_75 = quantile(estimate, probs = 0.75),
                   CI_97.5 = quantile(estimate, probs = 0.975))

# Save
save(summ_bFacGibbs, summ_bgibbs,
     summ_bgibbsUn, summ_fSensGibbs,
     summ_sgibbs,
     file = paste0('out/posteriors/', form, '/parameter_summaries.RData'))
