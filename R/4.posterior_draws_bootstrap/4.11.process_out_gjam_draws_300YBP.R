### STEP 4-11

## Processing output from GJAM model fit with STEPPs relative abundance poterior draws
## including 300 YBP time step
## Identical to step 4-6 but including 300 YBP
## Also only including the model with sand, aat, tpr, prsd
## covariates because this was determined to be the most ecologically relevant
## model in steps 3-3 to 3-7 and 4-5 to 4-7

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

## Input: out/posteriors/sand_aat_tpr_prsd_300YBP/GJAM_STEPPS_post_sand_aat_tpr_prsd_300YBP.RData
## Posterior estimates for GJAM with posterior draws

## Output: out/posteriors/sand_aat_tpr_prsd_300YBP/bFacGibbs.RDS
## Output: out/posteriors/sand_aat_tpr_prsd_300YBP/bgibbs.RDS
## Output: out/posteriors/sand_aat_tpr_prsd_300YBP/bgibbsUn.RDS
## Output: out/posteriors/sand_aat_tpr_prsd_300YBP/fSensGibbs.RDS
## Output: out/posteriors/sand_aat_tpr_prsd_300YBP/sgibbs.RDS
## Each parameter type for the model run is saved separately in data frame format
## to faciliate easy loading/manipulation of the full chains
## Used in 4.12.gjam_draws_300YBP_figures.R

## Output: out/posteriors/sand_aat_tpr_prsd_300YBP/parameter_summaries.RData
## Summary statistics over full chains. All parameter types are  saved
## together in different data frames
## Currently not used but could be used for plotting

rm(list = ls())

