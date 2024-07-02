### STEP 3-3

## Running GJAM for mean STEPPS data

## First perform variable selection:
## choosing to keep silt OR sand, average annual temperature,
## Total annual preciptiation OR temperature seasonality, 
## precipitation seasonality
## Based on correlations and VIFs

## Then formatting ydata for GJAM
## removing the ash taxon because it never is very abundant
## and GJAM has an error related to strong multicollinearity when
## ash is included, but not when it is removed

## Then running GJAM

## Input: data/processed/mean_stepps_soil_clim.RData
## Dataframe with co-located reconstructions of 12 taxa's relative abundances,
## Soil variables, and climate variables

## Output: out/mean/mean_sand_aat_tpr_prsd.RData
## Output: out/mean/mean_silt_aat_tpr_prsd.RData
## Output: out/mean/mean_sand_aat_tsd_prsd.RData
## Output: out/mean/mean_silt_aat_tsd_prsd.RData
## "out" object from fitting GJAM with different combinations of non-correlated covariates
## Used in 3.4.process_out_gjam_mean.R

rm(list = ls())

#### Variable selection ####

# Load data
load('data/processed/mean_stepps_soil_clim.RData')

# Format xdata
xdata <- dplyr::select(taxon_insample_all, clay:prsd)

# Check correlations between covariates
xdata_cor <- cor(xdata)
corrplot::corrplot(xdata_cor, type = 'upper', diag = FALSE)

## Check VIFs ##
# Pivot longer to make abundance one response
vif_df <- taxon_insample_all |>
  tidyr::pivot_longer(cols = ash:tamarack,
                      names_to = 'taxon',
                      values_to = 'abund')

# Simple mixed effects model with taxon random effect
lm <- lme4::lmer(abund ~ clay + sand + silt + caco3 + awc + aat + tpr + tsd + prsd + (1|taxon),
         data = vif_df)
# Summary showing correlations
summary(lm)
# VIFs
vifs <- car::vif(lm)
vifs

# Check VIFs with fewer variables
lm <- lme4::lmer(abund ~ silt + aat + tpr + tsd + prsd + (1|taxon),
                 data = vif_df)
summary(lm)
vifs <- car::vif(lm)
vifs

## Check with removing 1/4 climate variables
# Removing average annual temperature
lm <- lme4::lmer(abund ~ silt + tpr + tsd + prsd + (1|taxon),
                 data = vif_df)
summary(lm)
vifs <- car::vif(lm)
vifs # does not work

# Removing total precipitation
lm <- lme4::lmer(abund ~ silt + aat + tsd + prsd + (1|taxon),
                 data = vif_df)
summary(lm)
vifs <- car::vif(lm)
vifs # does work

# Removing temperature seasonality
lm <- lme4::lmer(abund ~ silt + aat + tpr + prsd + (1|taxon),
                 data = vif_df)
summary(lm)
vifs <- car::vif(lm)
vifs # does work

# Removing precipitation seasonality
lm <- lme4::lmer(abund ~ silt + aat + tpr + tsd + (1|taxon),
                 data = vif_df)
summary(lm)
vifs <- car::vif(lm)
vifs # does not work

# Best answer is removing temperature seasonality

#### Format ydata ####

# Set seed
set.seed(1996)

# Format ydata
ydata <- taxon_insample_all |>
  dplyr::select(beech:tamarack) |>
  dplyr::rename(oc = other_conifer,
                oh = other_hardwood)# |>
  # This is a different way of handling the inability
  # to invert the design matrix
  # However, it requires some finessing which
  # makes this solution intractable for the posterior samples
  #dplyr::mutate(ash = ash + runif(n = 1, min = -0.01, max = 0.01),
  #              ash = dplyr::if_else(ash <= 0, 1e-4, ash))

#### Run GJAM ####

# Define variables for gjam
niter <- 10000
nburn <- 2000
typeNames <- 'FC'

# model list
ml <- list(ng = niter, burnin = nburn, typeNames = typeNames)

### silt + aat + tpr + prsd ###

# run model
out <- gjam::gjam(formula = ~ silt + aat + tpr + prsd,
                  xdata = xdata, ydata = ydata,
                  modelList = ml)

# Simple plots
gjam::gjamPlot(out)

# Save output
save(out, file = 'out/mean/mean_silt_aat_tpr_prsd.RData')

### sand + aat + tpr + prsd ###

# run model
out <- gjam::gjam(formula = ~ sand + aat + tpr + prsd,
                  xdata = xdata, ydata = ydata,
                  modelList = ml)

# Simple plots
gjam::gjamPlot(out)

# Save output
save(out, file = 'out/mean/mean_sand_aat_tpr_prsd.RData')

### silt + aat + tsd + prsd ###

# run model
out <- gjam::gjam(formula = ~ silt + aat + tsd + prsd,
                  xdata = xdata, ydata = ydata,
                  modelList = ml)

# Simple plots
gjam::gjamPlot(out)

# Save output
save(out, file = 'out/mean/mean_silt_aat_tsd_prsd.RData')

### sand + aat + tsd + prsd ###

# run model
out <- gjam::gjam(formula = ~ sand + aat + tsd + prsd,
                  xdata = xdata, ydata = ydata,
                  modelList = ml)

# Simple plots
gjam::gjamPlot(out)

# Save output
save(out, file = 'out/mean/mean_sand_aat_tsd_prsd.RData')
