## Running GJAM for mean STEPPS data

## First perform variable selection:
## choosing to keep silt, average annual temperature,
## Total annual preciptiation, precipitation seasonality
## Based on correlations and VIFs

## Then formatting ydata for GJAM
## perturb most inconsequential taxon (ash) by randomly adding or
## subtracting small amount to get around sum to 1 constraint

## Then running GJAM

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
  dplyr::select(ash:tamarack) |>
  dplyr::rename(oc = other_conifer,
                oh = other_hardwood) |>
  dplyr::mutate(ash = ash + runif(n = 1, min = -0.01, max = 0.01),
                ash = dplyr::if_else(ash <= 0, 1e-4, ash))

#### Run GJAM ####

# Define variables for gjam
niter <- 10000
nburn <- 2000
typeNames = 'FC'

# model list
ml <- list(ng = niter, burnin = nburn, typeNames = typeNames)

# run model
out <- gjam::gjam(formula = ~ silt + aat + tpr + prsd,
                  xdata = xdata, ydata = ydata,
                  modelList = ml)

# Simple plots
gjam::gjamPlot(out)

# Save output
save(out, file = 'out/mean_silt_aat_tpr_prsd.RData')
