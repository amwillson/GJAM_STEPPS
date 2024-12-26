#### STEP 6-1

## Sensitivity of relative abundances to climate variables
## using multiple linear regression

## Fitting linear models to relative abundance of each taxon
## individually and environmental
## covariates over time in each grid cell separately using
## each STEPPS posterior draw at each time period

## Regression coefficients from standardized covariates can
## be used as a relative metric of sensitivity of the 
## response variable to each covariate in the linear model

## Fitting the models at each grid cell separately allows
## me to look at the sensitivity of each taxon's relative
## abundance to environmental change over time and across space

## Source of method: https://www.nature.com/articles/s41586-024-08232-z

## Input: data/processed/post_stepps_soil_clim.RData
## Dataframe of posterior draws of STEPPS relative abundance
## and environmental covariates at in-sample grid cells

## Input: data/processed/post_stepps_full_oos.RData
## Dataframe of posterior draws of STEPPS relative abundance
## and environmental covariates at out-of-sample grid cells

## Output: out/sensitivity/taxon_lm_sensitivity.RData
## Dataframe of coefficient estimates for each taxon
## at each grid cell estimating sensitivity of temporal
## changes in relative abundances to temporal changes in
## environmental conditions

rm(list = ls())

#### Load reconstructions ####

# Load in-sample reconstructions from section 4
load('data/processed/post_stepps_soil_clim.RData')

# Combine
data1 <- rbind(post_insample_all, post_oos_all)

# Load out-of-sample reconstructions from section 4
load('data/processed/post_stepps_full_oos.RData')

# Combine all
dat <- rbind(data1, post_oos_all)

#### Format reconstructions ####

# Drop NAs representing empty grid cells
dat_format <- dat |>
  tidyr::drop_na() |>
  # Calculate z-scores to standardize covariates
  dplyr::mutate(aat_stand = (aat - mean(aat)) / sd(aat),
                tpr_stand = (tpr - mean(tpr)) / sd(tpr),
                tsd_stand = (tsd - mean(tsd)) / sd(tsd),
                prsd_stand = (prsd - mean(prsd)) / sd(prsd))

# Unique grid cells
grid_cells <- dat_format |>
  dplyr::select(x, y) |>
  dplyr::distinct()

#### Calculate sensitivity ####

# Initialize matrices
beech_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)
birch_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)
elm_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)
hemlock_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)
maple_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)
oak_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)
oc_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)
oh_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)
pine_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)
spruce_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)
tamarack_sens <- matrix(, nrow = nrow(grid_cells), ncol = 8)

# Loop over grid cells
for(i in 1:nrow(grid_cells)){
  # Coordinates for grid cell of interest
  x_i <- grid_cells$x[i]
  y_i <- grid_cells$y[i]
  
  # Subset data for one grid cell
  dat_sub <- dplyr::filter(dat_format,
                           x == x_i,
                           y == y_i)
  
  beech_mod <- lm(formula = BEECH ~ aat + tpr + prsd,
                  data = dat_sub)
  birch_mod <- lm(formula = BIRCH ~ aat + tpr + prsd,
                  data = dat_sub)
  elm_mod <- lm(formula = ELM ~ aat + tpr + prsd,
                data = dat_sub)
  hemlock_mod <- lm(formula = HEMLOCK ~ aat + tpr + prsd,
                    data = dat_sub)
  maple_mod <- lm(formula = MAPLE ~ aat + tpr + prsd,
                  data = dat_sub)
  oak_mod <- lm(formula = OAK ~ aat + tpr + prsd,
                data = dat_sub)
  oc_mod <- lm(formula = OTHER.CONIFER ~ aat + tpr + prsd,
               data = dat_sub)
  oh_mod <- lm(formula = OTHER.HARDWOOD ~ aat + tpr + prsd,
               data = dat_sub)
  pine_mod <- lm(formula = PINE ~ aat + tpr + prsd,
                 data = dat_sub)
  spruce_mod <- lm(formula = SPRUCE ~ aat + tpr + prsd,
                   data = dat_sub)
  tamarack_mod <- lm(formula = TAMARACK ~ aat + tpr + prsd,
                     data = dat_sub)
  
  ## Extract coefficients and coordinates for each taxon
  # Beech
  beech_sens[i,1] <- x_i
  beech_sens[i,2] <- y_i
  beech_sens[i,3] <- summary(beech_mod)$coefficients[2,1]
  beech_sens[i,4] <- summary(beech_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  beech_sens[i,5] <- summary(beech_mod)$coefficients[3,1]
  beech_sens[i,6] <- summary(beech_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  beech_sens[i,7] <- summary(beech_mod)$coefficients[4,1]
  beech_sens[i,8] <- summary(beech_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Birch
  birch_sens[i,1] <- x_i
  birch_sens[i,2] <- y_i
  birch_sens[i,3] <- summary(birch_mod)$coefficients[2,1]
  birch_sens[i,4] <- summary(birch_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  birch_sens[i,5] <- summary(birch_mod)$coefficients[3,1]
  birch_sens[i,6] <- summary(birch_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  birch_sens[i,7] <- summary(birch_mod)$coefficients[4,1]
  birch_sens[i,8] <- summary(birch_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Elm
  elm_sens[i,1] <- x_i
  elm_sens[i,2] <- y_i
  elm_sens[i,3] <- summary(elm_mod)$coefficients[2,1]
  elm_sens[i,4] <- summary(elm_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  elm_sens[i,5] <- summary(elm_mod)$coefficients[3,1]
  elm_sens[i,6] <- summary(elm_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  elm_sens[i,7] <- summary(elm_mod)$coefficients[4,1]
  elm_sens[i,8] <- summary(elm_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Hemlock
  hemlock_sens[i,1] <- x_i
  hemlock_sens[i,2] <- y_i
  hemlock_sens[i,3] <- summary(hemlock_mod)$coefficients[2,1]
  hemlock_sens[i,4] <- summary(hemlock_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  hemlock_sens[i,5] <- summary(hemlock_mod)$coefficients[3,1]
  hemlock_sens[i,6] <- summary(hemlock_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  hemlock_sens[i,7] <- summary(hemlock_mod)$coefficients[4,1]
  hemlock_sens[i,8] <- summary(hemlock_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Maple
  maple_sens[i,1] <- x_i
  maple_sens[i,2] <- y_i
  maple_sens[i,3] <- summary(maple_mod)$coefficients[2,1]
  maple_sens[i,4] <- summary(maple_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  maple_sens[i,5] <- summary(maple_mod)$coefficients[3,1]
  maple_sens[i,6] <- summary(maple_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  maple_sens[i,7] <- summary(maple_mod)$coefficients[4,1]
  maple_sens[i,8] <- summary(maple_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Oak
  oak_sens[i,1] <- x_i
  oak_sens[i,2] <- y_i
  oak_sens[i,3] <- summary(oak_mod)$coefficients[2,1]
  oak_sens[i,4] <- summary(oak_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  oak_sens[i,5] <- summary(oak_mod)$coefficients[3,1]
  oak_sens[i,6] <- summary(oak_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  oak_sens[i,7] <- summary(oak_mod)$coefficients[4,1]
  oak_sens[i,8] <- summary(oak_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Other conifer
  oc_sens[i,1] <- x_i
  oc_sens[i,2] <- y_i
  oc_sens[i,3] <- summary(oc_mod)$coefficients[2,1]
  oc_sens[i,4] <- summary(oc_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  oc_sens[i,5] <- summary(oc_mod)$coefficients[3,1]
  oc_sens[i,6] <- summary(oc_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  oc_sens[i,7] <- summary(oc_mod)$coefficients[4,1]
  oc_sens[i,8] <- summary(oc_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Other hardwood
  oh_sens[i,1] <- x_i
  oh_sens[i,2] <- y_i
  oh_sens[i,3] <- summary(oh_mod)$coefficients[2,1]
  oh_sens[i,4] <- summary(oh_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  oh_sens[i,5] <- summary(oh_mod)$coefficients[3,1]
  oh_sens[i,6] <- summary(oh_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  oh_sens[i,7] <- summary(oh_mod)$coefficients[4,1]
  oh_sens[i,8] <- summary(oh_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Pine
  pine_sens[i,1] <- x_i
  pine_sens[i,2] <- y_i
  pine_sens[i,3] <- summary(pine_mod)$coefficients[2,1]
  pine_sens[i,4] <- summary(pine_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  pine_sens[i,5] <- summary(pine_mod)$coefficients[3,1]
  pine_sens[i,6] <- summary(pine_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  pine_sens[i,7] <- summary(pine_mod)$coefficients[4,1]
  pine_sens[i,8] <- summary(pine_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Spruce
  spruce_sens[i,1] <- x_i
  spruce_sens[i,2] <- y_i
  spruce_sens[i,3] <- summary(spruce_mod)$coefficients[2,1]
  spruce_sens[i,4] <- summary(spruce_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  spruce_sens[i,5] <- summary(spruce_mod)$coefficients[3,1]
  spruce_sens[i,6] <- summary(spruce_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  spruce_sens[i,7] <- summary(spruce_mod)$coefficients[4,1]
  spruce_sens[i,8] <- summary(spruce_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Tamarack
  tamarack_sens[i,1] <- x_i
  tamarack_sens[i,2] <- y_i
  tamarack_sens[i,3] <- summary(tamarack_mod)$coefficients[2,1]
  tamarack_sens[i,4] <- summary(tamarack_mod)$coefficients[2,2] / sqrt(nrow(dat_sub))
  tamarack_sens[i,5] <- summary(tamarack_mod)$coefficients[3,1]
  tamarack_sens[i,6] <- summary(tamarack_mod)$coefficients[3,2] / sqrt(nrow(dat_sub))
  tamarack_sens[i,7] <- summary(tamarack_mod)$coefficients[4,1]
  tamarack_sens[i,8] <- summary(tamarack_mod)$coefficients[4,2] / sqrt(nrow(dat_sub))
  
  # Progress
  print(i/nrow(grid_cells))
}

#### Save ####

# Save taxon sensitivity matrices
save(beech_sens, birch_sens,
     elm_sens, hemlock_sens,
     maple_sens, oak_sens,
     oc_sens, oh_sens,
     pine_sens, spruce_sens,
     tamarack_sens,
     file = 'out/sensitivity/taxon_lm_sensitivity.RData')
