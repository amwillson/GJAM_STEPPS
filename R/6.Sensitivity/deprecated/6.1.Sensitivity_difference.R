## Sensitivity of taxon relative abundances to
## climate over space

## Source: https://www.nature.com/articles/s41467-022-31667-9

rm(list = ls())

#### 1. Load reconstructions ####

# Load in-sample reconstructions from section 4
load('data/processed/post_stepps_soil_clim.RData')

# Combine
data1 <- rbind(post_insample_all, post_oos_all)

# Load out-of-sample reconstructions from section 4
load('data/processed/post_stepps_full_oos.RData')

# Combine all
dat <- rbind(data1, post_oos_all)

#### 2. Format reconstructions ####

# Drop NAs representing empty grid cells
dat_format <- dat |>
  tidyr::drop_na()

# Separate xdata
xdata <- dat_format |>
  dplyr::select(time, x, y, aat, tpr, tsd, prsd) |>
  # Take distinct rows since climate
  # estimates are the same across iterations
  dplyr::distinct() |>
  # Calculate z-scores to standardize variables
  dplyr::mutate(aat_stand = (aat - mean(aat)) / sd(aat),
                tpr_stand = (tpr - mean(tpr)) / sd(tpr),
                tsd_stand = (tsd - mean(tsd)) / sd(tsd),
                prsd_stand = (prsd - mean(prsd)) / sd(prsd))

# Separate ydata
ydata <- dat_format |>
  dplyr::select(time:y)

## Add difference between time periods to x and ydata
xdata_diff <- xdata |>
  # Group by location
  dplyr::group_by(x, y) |>
  # Sort by time going from most distant to most recent
  dplyr::arrange(desc(time)) |>
  # Find difference between adjacent time periods
  dplyr::mutate(diff_aat = aat_stand - dplyr::lag(aat_stand),
                diff_tpr = tpr_stand - dplyr::lag(tpr_stand),
                diff_tsd = tsd_stand - dplyr::lag(tsd_stand),
                diff_prsd = prsd_stand - dplyr::lag(prsd_stand))

# Do the same for ydata, also arranging  by draw
ydata_diff <- ydata |>
  # Group by location AND posterior draw
  dplyr::group_by(x, y, draw) |>
  # Sort by time going from most distant to most recent
  dplyr::arrange(desc(time)) |>
  # Find difference between adjacent time periods
  dplyr::mutate(diff_beech = BEECH - dplyr::lag(BEECH),
                diff_birch = BIRCH - dplyr::lag(BIRCH),
                diff_elm = ELM - dplyr::lag(ELM),
                diff_hemlock = HEMLOCK - dplyr::lag(HEMLOCK),
                diff_maple = MAPLE - dplyr::lag(MAPLE),
                diff_oak = OAK - dplyr::lag(OAK),
                diff_oc = OTHER.CONIFER - dplyr::lag(OTHER.CONIFER),
                diff_oh = OTHER.HARDWOOD - dplyr::lag(OTHER.HARDWOOD),
                diff_pine = PINE - dplyr::lag(PINE),
                diff_spruce = SPRUCE - dplyr::lag(SPRUCE),
                diff_tamarack = TAMARACK - dplyr::lag(TAMARACK))


####  3. Calculate sensitivity ####

# Initialize dataframes for sensitivity of each taxon
beech_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
birch_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
elm_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
hemlock_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
maple_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oak_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oc_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oh_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
pine_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
spruce_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
tamarack_aat <- matrix(, nrow = nrow(xdata_diff), ncol = 103)

beech_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
birch_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
elm_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
hemlock_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
maple_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oak_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oc_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oh_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
pine_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
spruce_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
tamarack_tpr <- matrix(, nrow = nrow(xdata_diff), ncol = 103)

beech_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
birch_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
elm_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
hemlock_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
maple_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oak_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oc_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oh_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
pine_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
spruce_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
tamarack_tsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)

beech_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
birch_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
elm_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
hemlock_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
maple_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oak_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oc_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
oh_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
pine_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
spruce_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)
tamarack_prsd <- matrix(, nrow = nrow(xdata_diff), ncol = 103)

# Add coordinates and time
beech_aat[,101] <- birch_aat[,101] <- elm_aat[,101] <-
  hemlock_aat[,101] <- maple_aat[,101] <- oak_aat[,101] <-
  oc_aat[,101] <- oh_aat[,101] <- pine_aat[,101] <-
  spruce_aat[,101] <- tamarack_aat[,101] <- beech_tpr[,101] <-
  birch_tpr[,101] <- elm_tpr[,101] <- hemlock_tpr[,101] <-
  maple_tpr[,101] <- oak_tpr[,101] <- oc_tpr[,101] <-
  oh_tpr[,101] <- pine_tpr[,101] <- spruce_tpr[,101] <-
  tamarack_tpr[,101] <- beech_tsd[,101] <- birch_tsd[,101] <-
  elm_tsd[,101] <- hemlock_tsd[,101] <- maple_tsd[,101] <-
  oak_tsd[,101] <- oc_tsd[,101] <- oh_tsd[,101] <- 
  pine_tsd[,101] <- spruce_tsd[,101] <- tamarack_tsd[,101] <-
  beech_prsd[,101] <- birch_prsd[,101] <- elm_prsd[,101] <-
  hemlock_prsd[,101] <- maple_prsd[,101] <- oak_prsd[,101] <-
  oc_prsd[,101] <- oh_prsd[,101] <- pine_prsd[,101] <-
  spruce_prsd[,101] <- tamarack_prsd[,101] <- 
  xdata_diff$time

beech_aat[,102] <- birch_aat[,102] <- elm_aat[,102] <-
  hemlock_aat[,102] <- maple_aat[,102] <- oak_aat[,102] <-
  oc_aat[,102] <- oh_aat[,102] <- pine_aat[,102] <-
  spruce_aat[,102] <- tamarack_aat[,102] <- beech_tpr[,102] <-
  birch_tpr[,102] <- elm_tpr[,102] <- hemlock_tpr[,102] <-
  maple_tpr[,102] <- oak_tpr[,102] <- oc_tpr[,102] <-
  oh_tpr[,102] <- pine_tpr[,102] <- spruce_tpr[,102] <-
  tamarack_tpr[,102] <- beech_tsd[,102] <- birch_tsd[,102] <-
  elm_tsd[,102] <- hemlock_tsd[,102] <- maple_tsd[,102] <-
  oak_tsd[,102] <- oc_tsd[,102] <- oh_tsd[,102] <- 
  pine_tsd[,102] <- spruce_tsd[,102] <- tamarack_tsd[,102] <-
  beech_prsd[,102] <- birch_prsd[,102] <- elm_prsd[,102] <-
  hemlock_prsd[,102] <- maple_prsd[,102] <- oak_prsd[,102] <-
  oc_prsd[,102] <- oh_prsd[,102] <- pine_prsd[,102] <-
  spruce_prsd[,102] <- tamarack_prsd[,102] <- 
  xdata_diff$x

beech_aat[,103] <- birch_aat[,103] <- elm_aat[,103] <-
  hemlock_aat[,103] <- maple_aat[,103] <- oak_aat[,103] <-
  oc_aat[,103] <- oh_aat[,103] <- pine_aat[,103] <-
  spruce_aat[,103] <- tamarack_aat[,103] <- beech_tpr[,103] <-
  birch_tpr[,103] <- elm_tpr[,103] <- hemlock_tpr[,103] <-
  maple_tpr[,103] <- oak_tpr[,103] <- oc_tpr[,103] <-
  oh_tpr[,103] <- pine_tpr[,103] <- spruce_tpr[,103] <-
  tamarack_tpr[,103] <- beech_tsd[,103] <- birch_tsd[,103] <-
  elm_tsd[,103] <- hemlock_tsd[,103] <- maple_tsd[,103] <-
  oak_tsd[,103] <- oc_tsd[,103] <- oh_tsd[,103] <- 
  pine_tsd[,103] <- spruce_tsd[,103] <- tamarack_tsd[,103] <-
  beech_prsd[,103] <- birch_prsd[,103] <- elm_prsd[,103] <-
  hemlock_prsd[,103] <- maple_prsd[,103] <- oak_prsd[,103] <-
  oc_prsd[,103] <- oh_prsd[,103] <- pine_prsd[,103] <-
  spruce_prsd[,103] <- tamarack_prsd[,103] <- 
  xdata_diff$y

# Loop over posterior draws of STEPPS
for(i in 1:length(unique(ydata_diff$draw))){
  # Filter for one posterior draw in ydata
  ydata_sub <- ydata_diff |>
    dplyr::filter(draw == i) |>
    dplyr::ungroup()

  # Check that xdata and ydata are in the same order
  # If they're not, break
  if(!(all(beech_aat[,101:103] == dplyr::select(ydata_sub, time, x, y)))) break

  # Calculate sensitivity of each taxon to change in average annual temperature
  beech_aat[,i] <- ydata_sub$diff_beech / xdata_diff$diff_aat
  birch_aat[,i] <- ydata_sub$diff_birch / xdata_diff$diff_aat
  elm_aat[,i] <- ydata_sub$diff_elm / xdata_diff$diff_aat
  hemlock_aat[,i] <- ydata_sub$diff_hemlock / xdata_diff$diff_aat
  maple_aat[,i] <- ydata_sub$diff_maple / xdata_diff$diff_aat
  oak_aat[,i] <- ydata_sub$diff_oak / xdata_diff$diff_aat
  oc_aat[,i] <- ydata_sub$diff_oc / xdata_diff$diff_aat
  oh_aat[,i] <- ydata_sub$diff_oh / xdata_diff$diff_aat
  pine_aat[,i] <- ydata_sub$diff_pine / xdata_diff$diff_aat
  spruce_aat[,i] <- ydata_sub$diff_spruce / xdata_diff$diff_aat
  tamarack_aat[,i] <- ydata_sub$diff_tamarack / xdata_diff$diff_aat
  
  # Total annual precipitation
  beech_tpr[,i] <- ydata_sub$diff_beech / xdata_diff$diff_tpr
  birch_tpr[,i] <- ydata_sub$diff_birch / xdata_diff$diff_tpr
  elm_tpr[,i] <- ydata_sub$diff_elm / xdata_diff$diff_tpr
  hemlock_tpr[,i] <- ydata_sub$diff_hemlock / xdata_diff$diff_tpr
  maple_tpr[,i] <- ydata_sub$diff_maple / xdata_diff$diff_tpr
  oak_tpr[,i] <- ydata_sub$diff_oak / xdata_diff$diff_tpr
  oc_tpr[,i] <- ydata_sub$diff_oc / xdata_diff$diff_tpr
  oh_tpr[,i] <- ydata_sub$diff_oh / xdata_diff$diff_tpr
  pine_tpr[,i] <- ydata_sub$diff_pine / xdata_diff$diff_tpr
  spruce_tpr[,i] <- ydata_sub$diff_spruce / xdata_diff$diff_tpr
  tamarack_tpr[,i] <- ydata_sub$diff_tamarack / xdata_diff$diff_tpr
  
  # Temperature seasonality
  beech_tsd[,i] <- ydata_sub$diff_beech / xdata_diff$diff_tsd
  birch_tsd[,i] <- ydata_sub$diff_birch / xdata_diff$diff_tsd
  elm_tsd[,i] <- ydata_sub$diff_elm / xdata_diff$diff_tsd
  hemlock_tsd[,i] <- ydata_sub$diff_hemlock / xdata_diff$diff_tsd
  maple_tsd[,i] <- ydata_sub$diff_maple / xdata_diff$diff_tsd
  oak_tsd[,i] <- ydata_sub$diff_oak / xdata_diff$diff_tsd
  oc_tsd[,i] <- ydata_sub$diff_oc / xdata_diff$diff_tsd
  oh_tsd[,i] <- ydata_sub$diff_oh / xdata_diff$diff_tsd
  pine_tsd[,i] <- ydata_sub$diff_pine / xdata_diff$diff_tsd
  spruce_tsd[,i] <- ydata_sub$diff_spruce / xdata_diff$diff_tsd
  tamarack_tsd[,i] <- ydata_sub$diff_tamarack / xdata_diff$diff_tsd
  
  # Precipitation seasonality
  beech_prsd[,i] <- ydata_sub$diff_beech / xdata_diff$diff_prsd
  birch_prsd[,i] <- ydata_sub$diff_birch / xdata_diff$diff_prsd
  elm_prsd[,i] <- ydata_sub$diff_elm / xdata_diff$diff_prsd
  hemlock_prsd[,i] <- ydata_sub$diff_hemlock / xdata_diff$diff_prsd
  maple_prsd[,i] <- ydata_sub$diff_maple / xdata_diff$diff_prsd
  oak_prsd[,i] <- ydata_sub$diff_oak / xdata_diff$diff_prsd
  oc_prsd[,i] <- ydata_sub$diff_oc / xdata_diff$diff_prsd
  oh_prsd[,i] <- ydata_sub$diff_oh / xdata_diff$diff_prsd
  pine_prsd[,i] <- ydata_sub$diff_pine / xdata_diff$diff_prsd
  spruce_prsd[,i] <- ydata_sub$diff_spruce / xdata_diff$diff_prsd
  tamarack_prsd[,i] <- ydata_sub$diff_tamarack / xdata_diff$diff_prsd
  
  # Progress
  print(i)
}

#### 4. Save ####

# Save all matrices for producing figures
save(beech_aat, birch_aat, elm_aat,
     hemlock_aat, maple_aat, oak_aat,
     oc_aat, oh_aat, pine_aat,
     spruce_aat, tamarack_aat,
     file = 'out/sensitivity/aat_sensitivity.RData')

save(beech_tpr, birch_tpr, elm_tpr,
     hemlock_tpr, maple_tpr, oak_tpr,
     oc_tpr, oh_tpr, pine_tpr,
     spruce_tpr, tamarack_tpr,
     file = 'out/sensitivity/tpr_sensitivity.RData')

save(beech_tsd, birch_tsd, elm_tsd,
     hemlock_tsd, maple_tsd, oak_tsd,
     oc_tsd, oh_tsd, pine_tsd,
     spruce_tsd, tamarack_tsd,
     file = 'out/sensitivity/tsd_sensitivity.RData')

save(beech_prsd, birch_prsd, elm_prsd,
     hemlock_prsd, maple_prsd, oak_prsd,
     oc_prsd, oh_prsd, pine_prsd,
     spruce_prsd, tamarack_prsd,
     file = 'out/sensitivity/prsd_sensitivity.RData')
