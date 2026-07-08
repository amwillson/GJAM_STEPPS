## STEP 5-6

## Model of oak relative abundance

## Checking whether oak relative abundance follows the same
## autoregressive pattern as hemlock. I chose oak because it
## is the other taxon with strong temporal patterns.

## Input: data/processed/mean_stepps_soil_clim.RData
## Original in-sample and out-of-sample data from running the model in steps 3.3-3.7
## Combined with the full dataset to use all available data for model fitting

## Input: data/processed/mean_stepps_full_oos.RData
## All out-of-sample data used in steps 3.11-3.13

## Output: out/hemlock/out_small_oak_ecotone.RData
## Fitted model with parameter estimates, the data used to fit
## the model and the model code for only a small
## region of high oak abundance near the savanna-forest ecotone

## Output: out/hemlock/out_med_oak_ecotone.RData
## Fitted model with parameter estimates, the data used to fit
## the model and the model code for a larger region
## surrounding the high oak abundance region near the
## savanna-forest ecotone

## Output: out/hemlock/out_large_oak_ecotone.RData
## Fitted model with parameter estimates, the data used to fit
## the model and the model code for an even larger region
## surrounding the high oak abundance region near the
## savanna-forest ecotone. This includes grid cells within 
## minimal spatial autocorrelation

## Output: out/hemlock/out_oak_full_domain.RData
## Fitted model with parameter estimates, the data used to fit
## the model and the model code for the entire Upper Midwest
## region. This is all available grid cells

rm(list = ls())

# Helper funs
source('R/funs.R')

#### Format data ####

# Load original in-sample and oos data
load('data/processed/mean_stepps_soil_clim.RData')

# Combine
taxon_insample <- rbind(taxon_insample_all, taxon_oos_all)

# Load all the rest of the data
load('data/processed/mean_stepps_full_oos.RData')

# Combine
taxon_all <- rbind(taxon_insample, taxon_oos_all)

# Format covariates (xdata)
xdata <- taxon_all |>
  # Remove NAs where grid cells are empty
  tidyr::drop_na() |>
  # Add location index
  dplyr::mutate(loc = paste0(x, '_', y)) |>
  dplyr::select(time, x, y, loc,
                sand, aat, tpr, prsd)

# Format response variable (ydata)
# We're only interested in hemlock now, instead of all taxa
ydata <- taxon_all |>
  # Remove NAs where grid cells are empty
  tidyr::drop_na() |>
  # Add location index
  dplyr::mutate(loc = paste0(x, '_', y)) |>
  # Take hemlock and indexing
  dplyr::select(time, x, y, loc,
                oak)

## Need to change the direction time moves
## Time is currently in YBP so runs backward
## To make sure we run the model using the PREVIOUS
## time step, just index time to go from 1 to 17 instead

# Times in order from most distant to most recent
times <- as.data.frame(unique(xdata$time)[rev(order(unique(xdata$time)))])
colnames(times) <- 'time'
# Add index of 1 to 17 with 1 being the first and 17 being the last time step
times$time_ind <- seq(from = 1, length.out = nrow(times))

# Add new times to xdata
xdata <- xdata |>
  dplyr::left_join(y = times,
                   by = 'time') |>
  dplyr::select(-time) |>
  dplyr::rename(time = time_ind)

# Add new times to ydata
ydata <- ydata |>
  dplyr::left_join(y = times,
                   by = 'time') |>
  dplyr::select(-time) |>
  dplyr::rename(time = time_ind)

#### Define model ####

## Here is our model for testing the relative contribution of the environment and
## previous abundance
## Simple linear regression, as in GJAM, but now including the influence of the
## hemlock abundance at the previous time step on the current time step

regression_model_fixed_alpha <- "
model{
#### Priors
for(s in 1:ns){
x[1,s] ~ dbeta(10, 100) # Prior for relative abundance at the first time step. Prior corresponds to starting abundance between ~0.01-0.2
x_stand[1,s] <- x[1,s] / (1 / sqrt(tau_proc)) # Expressing in units of standard deviation by dividing by SD
}
phi_obs ~ dgamma(1, 1) # Prior on observation precision
tau_proc ~ dgamma(1, 1) # Prior on process precision

#### Fixed Effects
alpha ~ dnorm(mu_alpha, tau_alpha) # Normal prior on alpha parametesr describing relationship between relative abundances through time
beta ~ dmnorm(mu_beta, tau_beta) # Multivariate normal prior on beta parameters describing relationship between relative abundance and environment

#### Data Model
for(t in 1:n){
for(s in 1:ns){
alpha_obs[t,s] <- x[t,s] * phi_obs # Moment matching
beta_obs[t,s] <- (1 - x[t,s]) * phi_obs # Moment matching
OBS[t,s] ~ dbeta(alpha_obs[t,s], beta_obs[t,s]) # Observation is drawn from beta distribution
}
}

#### Process Model
for(t in 2:n){
for(s in 1:ns){
# Mean hemlock abundance = intercept + alpha * previous relative abundance + beta * environmental covariates
logit(mu[t,s]) <- beta[5] + alpha * x_stand[t-1,s] + beta[1] * aat[t,s] + beta[2] * tpr[t,s] + beta[3] * prsd[t,s] + beta[4] * sand[s]
x[t,s] ~ dnorm(mu[t,s], tau_proc) # Latent relative abundance drawn from normal distribution
x_stand[t,s] <- x[t,s] / (1/sqrt(tau_proc)) # Expressing in units of standard deviation as above
}
}
}
"

#### Case 1: Near oak abundance peak at savanna-forest ecotone ####

## High oak relative abundance and worst prediction accuracy at
## x = 157000, y = 1054000
## I got this from looking at the difference between
## predicted and observed oak relative abundance in script 4-13
## This falls in the savanna along the ecotone
## Take 24 grid cells surrounding the peak in prediction error

# 25 grid cells near peak oak prediction error
oak_ecotone_locs <- expand.grid(x = c(109000, 133000, 157000,
                                      181000, 205000),
                                y = c(1006000, 1030000, 1054000,
                                      1078000, 1102000))
# Add location index
oak_ecotone_locs$loc <- paste0(oak_ecotone_locs$x, '_', oak_ecotone_locs$y)

# Subset data for only Hemlock Mountain region
xdata_oak <- dplyr::filter(xdata, loc %in% oak_ecotone_locs$loc)
ydata_oak <- dplyr::filter(ydata, loc %in% oak_ecotone_locs$loc)

# Map of study region
states <- map_states()

# Plot subsetted area
# Make sure region follows anticipated patterns and see spatial domain we have subsampled
ydata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                limits = c(0, 1),
                                name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~time) +
  ggplot2::ggtitle('Oak relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ydata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = oak, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Oak relative abundance') +
  ggplot2::theme_minimal()

# Plot covariates
# Make sure covariates follow anticipated patterns
xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Soil %\nsand') +
  ggplot2::ggtitle('Soil texture') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = '°C') +
  ggplot2::ggtitle('Average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'mm') +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'mm') +
  ggplot2::ggtitle('Precipitation seasonality') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = sand, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Soil % sand') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = aat, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Average annual temperature (°C)') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = tpr, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Total annual precipitation (mm)') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = prsd, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Precipitation seasonality (mm)') +
  ggplot2::theme_minimal()

# Standardize covariates
# Represent in units of standard deviation
# To match how relative abundance is standardized in the model
xdata_oak <- xdata_oak |>
  dplyr::mutate(sand_stand = sand / sd(sand),
                aat_stand = aat / sd(aat),
                tpr_stand = tpr / sd(tpr),
                prsd_stand = prsd / sd(prsd)) |>
  dplyr::select(-sand, -aat, -tpr, -prsd) |>
  dplyr::rename(sand = sand_stand,
                aat = aat_stand,
                tpr = tpr_stand,
                prsd = prsd_stand)

# Pivot data for inputting into model
ydata_wide <- ydata_oak |>
  dplyr::select(-x, -y) |>
  tidyr::pivot_wider(values_from = oak,
                     names_from = loc) |>
  dplyr::arrange(time)

sand_wide <- xdata_oak |>
  dplyr::select(-aat, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = sand,
                     names_from = loc) |>
  dplyr::arrange(time)

aat_wide <- xdata_oak |>
  dplyr::select(-sand, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = aat,
                     names_from = loc) |>
  dplyr::arrange(time)

tpr_wide <- xdata_oak |>
  dplyr::select(-sand, -aat, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = tpr,
                     names_from = loc) |>
  dplyr::arrange(time)

prsd_wide <- xdata_oak |>
  dplyr::select(-sand, -aat, -tpr, -x, -y) |>
  tidyr::pivot_wider(values_from = prsd,
                     names_from = loc) |>
  dplyr::arrange(time)

# Define data list for model
data <- list()
data$OBS <- dplyr::select(ydata_wide, -time) # observation of relative abundance
data$n <- nrow(data$OBS) # number of time steps
data$ns <- ncol(data$OBS) # number of grid cells
data$mu_alpha <- 0 # prior mean for alpha parameter
data$tau_alpha <- 0.1 # prior precision for alpha parameter
data$mu_beta <- rep(0, times = 5) # prior means for beta parameters
data$tau_beta <- diag(x = c(1, 1, 1, 1, 5),
                      nrow = 5,
                      ncol = 5) # prior precisions for beta parameters with slightly higher precision for intercept
data$aat <- dplyr::select(aat_wide, -time) # observation of temperature
data$tpr <- dplyr::select(tpr_wide, -time) # observation of precipitation
data$prsd <- dplyr::select(prsd_wide, -time) # observation of precipitation seasonality
data$sand <- as.vector(as.matrix(sand_wide[1,2:ncol(sand_wide)])) # observation of soil texture, which doesn't change over time

# Create JAGS model
jm <- rjags::jags.model(file = textConnection(regression_model_fixed_alpha),
                        data = data,
                        n.chains = 3,
                        n.adapt = 500000) # number of adaptation iterations

# Burn in iterations
update(jm, n.iter = 100000)

# Samples from model
out <- rjags::coda.samples(model = jm,
                           variable.names = c('alpha',
                                              'beta',
                                              'phi_obs',
                                              'tau_proc'),
                           n.iter = 20000) # number of iterations to keep

# Check for convergence
plot(out)
coda::gelman.diag(out, confidence = 0.99)

# Save
save(out, data, regression_model_fixed_alpha,
     file = 'out/hemlock/out_small_oak_ecotone.RData')

#### Case 2: 49 cell savanna-forest ecotone ####

## High oak relative abundance and worst prediction accuracy at
## x = 157000, y = 1054000
## I got this from looking at the difference between
## predicted and observed oak relative abundance in script 4-13
## This falls in the savanna along the ecotone
## Take 48 grid cells surrounding the peak in prediction error

# 49 grid cells near peak in prediction error
oak_ecotone_locs <- expand.grid(x = c(85000, 109000, 133000, 157000,
                                      181000, 205000, 229000),
                                y = c(982000, 1006000, 1030000, 1054000,
                                      1078000, 1102000, 1126000))
# Add location index
oak_ecotone_locs$loc <- paste0(oak_ecotone_locs$x, '_', oak_ecotone_locs$y)

# Subset data for only region of interest
xdata_oak <- dplyr::filter(xdata, loc %in% oak_ecotone_locs$loc)
ydata_oak <- dplyr::filter(ydata, loc %in% oak_ecotone_locs$loc)

# Map of study region
states <- map_states()

# Plot subsetted area
# Make sure region follows anticipated patterns and see spatial domain we have subsampled
ydata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                limits = c(0, 1),
                                name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~time) +
  ggplot2::ggtitle('Oak relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ydata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = oak, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Oak relative abundance') +
  ggplot2::theme_minimal()

# Plot covariates
# Make sure covariates follow anticipated patterns
xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Soil %\nsand') +
  ggplot2::ggtitle('Soil texture') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = '°C') +
  ggplot2::ggtitle('Average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'mm') +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'mm') +
  ggplot2::ggtitle('Precipitation seasonality') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = sand, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Soil % sand') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = aat, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Average annual temperature (°C)') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = tpr, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Total annual precipitation (mm)') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = prsd, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Precipitation seasonality (mm)') +
  ggplot2::theme_minimal()

# Standardize covariates
# Represent in units of standard deviation
# To match how relative abundance is standardized in the model
xdata_oak <- xdata_oak |>
  dplyr::mutate(sand_stand = sand / sd(sand),
                aat_stand = aat / sd(aat),
                tpr_stand = tpr / sd(tpr),
                prsd_stand = prsd / sd(prsd)) |>
  dplyr::select(-sand, -aat, -tpr, -prsd) |>
  dplyr::rename(sand = sand_stand,
                aat = aat_stand,
                tpr = tpr_stand,
                prsd = prsd_stand)

# Pivot data for inputting into model
ydata_wide <- ydata_oak |>
  dplyr::select(-x, -y) |>
  tidyr::pivot_wider(values_from = oak,
                     names_from = loc) |>
  dplyr::arrange(time)

sand_wide <- xdata_oak |>
  dplyr::select(-aat, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = sand,
                     names_from = loc) |>
  dplyr::arrange(time)

aat_wide <- xdata_oak |>
  dplyr::select(-sand, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = aat,
                     names_from = loc) |>
  dplyr::arrange(time)

tpr_wide <- xdata_oak |>
  dplyr::select(-sand, -aat, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = tpr,
                     names_from = loc) |>
  dplyr::arrange(time)

prsd_wide <- xdata_oak |>
  dplyr::select(-sand, -aat, -tpr, -x, -y) |>
  tidyr::pivot_wider(values_from = prsd,
                     names_from = loc) |>
  dplyr::arrange(time)

# Define data list for model
data <- list()
data$OBS <- dplyr::select(ydata_wide, -time) # observation of relative abundance
data$n <- nrow(data$OBS) # number of time steps
data$ns <- ncol(data$OBS) # number of grid cells
data$mu_alpha <- 0 # prior mean for alpha parameter
data$tau_alpha <- 0.1 # prior precision for alpha parameter
data$mu_beta <- rep(0, times = 5) # prior means for beta parameters
data$tau_beta <- diag(x = c(1, 1, 1, 1, 5),
                      nrow = 5,
                      ncol = 5) # prior precisions for beta parameters with slightly higher precision for intercept
data$aat <- dplyr::select(aat_wide, -time) # observation of temperature
data$tpr <- dplyr::select(tpr_wide, -time) # observation of precipitation
data$prsd <- dplyr::select(prsd_wide, -time) # observation of precipitation seasonality
data$sand <- as.vector(as.matrix(sand_wide[1,2:ncol(sand_wide)])) # observation of soil texture, which doesn't change over time

# Create JAGS model
jm <- rjags::jags.model(file = textConnection(regression_model_fixed_alpha),
                        data = data,
                        n.chains = 3,
                        n.adapt = 500000) # number of adaptation iterations

# Burn in iterations
update(jm, n.iter = 100000)

# Samples from model
out <- rjags::coda.samples(model = jm,
                           variable.names = c('alpha',
                                              'beta',
                                              'phi_obs',
                                              'tau_proc'),
                           n.iter = 50000) # number of iterations to keep

# Check for convergence
plot(out)
coda::gelman.diag(out, confidence = 0.99)

# Save
save(out, data, regression_model_fixed_alpha,
     file = 'out/hemlock/out_med_oak_ecotone.RData')

#### Case 3: 81 cell savanna-forest ecotone ####

## High oak relative abundance and worst prediction accuracy at
## x = 157000, y = 1054000
## I got this from looking at the difference between
## predicted and observed oak relative abundance in script 4-13
## This falls in the savanna along the ecotone
## Take 80 grid cells surrounding the peak in prediction error

# 81 grid cells near Hemlock Mountain
oak_ecotone_locs <- expand.grid(x = c(61000, 85000, 109000, 133000, 157000,
                                      181000, 205000, 229000, 253000),
                                y = c(958000, 982000, 1006000, 1030000, 1054000,
                                      1078000, 1102000, 1126000, 1150000))
# Add location index
oak_ecotone_locs$loc <- paste0(oak_ecotone_locs$x, '_', oak_ecotone_locs$y)

# Subset data for only Hemlock Mountain region
xdata_oak <- dplyr::filter(xdata, loc %in% oak_ecotone_locs$loc)
ydata_oak <- dplyr::filter(ydata, loc %in% oak_ecotone_locs$loc)

# Map of study region
states <- map_states()

# Plot subsetted area
# Make sure region follows anticipated patterns and see spatial domain we have subsampled
ydata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                limits = c(0, 1),
                                name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~time) +
  ggplot2::ggtitle('Oak relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ydata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = oak, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Oak relative abundance') +
  ggplot2::theme_minimal()

# Plot covariates
# Make sure covariates follow anticipated patterns
xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Soil %\nsand') +
  ggplot2::ggtitle('Soil texture') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = '°C') +
  ggplot2::ggtitle('Average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'mm') +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'mm') +
  ggplot2::ggtitle('Precipitation seasonality') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = sand, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Soil % sand') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = aat, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Average annual temperature (°C)') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = tpr, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Total annual precipitation (mm)') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = prsd, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Precipitation seasonality (mm)') +
  ggplot2::theme_minimal()

# Standardize covariates
# Represent in units of standard deviation
# To match how relative abundance is standardized in the model
xdata_oak <- xdata_oak |>
  dplyr::mutate(sand_stand = sand / sd(sand),
                aat_stand = aat / sd(aat),
                tpr_stand = tpr / sd(tpr),
                prsd_stand = prsd / sd(prsd)) |>
  dplyr::select(-sand, -aat, -tpr, -prsd) |>
  dplyr::rename(sand = sand_stand,
                aat = aat_stand,
                tpr = tpr_stand,
                prsd = prsd_stand)

# Pivot data for inputting into model
ydata_wide <- ydata_oak |>
  dplyr::select(-x, -y) |>
  tidyr::pivot_wider(values_from = oak,
                     names_from = loc) |>
  dplyr::arrange(time)

sand_wide <- xdata_oak |>
  dplyr::select(-aat, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = sand,
                     names_from = loc) |>
  dplyr::arrange(time)

aat_wide <- xdata_oak |>
  dplyr::select(-sand, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = aat,
                     names_from = loc) |>
  dplyr::arrange(time)

tpr_wide <- xdata_oak |>
  dplyr::select(-sand, -aat, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = tpr,
                     names_from = loc) |>
  dplyr::arrange(time)

prsd_wide <- xdata_oak |>
  dplyr::select(-sand, -aat, -tpr, -x, -y) |>
  tidyr::pivot_wider(values_from = prsd,
                     names_from = loc) |>
  dplyr::arrange(time)

# Define data list for model
data <- list()
data$OBS <- dplyr::select(ydata_wide, -time) # observation of relative abundance
data$n <- nrow(data$OBS) # number of time steps
data$ns <- ncol(data$OBS) # number of grid cells
data$mu_alpha <- 0 # prior mean for alpha parameter
data$tau_alpha <- 0.1 # prior precision for alpha parameter
data$mu_beta <- rep(0, times = 5) # prior means for beta parameters
data$tau_beta <- diag(x = c(1, 1, 1, 1, 5),
                      nrow = 5,
                      ncol = 5) # prior precisions for beta parameters with slightly higher precision for intercept
data$aat <- dplyr::select(aat_wide, -time) # observation of temperature
data$tpr <- dplyr::select(tpr_wide, -time) # observation of precipitation
data$prsd <- dplyr::select(prsd_wide, -time) # observation of precipitation seasonality
data$sand <- as.vector(as.matrix(sand_wide[1,2:ncol(sand_wide)])) # observation of soil texture, which doesn't change over time

# Create JAGS model
jm <- rjags::jags.model(file = textConnection(regression_model_fixed_alpha),
                        data = data,
                        n.chains = 3,
                        n.adapt = 500000) # number of adaptation iterations

# Burn in iterations
update(jm, n.iter = 100000)

# Samples from model
out <- rjags::coda.samples(model = jm,
                           variable.names = c('alpha',
                                              'beta',
                                              'phi_obs',
                                              'tau_proc'),
                           n.iter = 50000) # number of iterations to keep

# Check for convergence
plot(out)
coda::gelman.diag(out, confidence = 0.99)

# Save
save(out, data, regression_model_fixed_alpha,
     file = 'out/hemlock/out_large_oak_ecotone.RData.RData')

#### Case 4: Entire domain ####

## Use entire study domain
xdata_oak <- xdata
ydata_oak <- ydata

# Map of study region
states <- map_states()

# Plot subsetted area
# Make sure region follows anticipated patterns and see spatial domain we have subsampled
ydata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = oak)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                limits = c(0, 1),
                                name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~time) +
  ggplot2::ggtitle('Oak relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ydata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = oak, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Oak relative abundance') +
  ggplot2::theme_minimal()

# Plot covariates
# Make sure covariates follow anticipated patterns
xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'Soil %\nsand') +
  ggplot2::ggtitle('Soil texture') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = aat)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = '°C') +
  ggplot2::ggtitle('Average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tpr)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'mm') +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = prsd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::facet_wrap(~time) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                direction = 1,
                                na.value = '#00000000',
                                name = 'mm') +
  ggplot2::ggtitle('Precipitation seasonality') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = sand, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Soil % sand') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = aat, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Average annual temperature (°C)') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = tpr, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Total annual precipitation (mm)') +
  ggplot2::theme_minimal()

xdata_oak |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = prsd, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Precipitation seasonality (mm)') +
  ggplot2::theme_minimal()

# Standardize covariates
# Represent in units of standard deviation
# To match how relative abundance is standardized in the model
xdata_oak <- xdata_oak |>
  dplyr::mutate(sand_stand = sand / sd(sand),
                aat_stand = aat / sd(aat),
                tpr_stand = tpr / sd(tpr),
                prsd_stand = prsd / sd(prsd)) |>
  dplyr::select(-sand, -aat, -tpr, -prsd) |>
  dplyr::rename(sand = sand_stand,
                aat = aat_stand,
                tpr = tpr_stand,
                prsd = prsd_stand)

# Pivot data for inputting into model
ydata_wide <- ydata_oak |>
  dplyr::select(-x, -y) |>
  tidyr::pivot_wider(values_from = oak,
                     names_from = loc) |>
  dplyr::arrange(time)

sand_wide <- xdata_oak |>
  dplyr::select(-aat, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = sand,
                     names_from = loc) |>
  dplyr::arrange(time)

aat_wide <- xdata_oak |>
  dplyr::select(-sand, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = aat,
                     names_from = loc) |>
  dplyr::arrange(time)

tpr_wide <- xdata_oak |>
  dplyr::select(-sand, -aat, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = tpr,
                     names_from = loc) |>
  dplyr::arrange(time)

prsd_wide <- xdata_oak |>
  dplyr::select(-sand, -aat, -tpr, -x, -y) |>
  tidyr::pivot_wider(values_from = prsd,
                     names_from = loc) |>
  dplyr::arrange(time)

# Define data list for model
data <- list()
data$OBS <- dplyr::select(ydata_wide, -time) # observation of relative abundance
data$n <- nrow(data$OBS) # number of time steps
data$ns <- ncol(data$OBS) # number of grid cells
data$mu_alpha <- 0 # prior mean for alpha parameter
data$tau_alpha <- 0.1 # prior precision for alpha parameter
data$mu_beta <- rep(0, times = 5) # prior means for beta parameters
data$tau_beta <- diag(x = c(1, 1, 1, 1, 5),
                      nrow = 5,
                      ncol = 5) # prior precisions for beta parameters with slightly higher precision for intercept
data$aat <- dplyr::select(aat_wide, -time) # observation of temperature
data$tpr <- dplyr::select(tpr_wide, -time) # observation of precipitation
data$prsd <- dplyr::select(prsd_wide, -time) # observation of precipitation seasonality
data$sand <- as.vector(as.matrix(sand_wide[1,2:ncol(sand_wide)])) # observation of soil texture, which doesn't change over time

# Create JAGS model
jm <- rjags::jags.model(file = textConnection(regression_model_fixed_alpha),
                        data = data,
                        n.chains = 3,
                        n.adapt = 5000) # number of adaptation iterations. I decreased due to very slow adaptation with the volume of data in the full domain

# Burn in iterations
update(jm, n.iter = 100000)

# Samples from model
out <- rjags::coda.samples(model = jm,
                           variable.names = c('alpha',
                                              'beta',
                                              'phi_obs',
                                              'tau_proc'),
                           n.iter = 50000) # number of iterations to keep

# Check for convergence
plot(out)
coda::gelman.diag(out, confidence = 0.99)

# Save
save(out, data, regression_model_fixed_alpha,
     file = 'out/hemlock/out_oak_full_domain.RData')
