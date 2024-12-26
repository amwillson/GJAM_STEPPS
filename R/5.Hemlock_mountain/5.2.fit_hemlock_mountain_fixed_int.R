#### STEP 5-2

## Checking effect of intercept on Hemlock Mountain

## Rerunning step 5-1 but with fixed intercept
## The purpose of this is to check whether the value of
## the intercept significantly changes interpretation of
## the slope coefficients. If patterns in the slopes remain
## similar after fixing the intercept, then we can be more
## confident in our predictions

## Here, I fixed the intercept to a value randomly drawn from
## the distribution of values estimated from the model with
## the estimated intercept (step 5-1)

## Note that the model is not fit to the entire study domain
## because it is very computationally expensive to do so

## Input: data/processed/mean_stepps_soil_clim.RData
## Original in-sample and out-of-sample data from running the model in steps 3.3-3.7
## Combined with the full dataset to use all available data for model fitting

## Input: data/processed/mean_stepps_full_oos.RData
## All out-of-sample data used in steps 3.11-3.13

## Output: out/hemlock/out_small_hemlock_mountain_fixedint.RData
## Fitted model with parameter estimates, the data used to fit
## the model and the model code for only a small
## region of high hemlock abundance in northern Wisconsin
## using the model with the fixed intercept

## Output: out/hemlock/out_med_hemlock_mountain_fixedint.RData
## Fitted model with parameter estimates, the data used to fit
## the model and the model code for a larger region
## surrounding the high hemlock abundance region of northern
## Wisconsin using the model with the fixed intercept

## Output: out/hemlock/out_large_hemlock_mountain_fixedint.RData
## Fitted model with parameter estimates, the data used to fit
## the model and the model code for an even larger region
## surrounding the high hemlock abundance region of northern
## Wisconsin. This includes grid cells within minimal spatial
## autocorrelation using the model with the fixed intercept

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
                hemlock)

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
logit(mu[t,s]) <- int + alpha * x_stand[t-1,s] + beta[1] * aat[t,s] + beta[2] * tpr[t,s] + beta[3] * prsd[t,s] + beta[4] * sand[s]
x[t,s] ~ dnorm(mu[t,s], tau_proc) # Latent relative abundance drawn from normal distribution
x_stand[t,s] <- x[t,s] / (1/sqrt(tau_proc)) # Expressing in units of standard deviation as above
}
}
}
"

#### Case 1: Hemlock Mountain region only ####

## Maximum hemlock abundance at x = 445000, y = 982000
## This also corresponds to maximum bias in hemlock prediction from GJAM
## Take 24 grid cells surrounding the peak in abundance

# 25 grid cells near Hemlock Mountain
hemlock_mountain_locs <- expand.grid(x = c(397000, 421000, 445000,
                                           469000, 493000),
                                     y = c(934000, 958000, 982000,
                                           1006000, 1030000))
# Add location index
hemlock_mountain_locs$loc <- paste0(hemlock_mountain_locs$x, '_', hemlock_mountain_locs$y)

# Subset data for only Hemlock Mountain region
xdata_hm <- dplyr::filter(xdata, loc %in% hemlock_mountain_locs$loc)
ydata_hm <- dplyr::filter(ydata, loc %in% hemlock_mountain_locs$loc)

# Map of study region
states <- map_states()

# Plot subsetted area
# Make sure region follows anticipated patterns and see spatial domain we have subsampled
ydata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                limits = c(0, 1),
                                name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~time) +
  ggplot2::ggtitle('Hemlock relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ydata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = hemlock, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Hemlock relative abundance') +
  ggplot2::theme_minimal()

# Plot covariates
# Make sure covariates follow anticipated patterns
xdata_hm |>
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

xdata_hm |>
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

xdata_hm |>
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

xdata_hm |>
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

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = sand, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Soil % sand') +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = aat, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Average annual temperature (°C)') +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = tpr, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Total annual precipitation (mm)') +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = prsd, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Precipitation seasonality (mm)') +
  ggplot2::theme_minimal()

# Standardize covariates
# Represent in units of standard deviation
# To match how relative abundance is standardized in the model
xdata_hm <- xdata_hm |>
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
ydata_wide <- ydata_hm |>
  dplyr::select(-x, -y) |>
  tidyr::pivot_wider(values_from = hemlock,
                     names_from = loc) |>
  dplyr::arrange(time)

sand_wide <- xdata_hm |>
  dplyr::select(-aat, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = sand,
                     names_from = loc) |>
  dplyr::arrange(time)

aat_wide <- xdata_hm |>
  dplyr::select(-sand, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = aat,
                     names_from = loc) |>
  dplyr::arrange(time)

tpr_wide <- xdata_hm |>
  dplyr::select(-sand, -aat, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = tpr,
                     names_from = loc) |>
  dplyr::arrange(time)

prsd_wide <- xdata_hm |>
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
data$mu_beta <- rep(0, times = 4) # prior means for beta parameters
data$tau_beta <- diag(x = c(1, 1, 1, 1),
                      nrow = 4,
                      ncol = 4) # prior precisions for beta parameters with slightly higher precision for intercept
data$int <- rnorm(1, mean = -0.2, sd = 0.1) # fixed intercept term
data$aat <- dplyr::select(aat_wide, -time) # observation of temperature
data$tpr <- dplyr::select(tpr_wide, -time) # observation of precipitation
data$prsd <- dplyr::select(prsd_wide, -time) # observation of precipitation seasonality
data$sand <- as.vector(as.matrix(sand_wide[1,2:ncol(sand_wide)])) # observation of soil texture, which doesn't change over time

# Create JAGS model
jm <- rjags::jags.model(file = textConnection(regression_model_fixed_alpha),
                        data = data,
                        n.chains = 3,
                        n.adapt = 500000) # number of adaptation iterations

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
     file = 'out/hemlock/out_small_hemlock_mountain_fixedint.RData')

#### Case 2: 49 cell Hemlock Mountain ####

## Maximum hemlock abundance at x = 445000, y = 982000
## This also corresponds to maximum bias in hemlock prediction from GJAM
## Take 48 grid cells surrounding the peak in abundance

# 49 grid cells near Hemlock Mountain
hemlock_mountain_locs <- expand.grid(x = c(373000, 397000, 421000, 445000,
                                           469000, 493000, 517000),
                                     y = c(910000, 934000, 958000, 982000,
                                           1006000, 1030000, 1054000))
# Add location index
hemlock_mountain_locs$loc <- paste0(hemlock_mountain_locs$x, '_', hemlock_mountain_locs$y)

# Subset data for only Hemlock Mountain region
xdata_hm <- dplyr::filter(xdata, loc %in% hemlock_mountain_locs$loc)
ydata_hm <- dplyr::filter(ydata, loc %in% hemlock_mountain_locs$loc)

# Map of study region
states <- map_states()

# Plot subsetted area
# Make sure region follows anticipated patterns and see spatial domain we have subsampled
ydata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                limits = c(0, 1),
                                name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~time) +
  ggplot2::ggtitle('Hemlock relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ydata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = hemlock, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Hemlock relative abundance') +
  ggplot2::theme_minimal()

# Plot covariates
# Make sure covariates follow anticipated patterns
xdata_hm |>
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

xdata_hm |>
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

xdata_hm |>
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

xdata_hm |>
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

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = sand, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Soil % sand') +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = aat, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Average annual temperature (°C)') +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = tpr, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Total annual precipitation (mm)') +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = prsd, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Precipitation seasonality (mm)') +
  ggplot2::theme_minimal()

# Standardize covariates
# Represent in units of standard deviation
# To match how relative abundance is standardized in the model
xdata_hm <- xdata_hm |>
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
ydata_wide <- ydata_hm |>
  dplyr::select(-x, -y) |>
  tidyr::pivot_wider(values_from = hemlock,
                     names_from = loc) |>
  dplyr::arrange(time)

sand_wide <- xdata_hm |>
  dplyr::select(-aat, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = sand,
                     names_from = loc) |>
  dplyr::arrange(time)

aat_wide <- xdata_hm |>
  dplyr::select(-sand, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = aat,
                     names_from = loc) |>
  dplyr::arrange(time)

tpr_wide <- xdata_hm |>
  dplyr::select(-sand, -aat, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = tpr,
                     names_from = loc) |>
  dplyr::arrange(time)

prsd_wide <- xdata_hm |>
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
data$mu_beta <- rep(0, times = 4) # prior means for beta parameters
data$tau_beta <- diag(x = c(1, 1, 1, 1),
                      nrow = 4,
                      ncol = 4) # prior precisions for beta parameters with slightly higher precision for intercept
data$int <- rnorm(1, mean = -0.2, sd = 0.1) # fixed intercept term
data$aat <- dplyr::select(aat_wide, -time) # observation of temperature
data$tpr <- dplyr::select(tpr_wide, -time) # observation of precipitation
data$prsd <- dplyr::select(prsd_wide, -time) # observation of precipitation seasonality
data$sand <- as.vector(as.matrix(sand_wide[1,2:ncol(sand_wide)])) # observation of soil texture, which doesn't change over time

# Create JAGS model
jm <- rjags::jags.model(file = textConnection(regression_model_fixed_alpha),
                        data = data,
                        n.chains = 3,
                        n.adapt = 500000) # number of adaptation iterations

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
     file = 'out/hemlock/out_med_hemlock_mountain_fixedint.RData')

#### Case 3: 81 cell Hemlock Mountain ####

## Maximum hemlock abundance at x = 445000, y = 982000
## This also corresponds to maximum bias in hemlock prediction from GJAM
## Take 80 grid cells surrounding the peak in abundance

# 81 grid cells near Hemlock Mountain
hemlock_mountain_locs <- expand.grid(x = c(349000, 373000, 397000, 421000, 445000,
                                           469000, 493000, 517000, 541000),
                                     y = c(886000, 910000, 934000, 958000, 982000,
                                           1006000, 1030000, 1054000, 1078000))
# Add location index
hemlock_mountain_locs$loc <- paste0(hemlock_mountain_locs$x, '_', hemlock_mountain_locs$y)

# Subset data for only Hemlock Mountain region
xdata_hm <- dplyr::filter(xdata, loc %in% hemlock_mountain_locs$loc)
ydata_hm <- dplyr::filter(ydata, loc %in% hemlock_mountain_locs$loc)

# Map of study region
states <- map_states()

# Plot subsetted area
# Make sure region follows anticipated patterns and see spatial domain we have subsampled
ydata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                limits = c(0, 1),
                                name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~time) +
  ggplot2::ggtitle('Hemlock relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

ydata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = hemlock, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Hemlock relative abundance') +
  ggplot2::theme_minimal()

# Plot covariates
# Make sure covariates follow anticipated patterns
xdata_hm |>
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

xdata_hm |>
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

xdata_hm |>
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

xdata_hm |>
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

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = sand, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Soil % sand') +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = aat, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Average annual temperature (°C)') +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = tpr, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Total annual precipitation (mm)') +
  ggplot2::theme_minimal()

xdata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = prsd, color = loc),
                     show.legend = FALSE) +
  ggplot2::xlab('Time step') + ggplot2::ylab('Precipitation seasonality (mm)') +
  ggplot2::theme_minimal()

# Standardize covariates
# Represent in units of standard deviation
# To match how relative abundance is standardized in the model
xdata_hm <- xdata_hm |>
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
ydata_wide <- ydata_hm |>
  dplyr::select(-x, -y) |>
  tidyr::pivot_wider(values_from = hemlock,
                     names_from = loc) |>
  dplyr::arrange(time)

sand_wide <- xdata_hm |>
  dplyr::select(-aat, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = sand,
                     names_from = loc) |>
  dplyr::arrange(time)

aat_wide <- xdata_hm |>
  dplyr::select(-sand, -tpr, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = aat,
                     names_from = loc) |>
  dplyr::arrange(time)

tpr_wide <- xdata_hm |>
  dplyr::select(-sand, -aat, -prsd, -x, -y) |>
  tidyr::pivot_wider(values_from = tpr,
                     names_from = loc) |>
  dplyr::arrange(time)

prsd_wide <- xdata_hm |>
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
data$mu_beta <- rep(0, times = 4) # prior means for beta parameters
data$tau_beta <- diag(x = c(1, 1, 1, 1),
                      nrow = 4,
                      ncol = 4) # prior precisions for beta parameters with slightly higher precision for intercept
data$int <- rnorm(1, mean = -0.2, sd = 0.1) # fixed intercept term
data$aat <- dplyr::select(aat_wide, -time) # observation of temperature
data$tpr <- dplyr::select(tpr_wide, -time) # observation of precipitation
data$prsd <- dplyr::select(prsd_wide, -time) # observation of precipitation seasonality
data$sand <- as.vector(as.matrix(sand_wide[1,2:ncol(sand_wide)])) # observation of soil texture, which doesn't change over time

# Create JAGS model
jm <- rjags::jags.model(file = textConnection(regression_model_fixed_alpha),
                        data = data,
                        n.chains = 3,
                        n.adapt = 500000) # number of adaptation iterations

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
     file = 'out/hemlock/out_large_hemlock_mountain_fixedint.RData')
