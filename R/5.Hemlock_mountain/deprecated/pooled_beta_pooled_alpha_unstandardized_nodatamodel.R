## Building simple dynamic linear model for testing
## temporal depedence in STEPPS record for a subset
## of locations

## Simplest possible beta regression

rm(list = ls())

# Helper funs
source('R/funs.R')

#### Format all data ####

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
  dplyr::mutate(loc = paste0(stepps_x, '_', stepps_y)) |>
  # Take only covariates and indexing
  dplyr::select(time, stepps_x, stepps_y, loc, 
                sand, aat, tpr, prsd)

# Format response variable (ydata)
# Note that currently, we're only interested in hemlock
# because only looking at Hemlock Mountain
ydata <- taxon_all |>
  # Remove NAs where grid cells are empty
  tidyr::drop_na() |>
  # Add location index
  dplyr::mutate(loc = paste0(stepps_x, '_', stepps_y)) |>
  # Take hemlock and indexing
  dplyr::select(time, stepps_x, stepps_y, loc, 
                hemlock)

# Need to change the direction time moves
# Time is currently in YBP so runs backward
# Index time to go from 1 to 17 instead
times <- as.data.frame(unique(xdata$time)[rev(order(unique(xdata$time)))])
colnames(times) <- 'time'
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

# Standardize covariates
xdata <- xdata |>
  dplyr::mutate(sand_stand = sand / sd(sand),
                aat_stand = aat / sd(aat),
                tpr_stand = tpr / sd(tpr),
                prsd_stand = prsd / sd(prsd)) |>
  dplyr::select(-sand, -aat, -tpr, -prsd) |>
  dplyr::rename(sand = sand_stand,
                aat = aat_stand,
                tpr = tpr_stand,
                prsd = prsd_stand)

#### Define Model ####

## Dynamic linear model
## Current hemlock abundance = previous abundance + previous abundance * alpha + environment * beta

dlm_beta_simple <- "
model{
#### Priors
for(s in 1:ns){
x[1,s] ~ dbeta(alpha_ic, beta_ic) # Initial condition of hemlock abundance
sigma[1,s] <- sqrt((x[1,s] * (1 - x[1,s])) / (1 + phi_add))
x_scaled[1,s] <- x[1,s] / sigma[1,s]
}

phi_add ~ dgamma(a_add, r_add) # prior on process precision

#### Fixed Effects
beta ~ dmnorm(mu_beta, tau_beta) # Prior on beta coefficients
alpha ~ dnorm(mu_alpha, tau_alpha) # Prior on temporal dependence coefficients

#### Process Model
for(t in 2:n){
for(s in 1:ns){
# Process equation
logit(mu[t,s]) <- OBS[t-1,s] + alpha * OBS[t-1,s] + beta[1] * sand[t,s] + beta[2] * aat[t,s] + beta[3] * tpr[t,s] + beta[4] * prsd[t,s]
alpha_add[t,s] <- mu[t,s] * phi_add # beta distribution shape parameter
beta_add[t,s] <- (1 - mu[t,s]) * phi_add # beta distribution shape parameter
OBS[t,s] ~ dbeta(alpha_add[t,s], beta_add[t,s]) # latent relative abundance drawn from beta distribution
}
}
}
"

#### Hemlock Mountain ####

# Maximum at x = 445000, y = 982000
# Surrounding x = 397000, 421000, 469000, 493000
# Surrounding y = 934000, 958000, 1006000, 1030000

## Note different size samples

hemlock_mountain_locs <- expand.grid(x = c(373000, 397000, 421000,
                                           445000, 469000, 493000,
                                           517000),
                                     y = c(910000, 934000, 958000,
                                           982000, 1006000, 1030000,
                                           1054000))
#hemlock_mountain_locs <- expand.grid(x = c(349000, 373000, 397000,
#                                           421000, 445000, 469000,
#                                           493000, 517000, 541000),
#                                     y = c(886000, 910000, 934000,
#                                           958000, 982000, 1006000,
#                                           1030000, 1054000, 1078000))
#hemlock_mountain_locs <- expand.grid(x = c(397000, 421000, 445000,
#                                           469000, 493000),
#                                     y = c(934000, 958000, 982000,
#                                           1006000, 1030000))
hemlock_mountain_locs$loc <- paste0(hemlock_mountain_locs$x, '_', hemlock_mountain_locs$y)

# Subset data for only Hemlock Mountain region
xdata_hm <- dplyr::filter(xdata, loc %in% hemlock_mountain_locs$loc)
ydata_hm <- dplyr::filter(ydata, loc %in% hemlock_mountain_locs$loc)

# Map of study region
states <- map_states()

# Plot subsetted area
ydata_hm |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = stepps_x, y = stepps_y, fill = hemlock)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Greens',
                                direction = 1,
                                limits = c(0, 1),
                                name = 'Relative\nabundance') +
  ggplot2::facet_wrap(~time) +
  ggplot2::ggtitle('Hemlock relative abundance') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

## Pivot data
ydata_wide <- ydata_hm |>
  dplyr::select(-stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = hemlock,
                     names_from = loc) |>
  dplyr::arrange(time)
sand_wide <- xdata_hm |>
  dplyr::select(-aat, -tpr, -prsd, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = sand,
                     names_from = loc) |>
  dplyr::arrange(time)
aat_wide <- xdata_hm |>
  dplyr::select(-sand, -tpr, -prsd, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = aat,
                     names_from = loc) |>
  dplyr::arrange(time)
tpr_wide <- xdata_hm |>
  dplyr::select(-sand, -aat, -prsd, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = tpr,
                     names_from = loc) |>
  dplyr::arrange(time)
prsd_wide <- xdata_hm |>
  dplyr::select(-sand, -aat, -tpr, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = prsd,
                     names_from = loc) |>
  dplyr::arrange(time)

# Empty list
data <- list()
# Hemlock abundance observations
data$OBS <- dplyr::select(ydata_wide, -time)
# Number of time steps
data$n <- nrow(data$OBS)
# Number of locations
data$ns <- ncol(data$OBS)
# Initial hemlock abundance mean
mu_ic <- 0.2
# Initial hemlock abundance precision
tau_ic <- 10
# Moment matching
alpha_ic <- (mu_ic^2 - mu_ic^3 - mu_ic * (1/tau_ic)) / (1/tau_ic)
beta_ic <- (mu_ic - 2*mu_ic^2 + mu_ic^3 - (1/tau_ic) + mu_ic * (1/tau_ic)) / (1/tau_ic)
# Beta prior parameters
data$alpha_ic <- alpha_ic
data$beta_ic <- beta_ic
# Prior parameters for observation and process uncertainty
data$a_add <- 1
data$r_add <- 1
# Prior parameters for beta coefficients
# This doesn't work very well
# Priors taken from GJAM model bgibbs
# Order: sand, aat, tpr, prsd
data$mu_beta <- c(0, -0.03, 0.05, -0.03)
data$tau_beta <- diag(x = c(60000, 38000,
                            40000, 60000),
                      nrow = 4,
                      ncol = 4)
#data$mu_beta <- rep(0, times = 4)
#data$tau_beta <- diag(x = rep(0.001, times = 4),
#                      nrow = 4,
#                      ncol = 4)
# Prior parameters for alpha coefficient
data$mu_alpha <- 0
data$tau_alpha <- 0.001
# Covariates
data$sand <- dplyr::select(sand_wide, -time)
data$aat <- dplyr::select(aat_wide, -time)
data$tpr <- dplyr::select(tpr_wide, -time)
data$prsd <- dplyr::select(prsd_wide, -time)

# Create JAGS model with 3 chains
jm <- rjags::jags.model(file = textConnection(dlm_beta_simple),
                        data = data,
                        n.chains = 3,
                        n.adapt = 10000)

# Posterior samples of pooled parameters
hm_out_pooled <- rjags::coda.samples(model = jm,
                                     variable.names = c('beta',
                                                        'alpha',
                                                        'phi_add'),
                                     n.iter = 1000,
                                     thin = 1)

plot(hm_out_pooled)
