## Building simple dynamic linear model for testing
## temporal depedence in STEPPS record for a subset
## of locations

## 1. Hemlock Mountain
## 2. Pine stands
## 3. Oak savannas

## NOTE: Right now, it's only hemlock mountain

rm(list = ls())

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

#### Define Model ####

## Dynamic linear model
## Current hemlock abundance = previous abundance + previous abundance * alpha + environment * beta
## Written in two ways (uncomment the version you want)
## One is beta regression and one is Gaussian
## The Gaussian version converges a lot better which is why it's included

dlm_beta <- "
model{
#### Priors
for(s in 1:ns){
x[1,s] ~ dbeta(alpha_ic, beta_ic) # Initial condition of hemlock abundance
}

## for guassian model
#tau_obs ~ dgamma(a_obs, r_obs) # Prior on observation error
#tau_add ~ dgamma(a_add, r_add) # Prior on process error

## for beta model
phi_obs ~ dgamma(a_obs, r_obs)
phi_add ~ dgamma(a_add, r_add)

#### Fixed Effects
beta ~ dmnorm(mu_beta, tau_beta) # Prior on beta coefficients
alpha ~ dmnorm(mu_alpha, tau_alpha) # Prior on temporal dependence coefficients

#### Data Model
for(t in 1:n){
for(s in 1:ns){
## for  gaussian model
#OBS[t,s] ~ dnorm(x[t,s], tau_obs) # observation drawn from normal distribution

## for beta model
alpha_obs[t,s] <- x[t,s] * phi_obs
beta_obs[t,s] <- (1 - x[t,s]) * phi_obs
OBS[t,s] ~ dbeta(alpha_obs[t,s], beta_obs[t,s])
}
}

#### Process Model
for(t in 2:n){
for(s in 1:ns){
## for gaussian model
# Process equation
#mu[t,s] <- x[t-1,s] + alpha[s] * x[t-1,s] + beta[1] * sand[t,s] + beta[2] * aat[t,s] + beta[3] * tpr[t,s] + beta[4] * prsd[t,s]
#x[t,s] ~ dnorm(mu[t,s], tau_add) # latent state drawn from normal distribution

## for beta model
logit(mu[t,s]) <- x[t-1,s] + alpha[s] * x[t-1,s] + beta[1] * sand[t,s] + beta[2] * aat[t,s] + beta[3] * tpr[t,s] + beta[4] * prsd[t,s]
alpha_add[t,s] <- mu[t,s] * phi_add
beta_add[t,s] <- (1 - mu[t,s]) * phi_add
x[t,s] ~ dbeta(alpha_add[t,s], beta_add[t,s])
}
}
}
"

#### Hemlock Mountain ####

## CURRENTLY TRYING WITHOUT SUBSETTING LOCATION

# Maximum at x = 445000, y = 982000
# Surrounding x = 397000, 421000, 469000, 493000
# Surrounding y = 934000, 958000, 1006000, 1030000

hemlock_mountain_locs <- expand.grid(x = c(397000, 421000, 445000,
                                           469000, 493000),
                                     y = c(934000, 958000, 982000,
                                           1006000, 1030000))
hemlock_mountain_locs$loc <- paste0(hemlock_mountain_locs$x, '_', hemlock_mountain_locs$y)

# Subset data for only Hemlock Mountain region
xdata_hm <- dplyr::filter(xdata, loc %in% hemlock_mountain_locs$loc)
ydata_hm <- dplyr::filter(ydata, loc %in% hemlock_mountain_locs$loc)

## Pivot data
ydata_wide <- ydata |>
  dplyr::select(-stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = hemlock,
                     names_from = loc) |>
  dplyr::arrange(time)
sand_wide <- xdata |>
  dplyr::select(-aat, -tpr, -prsd, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = sand,
                     names_from = loc) |>
  dplyr::arrange(time)
aat_wide <- xdata |>
  dplyr::select(-sand, -tpr, -prsd, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = aat,
                     names_from = loc) |>
  dplyr::arrange(time)
tpr_wide <- xdata |>
  dplyr::select(-sand, -aat, -prsd, -stepps_x, -stepps_y) |>
  tidyr::pivot_wider(values_from = tpr,
                     names_from = loc) |>
  dplyr::arrange(time)
prsd_wide <- xdata |>
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
data$a_obs <- 1
data$r_obs <- 1
data$a_add <- 1
data$r_add <- 1
# Prior parameters for beta coefficients
data$mu_beta <- rep(0, times = 4)
data$tau_beta <- diag(x = rep(0.001, times = 4),
                      nrow = 4,
                      ncol = 4)
# Prior parameters for alpha coefficient
data$mu_alpha <- rep(0, times = data$ns)
data$tau_alpha <- diag(x = rep(0.001, times = data$ns),
                       nrow = data$ns,
                       ncol = data$ns)
# Covariates
data$sand <- dplyr::select(sand_wide, -time)
data$aat <- dplyr::select(aat_wide, -time)
data$tpr <- dplyr::select(tpr_wide, -time)
data$prsd <- dplyr::select(prsd_wide, -time)

# Create JAGS model with 3 chains
jm <- rjags::jags.model(file = textConnection(dlm_beta),
                        data = data,
                        n.chains = 3,
                        n.adapt = 100000)

# Save model!!!!!!!
save(jm, file = 'hemlock_jm.RData')

# Posterior samples of parameters
#hm_out_params <- rjags::coda.samples(model = jm,
#                                     variable.names = c('beta',
#                                                        'alpha',
#                                                        'phi_obs',
#                                                        'phi_add'#,
                                                        #'tau_add', 
                                                        #'tau_obs'
#                                                        ),
#                                     n.iter = 100,
#                                     thin = 1)

# Posterior samples of pooled parameters
hm_out_pooled <- rjags::coda.samples(model = jm,
                                     variable.names = c('beta',
                                                        'phi_obs',
                                                        'phi_add'),
                                     n.iter = 100,
                                     thin = 1)
# Posterior samples of alphas
hm_out_alpha <- rjags::coda.samples(model = jm,
                                    variable.names = 'alpha',
                                    n.iter = 100,
                                    thin = 1)
# Checks for convergence
plot(hm_out_params)
coda::gelman.diag(hm_out_params, confidence = 0.99)

# Extract parameter estimates
param_out <- as.data.frame(as.matrix(hm_out_params))

### Figures ###

# Violin plots of parameter distributions
# Just checking to make sure values are appropriate
param_out |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'param',
                      values_to = 'estimate') |>
  # These are different order of magnitude so remove
  dplyr::filter(!(param %in% c('tau_add', 'tau_obs', 'phi_add', 'phi_obs'))) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = param, y = estimate)) +
  ggplot2::theme_minimal()

# Column names related to locations, covariate, or uncertainty
colnames(param_out) <- c(colnames(ydata_wide[2:ncol(ydata_wide)]),
                         'sand', 'aat', 'tpr', 'prsd',
                         'tau_add', 'tau_obs')

# Parameters related to environment
env_params <- param_out |>
  dplyr::select(sand:prsd) |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'param',
                      values_to = 'estimate')

# Parameters related to lags over time
lag_params <- param_out |>
  dplyr::select(-(sand:tau_obs)) |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'param',
                      values_to = 'estimate') |>
  dplyr::mutate(x = as.numeric(sub(pattern = '_.*', replacement = '', x = param)),
                y = as.numeric(sub(pattern = '.*_', replacement = '', x = param)))

# Parameters related to uncertainty
uncert_params <- param_out |>
  dplyr::select(tau_add, tau_obs) |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'param',
                      values_to = 'estimate')

# Plot mean lags
lag_params |>
  dplyr::group_by(param, x, y) |>
  dplyr::summarize(mean = mean(estimate)) |>
  #dplyr::filter(mean > 0) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = mean)) +
  ggplot2::theme_void()

# Plot median +/- 95% CrI
lag_params |>
  dplyr::group_by(param, x, y) |>
  dplyr::summarize(low = quantile(estimate, probs = 0.025),
                   median = median(estimate),
                   high = quantile(estimate, probs = 0.975)) |>
  #dplyr::mutate(low = dplyr::if_else(low < 0 & median > 0, NA, low),
  #              high = dplyr::if_else(high > 0 & median < 0, NA, high),
  #              median = dplyr::if_else(is.na(low) | is.na(high), NA, median)) |>
  tidyr::pivot_longer(cols = low:high,
                      names_to = 'quantile',
                      values_to = 'estimate') |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = estimate)) +
  ggplot2::facet_wrap(~factor(quantile, levels = c('low', 'median', 'high'))) +
  ggplot2::theme_void()

# versus observed hemlock at one time period
ydata_hm |>
  dplyr::mutate(x = as.numeric(sub(pattern = '_.*', replacement = '', x = loc)),
                y = as.numeric(sub(pattern = '.*_', replacement = '', x = loc))) |>
  dplyr::filter(time == 17) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hemlock)) +
  ggplot2::theme_void()

# versus difference between time steps
unique_times <- unique(ydata_hm$time)
unique_locs <- unique(ydata_hm$loc)

diff <- matrix(, nrow = (length(unique_times)-1), ncol = length(unique_locs))

for(s in 1:length(unique_locs)){
  sub <- dplyr::filter(ydata_hm, loc == unique_locs[s])
  for(t in 1:(length(unique_times)-1)){
    diff[t,s] <- dplyr::filter(sub, time == t+1)$hemlock - dplyr::filter(sub, time == t)$hemlock
  }
}

colnames(diff) <- unique_locs
rownames(diff) <- 1:(length(unique_times)-1)

diff <- as.data.frame(diff)

diff |>
  tibble::rownames_to_column(var = 'time') |>
  tidyr::pivot_longer(cols = -time,
                      names_to = 'loc',
                      values_to = 'diff') |>
  dplyr::mutate(x = as.numeric(sub(pattern = '_.*', replacement = '', x = loc)),
                y = as.numeric(sub(pattern = '.*_', replacement = '', x = loc))) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = diff)) +
  ggplot2::facet_wrap(~as.numeric(time)) +
  ggplot2::theme_void()
