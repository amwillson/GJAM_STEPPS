beta_regression <- "
model{
#### Priors
for(s in 1:ns){
x[1,s] ~ dbeta(10, 100)
x_scaled[1,s] ~ dunif(0.0001, 0.9999)
}
phi_obs ~ dgamma(1, 1)
phi_proc ~ dgamma(1, 1)

#### Fixed Effects
beta ~ dmnorm(mu_beta, tau_beta)
alpha ~ dmnorm(mu_alpha, tau_alpha)

#### Data Model
for(t in 1:n){
for(s in 1:ns){
alpha_obs[t,s] <- x[t,s] * phi_obs
beta_obs[t,s] <- (1 - x[t,s]) * phi_obs
OBS[t,s] ~ dbeta(alpha_obs[t,s], beta_obs[t,s])
}
}
#### Process Model
for(t in 2:n){
for(s in 1:ns){
logit(mu[t,s]) <- x[t-1,s] + alpha[map[t,s]] * x_scaled[t-1,s] + beta[1] * aat[t,s] + beta[2] * tpr[t,s] + beta[3] * prsd[t,s] + beta[4] * sand[t,s]
alpha_proc[t,s] <- mu[t,s] * phi_proc
beta_proc[t,s] <- mu[t,s] * phi_proc
x[t,s] ~ dbeta(alpha_proc[t,s], beta_proc[t,s])
sigma[t,s] <- sqrt((mu[t,s] * (1 - mu[t,s])) / (1 + phi_proc))
x_scaled[t,s] <- mu[t,s] / sigma[t,s]
}
}
}
"

data <- list()
data$OBS <- dplyr::select(ydata_wide, -time)
data$n <- nrow(data$OBS)
data$ns <- ncol(data$OBS)
data$mu_beta <- rep(0, times = 4)
data$tau_beta <- diag(x = rep(0.01, times = 4),
                      nrow = 4,
                      ncol = 4)
data$mu_alpha <- rep(0, times = background_id)
data$tau_alpha <- diag(x = rep(0.01, times = background_id),
                       nrow = background_id,
                       ncol = background_id)
data$aat <- dplyr::select(aat_wide, -time)
data$tpr <- dplyr::select(tpr_wide, -time)
data$prsd <- dplyr::select(prsd_wide, -time)
data$sand <- dplyr::select(sand_wide, -time)
data$map <- dplyr::select(hm_cells_wide, -time)

jm <- rjags::jags.model(file = textConnection(beta_regression),
                        data = data,
                        n.chains = 3,
                        n.adapt = 1000)

out <- rjags::coda.samples(model = jm,
                           variable.names = c('beta',
                                              'alpha',
                                              'phi_obs',
                                              'phi_proc'),
                           n.iter = 1000)

plot(out)
coda::gelman.diag(out)
