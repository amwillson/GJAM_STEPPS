pooled_dynamic_beta_regression <- "
model{
#### Priors
for(s in 1:ns){
x[1,s] ~ dbeta(10, 100)
x_stand[1,s] <- x[1,s] / (1 / sqrt(tau_proc))
}
phi_obs ~ dgamma(1, 1)
tau_proc ~ dgamma(1, 1)

#### Fixed Effects
#alpha ~ dnorm(0, 0.1)
alpha ~ dmnorm(mu_alpha, tau_alpha)
beta ~ dmnorm(mu_beta, tau_beta)

#### Data Model
for(t in 1:n){
for(s in 2:ns){
alpha_obs[t,s] <- x[t,s] * phi_obs
beta_obs[t,s] <- (1 - x[t,s]) * phi_obs
OBS[t,s] ~ dbeta(alpha_obs[t,s], beta_obs[t,s])
}
}
#### Process Model
for(t in 2:n){
for(s in 1:ns){
logit(mu[t,s]) <- beta[5] + alpha[s] * x_stand[t-1,s] + beta[1] * aat[t,s] + beta[2] * tpr[t,s] + beta[3] * prsd[t,s] + beta[4] * sand[s]
x[t,s] ~ dnorm(mu[t,s], tau_proc)
x_stand[t,s] <- x[t,s] / (1/sqrt(tau_proc))
}
}
}
"

data <- list()
data$OBS <- dplyr::select(ydata_wide, -time)
data$n <- nrow(data$OBS)
data$ns <- ncol(data$OBS)
data$mu_alpha <- rep(0, times = data$ns)
data$tau_alpha <- diag(x = rep(0.1, times = data$ns),
                       nrow = data$ns,
                       ncol = data$ns)
data$mu_beta <- rep(0, times = 5)
data$tau_beta <- diag(x = c(1, 1, 1, 1, 5),
                      nrow = 5,
                      ncol = 5)
data$aat <- dplyr::select(aat_wide, -time)
data$tpr <- dplyr::select(tpr_wide, -time)
data$prsd <- dplyr::select(prsd_wide, -time)
data$sand <- as.vector(as.matrix(sand_wide[1,2:ncol(sand_wide)]))

jm <- rjags::jags.model(file = textConnection(pooled_dynamic_beta_regression),
                        data = data,
                        n.chains = 3,
                        n.adapt = 100000)

out <- rjags::coda.samples(model = jm,
                           variable.names = c('alpha',
                                              'beta',
                                              'phi_obs',
                                              'tau_proc'),
                           n.iter = 10000)

plot(out)
coda::gelman.diag(out)

mat <- as.matrix(out)

mat <- as.data.frame(mat)

mat |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  dplyr::filter(!(var %in% c('phi_obs', 'tau_proc'))) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var, y = val))
