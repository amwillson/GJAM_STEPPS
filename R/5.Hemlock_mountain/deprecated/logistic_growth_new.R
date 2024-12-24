logistic_growth <- "
model{
#### Priors
for(s in 1:ns){
x[1,s] ~ dbeta(10, 100)
}
phi_obs ~ dgamma(1, 1)
phi_proc ~ dgamma(1, 1)

#### Fixed Effects
#beta ~ dmnorm(mu_beta, tau_beta)
r ~ dmnorm(mu_r, tau_r)
K ~ dbeta(1, 1)

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
logit(mu[t,s]) <- x[t-1,s] + r[s] * x[t-1,s] * (1 - x[t-1,s] / K)
#logit(mu[t,s]) <- x[t-1,s] + r[map[s]] * x[t-1,s] * (1 - x[t-1,s] / K[t,s])
#logit(K[t,s]) <- beta[1] + beta[2] * aat[t,s] + beta[3] * tpr[t,s] + beta[4] * prsd[t,s] + beta[5] * sand[s]
alpha_proc[t,s] <- mu[t,s] * phi_proc
beta_proc[t,s] <- (1 - mu[t,s]) * phi_proc
x[t,s] ~ dbeta(alpha_proc[t,s], beta_proc[t,s])
}
}
}
"

data <- list()
data$OBS <- dplyr::select(ydata_wide, -time)
data$n <- nrow(data$OBS)
data$ns <- ncol(data$OBS)
#data$mu_beta <- rep(0, times = 5)
#data$tau_beta <- diag(x = rep(0.1, times = 5),
#                      nrow = 5,
#                      ncol = 5)
data$mu_r <- rep(0, times = data$ns)
data$tau_r <- diag(x = rep(0.01, times = data$ns),
                   nrow = data$ns,
                   ncol = data$ns)
#data$aat <- dplyr::select(aat_wide, -time)
#data$tpr <- dplyr::select(tpr_wide, -time)
#data$prsd <- dplyr::select(prsd_wide, -time)
#sand <- sand_wide[1,2:703]
#data$sand <- as.vector(as.matrix(sand))
#map <- hm_cells_wide[1,2:703]
#data$map <- as.vector(as.matrix(map))

jm <- rjags::jags.model(file = textConnection(logistic_growth),
                        data = data,
                        n.chains = 3,
                        n.adapt = 1000)

out <- rjags::coda.samples(model = jm,
                           variable.names = c('K',
                                              #'beta',
                                              'r',
                                              'phi_obs',
                                              'phi_proc'),
                           n.iter = 1000)

plot(out)
coda::gelman.diag(out)
