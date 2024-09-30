beta_regression <- "
model{
#### Priors
for(s in 1:ns){
x[1,s] ~ dbeta(10, 100)
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
logit(mu[t,s]) <- x[t-1,s] + alpha[map[t,s]] * x[t-1,s] + beta[1] * aat[t,s] + beta[2] * tpr[t,s] + beta[3] * prsd[t,s] + beta[4] * sand[t,s]
alpha_proc[t,s] <- mu[t,s] * phi_proc
beta_proc[t,s] <- mu[t,s] * phi_proc
x[t,s] ~ dbeta(alpha_proc[t,s], beta_proc[t,s])
}
}
}
"

data <- list()
data$OBS <- dplyr::select(ydata_wide, -time)
data$n <- nrow(data$OBS)
data$ns <- ncol(data$OBS)
data$mu_beta <- rep(0, times = 4)
data$tau_beta <- diag(x = rep(0.1, times = 4),
                      nrow = 4,
                      ncol = 4)
data$mu_alpha <- rep(1, times = background_id)
data$tau_alpha <- diag(x = rep(0.1, times = background_id),
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
                        n.adapt = 10000)

out <- rjags::coda.samples(model = jm,
                           variable.names = c('beta',
                                              'alpha',
                                              'phi_obs',
                                              'phi_proc'),
                           n.iter = 10000)

plot(out)
coda::gelman.diag(out)

out_mat <- as.matrix(out)

alpha_mat <- out_mat[,1:26]

alpha_df <- as.data.frame(alpha_mat)

alpha_df |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = var, y = val)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

alpha_mean <- apply(alpha_mat, 2, mean)

alpha_mean <- as.data.frame(alpha_mean)

alpha_mean2 <- alpha_mean |>
  dplyr::mutate(id = dplyr::row_number(),
                id = as.character(id)) |>
  dplyr::left_join(y = hemlock_mountain_locs2,
                   by = 'id') |>
  dplyr::mutate(x = sub(pattern = '_.*', replacement = '', x = loc),
                y = sub(pattern = '.*_', replacement = '', x = loc),
                x = as.numeric(x),
                y = as.numeric(y))

alpha_mean2 |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = alpha_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
