library(tidyverse)
library(MASS)


n_participants <- 50
n_data_per_session <- 200
p <- .66

props_indiv_diff <- rnorm(n_participants, 1, .2)

props_normal1 <- rbeta(n_participants, ceiling(10*p), floor(10*(1-p)))
props_normal2 <- rbeta(n_participants, ceiling(10*p), floor(10*(1-p)))

one_session <- function(prop_normal, n_data_per_session, participant_id, props_indiv_diff) {
  n_normal <- ceiling(n_data_per_session * prop_normal)
  n_uniform <- floor(n_data_per_session * (1 - prop_normal))
  data_normal <- rnorm(n_normal, mean = 0, sd = props_indiv_diff[participant_id])
  data_uniform <- runif(n_uniform, -3, 3)
  tbl_y <- tibble(p_id = participant_id, y = c(data_normal, data_uniform))
  return(tbl_y)
}

l_data_session1 <- pmap(list(props_normal1, n_data_per_session, 1:n_participants), one_session, props_indiv_diff)
l_data_session2 <- pmap(list(props_normal2, n_data_per_session, 1:n_participants), one_session, props_indiv_diff)


my_optim <- function(y, p, f) {
  o <- optim(p, f, y = y$y)
  return(o$par)
}

fit_normal <- function(pars, y) {
  -sum(dnorm(y, pars[[1]], pars[[2]], log = TRUE))
}
# optim(pars_init_normal, fit_normal, y = tbl_data$y)
# optim(pars_init_normal, fit_normal, y = l_data_session1[[1]]$y)

fit_normal_uniform <- function(pars, y) {
  ps <- (pars[[3]] * dnorm(y, pars[[1]], pars[[2]])) + ((1 - pars[[3]]) * dunif(y, -3, 3))
  return(-sum(log(pmax(ps, 1e-10))))
}
# optim(
#   pars_init_mixture, fit_normal_uniform, y = tbl_data$y,
#   lower = c(-10, .1, 0), upper = c(10, 10, 1),
#   method = "L-BFGS-B"
# )

# fit all participants at once
pars_init_normal <- c(2, 1)
pars_normal_session1 <- map(l_data_session1, my_optim, p = pars_init_normal, f = fit_normal)
pars_normal_session2 <- map(l_data_session2, my_optim, p = pars_init_normal, f = fit_normal)

pars_init_mixture <- c(0, 1, .6)
pars_mix_session1 <- map(l_data_session1, my_optim, p = pars_init_mixture, f = fit_normal_uniform)
pars_mix_session2 <- map(l_data_session2, my_optim, p = pars_init_mixture, f = fit_normal_uniform)

tbl_normal <- tibble(
  reduce(pars_normal_session1, rbind) %>% as.data.frame() %>%
    rename(mu1 = V1, sd1 = V2) %>%
    cbind(
      reduce(pars_normal_session2, rbind) %>% as.data.frame() %>%
        rename(mu2 = V1, sd2 = V2)
      )
)
tbl_cor_normal <- tbl_normal %>% summarize(cor_mu = cor(mu1, mu2), cor_sd = cor(sd1, sd2))
tbl_cor_normal$model <- "Normal"

tbl_mix <- tibble(
  reduce(pars_mix_session1, rbind) %>% as.data.frame() %>%
    rename(mu1 = V1, sd1 = V2, p_mix1 = V3) %>%
    cbind(
      reduce(pars_mix_session2, rbind) %>% as.data.frame() %>%
        rename(mu2 = V1, sd2 = V2, p_mix2 = V3)
    )
)
tbl_both <- tbl_mix %>% dplyr::select(-starts_with("p_mix")) %>%
  mutate(model = "Mixture") %>%
  rbind(tbl_normal %>% mutate(model = "Normal"))


tbl_cor_mix <- tbl_mix %>% summarize(cor_mu = cor(mu1, mu2), cor_sd = cor(sd1, sd2))
tbl_cor_mix$model <- "Mixture"

tbl_cor_both <- rbind(tbl_cor_mix, tbl_cor_normal)


tbl_both %>% ggplot(aes(sd1, sd2, group = model)) +
  geom_smooth(method = "lm", color = "#21918c", alpha = .2) +
  geom_point(aes(color = model)) +
  geom_label(data = tbl_cor_both, aes(x = 1.35, y = .7, label = str_c("r = ", round(cor_sd, 2)))) +
  facet_wrap(~ model) +
  theme_bw() +
  scale_color_viridis_d(name = "Model") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "SD Session 1", y = "SD Session 2")
