# notes
# danwitz et al. 2022 only fit softmax temperature param, but keep var_xi and var_eps fixed to true values
# daw et al. 2006 fit kalman filter with two variances being estimated, but provide no recovery studies
# speekenbrink & konstantinidis (2015) fit kalman model also with two variances being estimated, but provide no recovery studies


rm(list = ls())

library(tidyverse)
library(rutils)
library(grid)
library(gridExtra)
library(mvtnorm)
library(zoo)
library(TTR)
library(future)
library(furrr)


home_grown <- c("exploration-R/utils/utils.R", "exploration-R/utils/plotting.R")
walk(home_grown, source)


# Generate Random Walk Data -----------------------------------------------

fit_or_load <- "fit"

mu1 <- c(-60, -20, 20, 60)
nr_trials <- 200
sigma_xi_sq <- 16
sigma_epsilon_sq <- 16
lambda <- .9836

tbl_bandits <- generate_restless_bandits(sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials)

ggplot(tbl_bandits %>% pivot_longer(-trial_id), aes(trial_id, value, group = name)) +
  geom_line(aes(color = name), size = .75) +
  scale_color_viridis_d(name = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Trial ID", y = "Reward")



# Kalman & Softmax --------------------------------------------------------


tbl_gammas <- tibble(
  gamma_mn = c(.16, .5, 1, 2),#[1:2],
  gamma_sd = c(.03, .1, .2, .3)#[1:2]
)
simulate_data <- c(TRUE, FALSE)#[1]
nr_participants <- c(200)
nr_trials <- c(300, 500)
cond_on_choices <- c(TRUE)


tbl_params_softmax <- crossing(
  tbl_gammas, simulate_data, nr_participants, nr_trials, cond_on_choices
)

if (fit_or_load == "fit")  {
  l_results_softmax <- pmap(
    tbl_params_softmax, simulate_and_fit_softmax, 
    lambda = lambda, nr_vars = 2
  )
  saveRDS(l_results_softmax, "exploration-R/data/recovery-softmax-two-variances.RDS")
} else if (fit_or_load == "load")  {
  l_results_softmax <- readRDS("exploration-R/data/recovery-softmax-two-variances.RDS")
}

counter <- 1
l_results_c <- list()
for (tbl_r in l_results_softmax) {
  l_results_c[[counter]] <- as_tibble(cbind(
    tbl_r %>% select(-c(simulate_data, nr_trials)), tbl_params_softmax[counter, ]
  ))
  counter <- counter + 1
}

tbl_cor_softmax <- reduce(l_results_c, rbind) %>%
  unnest_wider(params_decision) %>%
  group_by(gamma_mn, simulate_data, nr_participants, nr_trials) %>%
  filter(
    gamma_ml < 2.9 & sigma_xi_sq_ml < 29 & sigma_epsilon_sq_ml < 29
  ) %>%
  summarize(
    r_sigma_xi = cor(sigma_xi_sq, sigma_xi_sq_ml),
    r_sigma_epsilon = cor(sigma_epsilon_sq, sigma_epsilon_sq_ml),
    r_gamma = cor(gamma, gamma_ml)
  ) %>% ungroup()

tbl_cor_softmax_long <- tbl_cor_softmax %>% 
  mutate(
    simulate_data = factor(simulate_data),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE")
  ) %>% rename("Sigma Xi" = r_sigma_xi, "Sigma Epsilon" = r_sigma_epsilon, "Gamma" = r_gamma) %>%
  pivot_longer(cols = c(Gamma, `Sigma Xi`, `Sigma Epsilon`))

pd <- position_dodge(width = .9)
plot_cor_recovery(tbl_cor_softmax_long, pd, "softmax")


# cors between pars
f_clean_cor <- function(x) {
  x <- x[x$gamma_ml < 2.9 & x$sigma_xi_sq_ml < 29 & x$sigma_epsilon_sq_ml < 29, ]
  cor(x[, c("sigma_xi_sq_ml", "sigma_epsilon_sq_ml", "gamma_ml")])
}
l_cors_params <- map(
  l_results_c, f_clean_cor
)

counter <- 1
for (tbl_r in l_cors_params) {
  l_cors_params[[counter]] <- as_tibble(cbind(
    tbl_r, tbl_params_softmax[counter, ]
  ))
  counter = counter + 1
}

l_heatmaps_par_cor <- map(l_cors_params, plot_my_heatmap_softmax)
grid.draw(marrangeGrob(l_heatmaps_par_cor, nrow = 4, ncol = 4))




# Kalman & Softmax: Fit Only Xi Variance ----------------------------------


# take same combination of hyperparameters as before

if (fit_or_load == "fit")  {
  l_results_softmax_1var <- pmap(
    tbl_params_softmax, 
    simulate_and_fit_softmax, 
    lambda = lambda,
    nr_vars = 1
  )
  saveRDS(l_results_softmax_1var, "exploration-R/data/recovery-softmax-one-variance.RDS")
} else if (fit_or_load == "load")  {
  l_results_softmax_1var <- readRDS("exploration-R/data/recovery-softmax-one-variance.RDS")
}

counter <- 1
l_results_c_1var <- list()
for (tbl_r in l_results_softmax_1var) {
  l_results_c_1var[[counter]] <- as_tibble(cbind(
    tbl_r %>% select(-c(simulate_data, nr_trials)), tbl_params_softmax[counter, ]
  ))
  counter <- counter + 1
}

tbl_cor_softmax_1var <- reduce(l_results_c_1var, rbind) %>%
  unnest_wider(params_decision) %>%
  group_by(gamma_mn, simulate_data, nr_participants, nr_trials) %>%
  filter(
    gamma_ml < 2.9 & sigma_xi_sq_ml < 29
  ) %>%
  summarize(
    r_sigma_xi = cor(sigma_xi_sq, sigma_xi_sq_ml),
    r_gamma = cor(gamma, gamma_ml)
  ) %>% ungroup()

tbl_cor_softmax_1var_long <- tbl_cor_softmax_1var %>% 
  mutate(
    simulate_data = factor(simulate_data),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE")
  ) %>% rename("Sigma Xi" = r_sigma_xi, "Gamma" = r_gamma) %>%
  pivot_longer(cols = c(Gamma, `Sigma Xi`))

pd <- position_dodge(width = .9)
plot_cor_recovery(tbl_cor_softmax_1var_long, pd, "softmax")

# cors between pars
f_clean_cor <- function(x) {
  x <- x[x$gamma_ml < 2.9 & x$sigma_xi_sq_ml < 29, ]
  cor(x[, c("sigma_xi_sq_ml", "gamma_ml")])
}
# cors between pars
l_cors_params <- map(
  l_results_c_1var, f_clean_cor
)

counter <- 1
for (tbl_r in l_cors_params) {
  l_cors_params[[counter]] <- as_tibble(cbind(
    tbl_r, tbl_params_softmax[counter, ]
  ))
  counter = counter + 1
}


l_heatmaps_par_cor <- map(l_cors_params, plot_my_heatmap_softmax, nr_var = 1)
grid.draw(marrangeGrob(l_heatmaps_par_cor, nrow = 4, ncol = 4))





# Kalman & Softmax, but Fix Variances -------------------------------------


if (fit_or_load == "fit")  {
  l_results_softmax_0var <- pmap(
    tbl_params_softmax, 
    simulate_and_fit_softmax, 
    lambda = lambda,
    nr_vars = 0
  )
  saveRDS(l_results_softmax_0var, "exploration-R/data/recovery-softmax-no-variance.RDS")
} else if (fit_or_load == "load")  {
  l_results_softmax_0var <- readRDS("exploration-R/data/recovery-softmax-no-variance.RDS")
}

counter <- 1
l_results_c_0var <- list()
for (tbl_r in l_results_softmax_0var) {
  l_results_c_0var[[counter]] <- as_tibble(cbind(
    tbl_r %>% select(-c(simulate_data, nr_trials)), tbl_params_softmax[counter, ]
  ))
  counter <- counter + 1
}

tbl_cor_softmax_0var <- reduce(l_results_c_0var, rbind) %>%
  unnest_wider(params_decision) %>%
  group_by(gamma_mn, simulate_data, nr_participants, nr_trials) %>%
  filter(
    gamma_ml < 2.9
  ) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml)
  ) %>% ungroup()

tbl_cor_softmax_0var_long <- tbl_cor_softmax_0var %>% 
  mutate(
    simulate_data = factor(simulate_data),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE")
  ) %>% rename("Gamma" = r_gamma) %>%
  pivot_longer(cols = c(Gamma))

pd <- position_dodge(width = .9)
plot_cor_recovery(tbl_cor_softmax_0var_long, pd, "softmax")





# Kalman & Thompson: Two Variances ----------------------------------------


simulate_data <- c(TRUE, FALSE)#[1]
nr_participants <- c(200)
nr_trials <- c(300, 500)
cond_on_choices <- c(TRUE)


tbl_params_thompson <- crossing(
  simulate_data, nr_participants, nr_trials, cond_on_choices
)


if (fit_or_load == "fit")  {
  l_results_thompson <- pmap(tbl_params_thompson, simulate_and_fit_thompson, lambda = lambda, nr_vars = 2)
  saveRDS(l_results_thompson, "exploration-R/data/recovery-thompson-two-variances.RDS")
} else if (fit_or_load == "load")  {
  l_results_thompson <- readRDS("exploration-R/data/recovery-thompson-two-variances.RDS")
}

counter <- 1
l_results_c <- list()
for (tbl_r in l_results_thompson) {
  l_results_c[[counter]] <- as_tibble(cbind(
    tbl_r %>% select(-c(simulate_data, nr_trials)), tbl_params_thompson[counter, ]
  ))
  counter = counter + 1
}

tbl_cor_thompson <- reduce(l_results_c, rbind) %>%
  unnest_wider(params_decision) %>%
  filter(sigma_xi_sq_ml < 29 & sigma_epsilon_sq_ml < 29) %>%
  group_by(simulate_data, nr_participants, nr_trials) %>%
  summarize(
    r_sigma_xi = cor(sigma_xi_sq, sigma_xi_sq_ml),
    r_sigma_epsilon = cor(sigma_epsilon_sq, sigma_epsilon_sq_ml)
  ) %>% ungroup()

tbl_cor_thompson_long <- tbl_cor_thompson %>% 
  mutate(
    simulate_data = factor(simulate_data),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE")
  ) %>% rename("Sigma Xi" = r_sigma_xi, "Sigma Epsilon" = r_sigma_epsilon) %>%
  pivot_longer(cols = c(`Sigma Xi`, `Sigma Epsilon`))

pd <- position_dodge(width = 1)
plot_cor_recovery(tbl_cor_thompson_long, pd, "thompson")
#   facet_wrap(~ name) +
# possibly adopt plot_cor_recovery function, if error here


# cors between pars
f_clean_cor <- function(x) {
  x <- x[x$sigma_xi_sq_ml < 29 & x$sigma_xi_sq_ml < 29, ]
  cor(x[, c("sigma_xi_sq_ml", "sigma_epsilon_sq_ml")])
}
l_cors_params <- map(l_results_c, f_clean_cor)

counter <- 1
for (tbl_r in l_cors_params) {
  l_cors_params[[counter]] <- as_tibble(cbind(
    tbl_r, tbl_params_thompson[counter, ]
  ))
  counter = counter + 1
}


l_heatmaps_par_cor <- map(l_cors_params, plot_my_heatmap_thompson)
grid.draw(marrangeGrob(l_heatmaps_par_cor, nrow = 2, ncol = 2))




# Kalman & Thompson Fit Xi Variance ---------------------------------------



if (fit_or_load == "fit")  {
  l_results_thompson_1var <- pmap(
    tbl_params_thompson, simulate_and_fit_thompson, 
    lambda = lambda, nr_vars = 1
  )
  saveRDS(l_results_thompson_1var, "exploration-R/data/recovery-thompson-one-variance.RDS")
} else if (fit_or_load == "load")  {
  l_results_thompson_1var <- readRDS("exploration-R/data/recovery-thompson-one-variance.RDS")
}

counter <- 1
l_results_c <- list()
for (tbl_r in l_results_thompson_1var) {
  l_results_c[[counter]] <- as_tibble(cbind(
    tbl_r %>% select(-c(simulate_data, nr_trials)), tbl_params_thompson[counter, ]
  ))
  counter = counter + 1
}

tbl_cor_thompson_1var <- reduce(l_results_c, rbind) %>%
  unnest_wider(params_decision) %>%
  filter(sigma_xi_sq_ml < 29) %>%
  group_by(simulate_data, nr_participants, nr_trials) %>%
  summarize(
    r_sigma_xi = cor(sigma_xi_sq, sigma_xi_sq_ml),
  ) %>% ungroup()

tbl_cor_thompson_long_1var <- tbl_cor_thompson_1var %>% 
  mutate(
    simulate_data = factor(simulate_data),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE")
  ) %>% rename("Sigma Xi" = r_sigma_xi) %>%
  pivot_longer(cols = c(`Sigma Xi`))

pd <- position_dodge(width = 1)
plot_cor_recovery(tbl_cor_thompson_long_1var, pd, "thompson")
#   facet_wrap(~ name) +
# possibly adopt plot_cor_recovery function, if error here


# Kalman & UCB ------------------------------------------------------------



tbl_gammas <- tibble(
  gamma_mn = c(.16, .5, 1, 2),#[1:2],
  gamma_sd = c(.03, .1, .2, .3)#[1:2]
)
tbl_betas <- tibble(
  beta_mn = c(.17, .5, 8),
  beta_sd = c(.05, .1, .5)
)
simulate_data <- c(TRUE, FALSE)#[1]
nr_participants <- c(200)
nr_trials <- c(300, 500)
cond_on_choices <- c(TRUE)


tbl_params_ucb <- crossing(
  tbl_gammas, tbl_betas, simulate_data, nr_participants, nr_trials, cond_on_choices
)


if (fit_or_load == "fit")  {
  l_results_ucb_0var <- pmap(
    tbl_params_ucb, simulate_and_fit_ucb,
    lambda = lambda, nr_vars = 0
  )
  saveRDS(l_results_ucb_0var, "exploration-R/data/recovery-ucb-no-variance.RDS")
} else if (fit_or_load == "load")  {
  l_results_ucb_0var <- readRDS("exploration-R/data/recovery-ucb-no-variance.RDS")
}

counter <- 1
l_results_c_0var <- list()
for (tbl_r in l_results_ucb_0var) {
  l_results_c_0var[[counter]] <- as_tibble(cbind(
    tbl_r %>% 
      unnest_wider(params_decision) %>%
      select(-c(simulate_data, nr_trials)), tbl_params_ucb[counter, ]
  ))
  counter <- counter + 1
}

tbl_cor_c_0var <- reduce(l_results_c_0var, rbind) %>%
  group_by(gamma_mn, beta_mn, simulate_data, nr_trials) %>%
  filter(
    gamma_ml < 2.9 |
      beta_ml < 2.9
  ) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_beta = cor(beta, beta_ml)
  ) %>% ungroup()

tbl_cor_ucb_0var_long <- tbl_cor_c_0var %>% 
  mutate(
    simulate_data = factor(simulate_data),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE")
  ) %>% 
  rename(
    "Gamma" = r_gamma,
    "Beta" = r_beta
  ) %>%
  pivot_longer(cols = c(Gamma, Beta))

pd <- position_dodge(width = .9)
plot_cor_recovery(tbl_cor_ucb_0var_long, pd, "ucb")




# Delta & Softmax ---------------------------------------------------------


tbl_gammas <- tibble(
  gamma_mn = c(.16, .5, 1),
  gamma_sd = c(.03, .1, .2)
)
tbl_deltas <- tibble(
  delta_mn = c(.55, .9),
  delta_sd = c(.05, .03)
)
simulate_data <- c(TRUE, FALSE)
nr_participants <- c(200)
nr_trials <- c(300, 500)
cond_on_choices <- c(TRUE)
is_decay <- c(FALSE, TRUE)

tbl_params_delta <- crossing(
  tbl_gammas, tbl_deltas, simulate_data, nr_participants, 
  nr_trials, cond_on_choices, is_decay
)


if (fit_or_load == "fit")  {
  l_results_delta_softmax <- pmap(
    tbl_params_delta, safely(simulate_and_fit_delta),
    lambda = lambda
  )
  saveRDS(l_results_delta_softmax, "exploration-R/data/recovery-delta-softmax.RDS")
} else if (fit_or_load == "load")  {
  l_results_delta_softmax <- readRDS("exploration-R/data/recovery-delta-softmax.RDS")
}


counter <- 1
l_results_c_delta_softmax <- list()
for (tbl_r in l_results_delta_softmax) {
  l_results_c_delta_softmax[[counter]] <- as_tibble(cbind(
    tbl_r %>% 
      unnest_wider(params_decision) %>%
      select(-c(simulate_data, nr_trials, is_decay)), tbl_params_delta[counter, ]
  ))
  counter <- counter + 1
}

tbl_cor_c_delta_softmax <- reduce(l_results_c_delta_softmax, rbind) %>%
  group_by(delta_mn, gamma_mn, simulate_data, nr_trials, is_decay) %>%
  filter(
    gamma_ml < 2.9 |
      delta_ml < 0.99
  ) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_delta = cor(delta, delta_ml)
  ) %>% ungroup()

tbl_cor_delta_softmax_long <- tbl_cor_c_delta_softmax %>% 
  mutate(
    simulate_data = factor(simulate_data),
    is_decay = factor(is_decay),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE"),
    is_decay = fct_recode(is_decay, "Decay Rule" = "TRUE", "Delta Rule" = "FALSE")
  ) %>% 
  rename(
    "Gamma" = r_gamma,
    "Delta" = r_delta
  ) %>%
  pivot_longer(cols = c(Gamma, Delta))

pd <- position_dodge(width = .9)
plot_cor_recovery(tbl_cor_delta_softmax_long, pd, "softmax") +
  facet_grid(name ~ is_decay)



