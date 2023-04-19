rm(list = ls())



# Import Packages and Load Data -------------------------------------------


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

tbl_rb <- read_csv(
  "https://github.com/speekenbrink-lab/data/raw/master/Speekenbrink_Konstantinidis_2015.csv"
)
tbl_rb <- tbl_rb %>% mutate(
  trend = factor(startsWith(cond, "t"), labels = c("Trend", "No Trend")),
  volatility = factor(endsWith(cond, "n"), labels = c("Variance Changes", "Variance Stable"))
) %>%
  rename(rewards = payoff, choices = deck)

tbl_rb %>% count(id2)
tbl_rb %>% group_by(id2) %>% count(choices) %>% arrange(desc(n))
# these two participants have likely not understood the task instructions
tbl_rb <- tbl_rb %>% filter(!(id2 %in% c(1, 43)))

l_participants <- tbl_rb %>% split(., .$"id2")

tbl_rewards <- tibble(NA)


# generate randomly walking arms
mu1 <- c(-60, -20, 20, 60)
nr_trials <- nrow(l_participants[[1]])
nr_participants <- length(l_participants)
sigma_xi_sq <- 16
sigma_epsilon_sq <- 16
lambda <- .9836

tbl_rewards <- generate_restless_bandits(
  sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials
) %>% 
  select(-trial_id)


my_participants_tbl_kalman <- function(l_params_decision) {
  tibble(
    sigma_prior = 1000,
    mu_prior = 0,
    sigma_xi_sq = 16,
    sigma_epsilon_sq = 16,
    lambda = lambda,
    nr_trials = nr_trials,
    params_decision = l_params_decision,
    simulate_data = TRUE,
    seed = round(rnorm(nr_participants, 100000, 1000))
  )
}

my_participants_tbl_delta <- function(l_params_decision, delta) {
  tibble(
    delta = delta,
    lambda = lambda,
    nr_trials = nr_trials,
    params_decision = l_params_decision,
    simulate_data = TRUE,
    seed = round(rnorm(nr_participants, 100000, 1000))
  )
}



# Fit, Simulate, & Recover Parameters -------------------------------------





## Softmax no variance ----------------------------------------------------



l_kalman_softmax_no_variance <- furrr::future_map(
  l_participants, fit_softmax_no_variance_wrapper, 
  tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE,
  .progress = TRUE
)

tbl_kalman_softmax_no_variance <- reduce(l_kalman_softmax_no_variance, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(gamma = V1, ll = V2)

l_params_decision <- map(
  tbl_kalman_softmax_no_variance$gamma, 
  ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
)

tbl_participants_kalman_softmax <- tbl_participants_delta_kalman(l_params_decision)
tbl_results_kalman_softmax <- simulate_and_fit_softmax(tbl_participants_kalman_softmax, nr_vars = 0)





## UCB with Softmax -------------------------------------------------------



plan(multisession, workers = availableCores() - 2)
l_kalman_ucb_no_variance <- furrr:::future_map(
  l_participants, fit_ucb_no_variance_wrapper,
  tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE,
  .progress = TRUE
)
tbl_kalman_ucb_no_variance <- reduce(l_kalman_ucb_no_variance, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(gamma = V1, beta = V2, ll = V3)

l_params_decision <- map2(
  tbl_kalman_ucb_no_variance$gamma, tbl_kalman_ucb_no_variance$beta,
  ~ list(gamma = ..1, beta = ..2, choicemodel = "ucb", no = 4)
)

tbl_participants_kalman_ucb <- tbl_participants_delta_kalman(l_params_decision)
tbl_results_kalman_softmax <- simulate_and_fit_softmax(tbl_participants_kalman_ucb, nr_vars = 0)




## Thompson Sampling (Xi Variance) ----------------------------------------



plan(multisession, workers = availableCores() - 2)
l_kalman_thompson_one_variance <- furrr::future_map(
  l_participants, fit_thompson_one_variance_wrapper,
  tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE,
  .progress = TRUE
)
tbl_kalman_thompson_one_variance <- reduce(l_kalman_thompson_one_variance, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(xi_innovation = V1, ll = V3)

l_params_decision <- map2(
  tbl_kalman_thompson_one_variance$xi_innovation,
  ~ list(xi_eta_sq = ..1, choicemodel = "thompson", no = 4)
)

tbl_participants_kalman_thompson <- tbl_participants_delta_kalman(l_params_decision)
tbl_results_kalman_softmax <- simulate_and_fit_softmax(tbl_participants_kalman_thompson, nr_vars = 1)



## Delta Rule -------------------------------------------------------------



plan(multisession, workers = availableCores() - 2)
l_delta_softmax <- furrr::future_map(
  l_participants, fit_delta_softmax_wrapper,
  tbl_rewards = tbl_rewards, is_decay = FALSE, condition_on_observed_choices = TRUE,
  .progress = TRUE
)
tbl_delta_softmax <- reduce(l_delta_softmax, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(delta = V1, gamma = V2, ll = V3)

l_params_decision <- map(
  tbl_delta_softmax$gamma,
  ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
)

tbl_participants_delta <- my_participants_tbl_delta(l_params_decision, tbl_delta_softmax$delta)
tbl_results_delta_softmax <- simulate_and_fit_delta(tbl_participants_delta, is_decay = FALSE)




## Decay Rule -------------------------------------------------------------



plan(multisession, workers = availableCores() - 2)
l_decay_softmax <- furrr::future_map(
  l_participants, fit_delta_softmax_wrapper,
  tbl_rewards = tbl_rewards, is_decay = TRUE, condition_on_observed_choices = TRUE,
  .progress = TRUE
)
tbl_decay_softmax <- reduce(l_decay_softmax, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(delta = V1, gamma = V2, ll = V3)

l_params_decision <- map(
  tbl_decay_softmax$gamma,
  ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
)

tbl_participants_decay <- my_participants_tbl_delta(l_params_decision, tbl_delta_softmax$delta)
tbl_results_delta_softmax <- simulate_and_fit_delta(tbl_participants_decay, is_decay = TRUE)

