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

# simulate rewards of four armed restless bandit according to random walk
# 1.
# simulate choices according to soft max model with 0 variances
# fit these choices with
# soft max 0 variances
# thompson 1 variance?
# ucb 0 variances


mu1 <- c(-60, -20, 20, 60)
nr_trials <- 200
sigma_xi_sq <- 16
sigma_epsilon_sq <- 16
lambda <- .9836


tbl_gammas <- tibble(
  gamma_mn = c(.16),
  gamma_sd = c(.03)
)
simulate_data <- c(TRUE)
nr_participants <- c(50)
nr_trials <- c(50)
cond_on_choices <- c(TRUE)


# simulate according to soft max
# fit with all three models
tbl_params_softmax <- crossing(
  tbl_gammas, simulate_data, nr_participants, nr_trials, cond_on_choices
)


list2env(tbl_params_softmax[1, ], rlang::current_env())

# simulate from softmax with inverse temperature only
recover_softmax(
  gamma_mn, gamma_sd, simulate_data, nr_participants,
  nr_trials, cond_on_choices, lambda, nr_vars = 0
)
# simulate from thompson, estimate innovation variance
recover_thompson(
  gamma_mn, gamma_sd, simulate_data, nr_participants,
  nr_trials, cond_on_choices, lambda, nr_vars = 1
)


recover_ucb(
  gamma_mn, gamma_sd, beta_mn, beta_sd, simulate_data, nr_participants,
  nr_trials, cond_on_choices, lambda, nr_vars = 0
)
# write functions
# 1. fit three models
# 2. summarize recovery results
