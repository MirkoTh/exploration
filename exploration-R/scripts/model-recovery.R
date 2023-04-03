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


fit_or_load <- "fit"


# Soft Max ----------------------------------------------------------------


tbl_gammas <- tibble(
  gamma_mn = c(.16),
  gamma_sd = c(.03)
)
simulate_data <- c(TRUE)
nr_participants <- c(50)
nr_trials <- c(50)
cond_on_choices <- c(TRUE)

tbl_params_softmax <- crossing(
  tbl_gammas, simulate_data, nr_participants, nr_trials, cond_on_choices
)

if (fit_or_load == "fit")  {
  l_model_recovery_softmax <- pmap(
    tbl_params_softmax, recover_softmax,
    lambda = lambda, nr_vars = 0
  )
  saveRDS(l_model_recovery_softmax, "exploration-R/data/model-recovery-softmax.RDS")
} else if (fit_or_load == "load")  {
  l_model_recovery_softmax <- readRDS("exploration-R/data/model-recovery-softmax.RDS")
}




# Thompson ----------------------------------------------------------------


simulate_data <- c(TRUE, FALSE)#[1]
nr_participants <- c(200)
nr_trials <- c(300, 500)
cond_on_choices <- c(TRUE)


tbl_params_thompson <- crossing(
  simulate_data, nr_participants, nr_trials, cond_on_choices
)

if (fit_or_load == "fit")  {
  l_model_recovery_thompson <- pmap(
    tbl_params_thompson, recover_thompson,
    lambda = lambda, nr_vars = 0
  )
  saveRDS(l_model_recovery_thompson, "exploration-R/data/model-recovery-thompson.RDS")
} else if (fit_or_load == "load")  {
  l_model_recovery_thompson <- readRDS("exploration-R/data/model-recovery-thompson.RDS")
}



# UCB ---------------------------------------------------------------------


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
  simulate_data, nr_participants, nr_trials, cond_on_choices
)

if (fit_or_load == "fit")  {
  l_model_recovery_ucb <- pmap(
    tbl_params_ucb, recover_ucb,
    lambda = lambda, nr_vars = 0
  )
  saveRDS(l_model_recovery_ucb, "exploration-R/data/model-recovery-ucb.RDS")
} else if (fit_or_load == "load")  {
  l_model_recovery_ucb <- readRDS("exploration-R/data/model-recovery-ucb.RDS")
}

