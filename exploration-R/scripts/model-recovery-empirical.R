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
library(formattable)



home_grown <- c("exploration-R/utils/utils.R", "exploration-R/utils/plotting.R")
walk(home_grown, source)

mu1 <- c(-60, -20, 20, 60)
nr_trials <- 200
sigma_xi_sq <- 16
sigma_epsilon_sq <- 16
lambda <- .9836
cond_on_choices <- TRUE

tbl_rewards <- generate_restless_bandits(
  sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials
) %>% 
  select(-trial_id)


my_participants_tbl_kalman <- function(l_params_decision, sim_d) {
  tibble(
    sigma_prior = 1000,
    mu_prior = 0,
    sigma_xi_sq = 16,
    sigma_epsilon_sq = 16,
    lambda = lambda,
    nr_trials = nr_trials,
    params_decision = l_params_decision,
    simulate_data = sim_d,
    seed = round(rnorm(length(l_params_decision), 100000, 1000))
  )
}

my_participants_tbl_delta <- function(l_params_decision, delta, sim_d) {
  tibble(
    delta = delta,
    lambda = lambda,
    nr_trials = nr_trials,
    params_decision = l_params_decision,
    simulate_data = sim_d,
    seed = round(rnorm(length(l_params_decision), 100000, 1000))
  )
}



# Softmax & Kalman --------------------------------------------------------


l_kalman_softmax_no_variance <- readRDS(
  file = "exploration-R/data/empirical-parameter-recovery-kalman-softmax-fit.rds"
)
tbl_kalman_softmax_no_variance <- reduce(l_kalman_softmax_no_variance, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(gamma = V1, ll = V2)

l_params_decision <- map(
  tbl_kalman_softmax_no_variance$gamma, 
  ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
)
nr_participants <- length(l_params_decision)

tbl_participants_kalman_softmax <- my_participants_tbl_kalman(l_params_decision, TRUE)
l_models_fit_softmax_kalman <- simulate_and_fit_models(
  tbl_participants_kalman_softmax, tbl_rewards, cond_on_choices, family = "kalman"
)

l_goodness_softmax_kalman <- read_out_lls_and_ics(l_models_fit_softmax_kalman, nr_participants)




# Thompson ----------------------------------------------------------------


l_kalman_thompson_one_variance <- readRDS(
  file = "exploration-R/data/empirical-parameter-recovery-kalman-thompson-one-variance-fit.rds"
)
tbl_kalman_thompson_one_variance <- reduce(l_kalman_thompson_one_variance, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(xi_innovation = V1, ll = V3)

l_params_decision <- map2(
  tbl_kalman_thompson_one_variance$xi_innovation,
  ~ list(xi_eta_sq = ..1, choicemodel = "thompson", no = 4)
)

tbl_participants_kalman_thompson_fix <- my_participants_tbl_kalman(l_params_decision, TRUE)
l_models_fit_kalman_thompson <- simulate_and_fit_models(
  tbl_participants_kalman_thompson_fix, tbl_rewards, cond_on_choices, family = "kalman"
)

l_goodness_kalman_thompson <- read_out_lls_and_ics(l_models_fit_kalman_thompson, nr_participants)




# UCB ---------------------------------------------------------------------


l_kalman_ucb_no_variance <- readRDS(file = "exploration-R/data/empirical-parameter-recovery-kalman-ucb-fit.rds")
tbl_kalman_ucb_no_variance <- reduce(l_kalman_ucb_no_variance, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(gamma = V1, beta = V2, ll = V3)

l_params_decision <- map2(
  tbl_kalman_ucb_no_variance$gamma, tbl_kalman_ucb_no_variance$beta,
  ~ list(gamma = ..1, beta = ..2, choicemodel = "ucb", no = 4)
)

tbl_participants_kalman_ucb <- my_participants_tbl_kalman(l_params_decision, TRUE)
l_models_fit_kalman_ucb <- simulate_and_fit_models(
  tbl_participants_kalman_ucb, tbl_rewards, cond_on_choices, family = "kalman"
)

l_goodness_kalman_ucb <- read_out_lls_and_ics(l_models_fit_kalman_ucb, nr_participants)





# RU & Thompson -----------------------------------------------------------


l_kalman_ru_thompson_no_variance <- readRDS(
  file = "exploration-R/data/empirical-parameter-recovery-kalman-ru_thompson-fit.rds"
)
tbl_kalman_ru_thompson_no_variance <- reduce(l_kalman_ru_thompson_no_variance, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(gamma = V1, beta = V2, w_mix = V3, ll = V4)

l_params_decision <- pmap(
  list(
    tbl_kalman_ru_thompson_no_variance$gamma, 
    tbl_kalman_ru_thompson_no_variance$beta,
    tbl_kalman_ru_thompson_no_variance$w_mix
  ),
  ~ list(gamma = ..1, beta = ..2, w_mix = ..3, choicemodel = "ru_thompson", no = 4)
)

tbl_participants_kalman_ru_thompson <- my_participants_tbl_kalman(l_params_decision, TRUE)
l_models_fit_kalman_ru_thompson <- simulate_and_fit_models(
  tbl_participants_kalman_ru_thompson, tbl_rewards, cond_on_choices, family = "kalman"
)

l_goodness_kalman_ru_thompson <- read_out_lls_and_ics(l_models_fit_kalman_ru_thompson, nr_participants)




# Delta -------------------------------------------------------------------


l_delta_softmax <- readRDS(file = "exploration-R/data/empirical-parameter-recovery-delta-softmax-fit.rds")
tbl_delta_softmax <- reduce(l_delta_softmax, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(delta = V1, gamma = V2, ll = V3)

l_params_decision <- map(
  tbl_delta_softmax$gamma,
  ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
)

tbl_participants_delta <- my_participants_tbl_delta(l_params_decision, tbl_delta_softmax$delta, TRUE)
l_models_fit_delta <- simulate_and_fit_models(
  tbl_participants_delta, tbl_rewards, cond_on_choices, family = "delta", is_decay = FALSE
)

l_goodness_delta <- read_out_lls_and_ics(l_models_fit_delta, nr_participants)




# Decay -------------------------------------------------------------------


l_decay_softmax <- readRDS(
  file = "exploration-R/data/empirical-parameter-recovery-decay-softmax-fit.rds"
)

tbl_decay_softmax <- reduce(l_decay_softmax, rbind) %>%
  as.data.frame() %>% as_tibble() %>% rename(delta = V1, gamma = V2, ll = V3)

l_params_decision <- map(
  tbl_decay_softmax$gamma,
  ~ list(gamma = ..1, choicemodel = "softmax", no = 4)
)

tbl_participants_decay <- my_participants_tbl_delta(l_params_decision, tbl_decay_softmax$delta, TRUE)
l_models_fit_decay <- simulate_and_fit_models(
  tbl_participants_decay, tbl_rewards, cond_on_choices, family = "delta", is_decay = TRUE
)

l_goodness_decay <- read_out_lls_and_ics(l_models_fit_decay, nr_participants)



my_ic_comparison <- function(ic){
  if (ic == "bic") nm <- "tbl_recovered_bic"
  else if (ic == "aic") nm <- "tbl_recovered_aic"
  tbl_ic <- l_goodness_softmax_kalman[[nm]] %>%
    mutate(model_in = "Kalman & Softmax") %>%
    rbind(
      l_goodness_kalman_thompson[[nm]] %>%
        mutate(model_in = "Kalman &\nThompson")
    ) %>%
    rbind(
      l_goodness_kalman_ucb[[nm]] %>%
        mutate(model_in = "Kalman &\nUCB")
    ) %>%
    rbind(
      l_goodness_kalman_ru_thompson[[nm]] %>%
        mutate(model_in = "Kalman & RU\nand Thompson")
    ) %>%
    rbind(
      l_goodness_delta[[nm]] %>%
        mutate(model_in = "Delta &\nSoftmax")
    ) %>%
    rbind(
      l_goodness_decay[[nm]] %>%
        mutate(model_in = "Decay &\nSoftmax")
    )
  tbl_ic$model <- factor(tbl_ic$model)
  levels(tbl_ic$model) <- c(
    "Decay &\nSoftmax", "Delta &\nSoftmax", "Kalman & RU\nand Thompson",
    "Kalman &\nSoftmax", "Kalman &\nThompson", "Kalman &\nUCB"
  )
  tbl_ic$model_in <- factor(tbl_ic$model_in)
  levels(tbl_ic$model_in) <- c(
    "Decay &\nSoftmax", "Delta &\nSoftmax", "Kalman & RU\nand Thompson",
    "Kalman &\nSoftmax", "Kalman &\nThompson", "Kalman &\nUCB"
  )
  tbl_ic %>% rename(model_out = model)
}

plot_my_ic <- function(my_tbl) {
  ggplot(my_tbl, aes(model_in, model_out)) +
  geom_tile(aes(fill = n)) + 
  geom_label(aes(label = n)) +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Model In", y = "Model Out") +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_viridis_c(guide = "none")
}


tbl_bic <- my_ic_comparison("bic")
tbl_aic <- my_ic_comparison("aic")

pl_bic <- plot_my_ic(tbl_bic) + ggtitle("BIC")
pl_aic <- plot_my_ic(tbl_aic) + ggtitle("AIC")

pl_bic_aic <- arrangeGrob(pl_bic, pl_aic, nrow = 1)
save_my_pdf_and_tiff(pl_bic_aic, "figures/model-recovery-empirical", 12, 5)



