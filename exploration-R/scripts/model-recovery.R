
# Set Up ------------------------------------------------------------------



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


fit_or_load <- "fit"


# Soft Max ----------------------------------------------------------------


tbl_gammas <- tibble(
  gamma_mn = c(.05),
  gamma_sd = c(.025)
)
simulate_data <- c(TRUE)#[1]
nr_participants <- c(200)
nr_trials <- c(400)
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

tbl_results_softmax <- cbind(tbl_params_softmax, map(l_model_recovery_softmax, ~ cbind(
  summarize_model_recovery(.x$tbl_lls, "aic") %>% pivot_wider(names_from = model, values_from = n),
  summarize_model_recovery(.x$tbl_lls, "bic") %>% pivot_wider(names_from = model, values_from = n)
)) %>% reduce(rbind))

tbl_table_softmax <- tbl_results_softmax %>% 
  mutate(
    aic_total = aic_softmax + aic_thompson + aic_ucb,
    bic_total = bic_softmax + bic_thompson + bic_ucb,
    prop_aic = aic_softmax / aic_total,
    prop_bic = bic_softmax / bic_total
  ) %>%
  select(-c(starts_with("aic"), starts_with("bic"), cond_on_choices))
names(tbl_table_softmax) <- c(
  "Gamma Mean", "Gamma SD", "Sim. by Participant", "Nr. Participants", 
  "Nr. Trials", "Prop. AIC", "Prop. BIC"
)

badtogood_cols <- c('#d65440', '#ffffff', "forestgreen")
# 
# reactable(
#   tbl_table_softmax,
#   defaultColDef = colDef(
#     minWidth = 150,
#     align = "center",
#     cell = color_tiles(tbl_table_softmax, span = 6:7, colors = badtogood_cols),
#   )
# )



# Thompson ----------------------------------------------------------------


simulate_data <- c(TRUE)#[1]
nr_participants <- c(200)
nr_trials <- c(400)
cond_on_choices <- c(TRUE)


tbl_params_thompson <- crossing(
  simulate_data, nr_participants, nr_trials, cond_on_choices
)

if (fit_or_load == "fit")  {
  l_model_recovery_thompson <- pmap(
    tbl_params_thompson, recover_thompson,
    lambda = lambda, nr_vars = 1
  )
  saveRDS(l_model_recovery_thompson, "exploration-R/data/model-recovery-thompson.RDS")
} else if (fit_or_load == "load")  {
  l_model_recovery_thompson <- readRDS("exploration-R/data/model-recovery-thompson.RDS")
}


tbl_results_thompson <- cbind(tbl_params_thompson, map(l_model_recovery_thompson, ~ cbind(
  summarize_model_recovery(.x$tbl_lls, "aic") %>% pivot_wider(names_from = model, values_from = n),
  summarize_model_recovery(.x$tbl_lls, "bic") %>% pivot_wider(names_from = model, values_from = n)
)) %>% reduce(rbind))

tbl_table_thompson <- tbl_results_thompson %>% 
  mutate(
    aic_total = aic_softmax + aic_thompson + aic_ucb,
    bic_total = bic_softmax + bic_thompson + bic_ucb,
    prop_aic = aic_thompson / aic_total,
    prop_bic = bic_thompson / bic_total
  ) %>%
  select(-c(starts_with("aic"), starts_with("bic"), cond_on_choices))
names(tbl_table_thompson) <- c(
  "Sim. by Participant", "Nr. Participants", 
  "Nr. Trials", "Prop. AIC", "Prop. BIC"
)
# 
# 
# reactable(
#   tbl_table_thompson,
#   defaultColDef = colDef(
#     minWidth = 150,
#     align = "center",
#     cell = color_tiles(tbl_table_thompson, span = 4:5, colors = badtogood_cols),
#   )
# )


# UCB ---------------------------------------------------------------------


tbl_gammas <- tibble(
  gamma_mn = c(.05),
  gamma_sd = c(.025)
)
tbl_betas <- tibble(
  beta_mn = c(.05),
  beta_sd = c(.025)
)
simulate_data <- c(TRUE)
nr_participants <- c(200)
nr_trials <- c(400)
cond_on_choices <- c(TRUE)

tbl_params_ucb <- crossing(
  tbl_gammas, tbl_betas,
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


tbl_results_ucb <- cbind(tbl_params_ucb, map(l_model_recovery_ucb, ~ cbind(
  summarize_model_recovery(.x$tbl_lls, "aic") %>% pivot_wider(names_from = model, values_from = n),
  summarize_model_recovery(.x$tbl_lls, "bic") %>% pivot_wider(names_from = model, values_from = n)
)) %>% reduce(rbind))

tbl_table_ucb <- tbl_results_ucb %>% 
  mutate(
    aic_total = aic_softmax + aic_thompson + aic_ucb,
    bic_total = bic_softmax + bic_thompson + bic_ucb,
    prop_aic = aic_ucb / aic_total,
    prop_bic = bic_ucb / bic_total
  ) %>%
  select(-c(starts_with("aic"), starts_with("bic"), cond_on_choices))
names(tbl_table_ucb) <- c(
  "Gamma Mean", "Gamma SD", "Beta Mean", "Beta SD", 
  "Sim. by Participant", "Nr. Participants", 
  "Nr. Trials", "Prop. AIC", "Prop. BIC"
)

# 
# reactable(
#   tbl_table_ucb,
#   defaultPageSize = 24,
#   defaultColDef = colDef(
#     minWidth = 150,
#     align = "center",
#     cell = color_tiles(tbl_table_ucb, span = 8:9, colors = badtogood_cols),
#   )
# )




# Mixture ------------------------------------------------------------------



tbl_gammas <- tibble(
  gamma_mn = c(.05),
  gamma_sd = c(.025)
)
tbl_betas <- tibble(
  beta_mn = c(.05),
  beta_sd = c(.025)
)
tbl_w_mix <- tibble(
  w_mix_mn = c(.85),
  w_mix_sd = c(.07)
)

simulate_data <- c(TRUE)
nr_participants <- c(200)
nr_trials <- c(400)
cond_on_choices <- c(TRUE)
mixturetype <- c("ucb_thompson", "ru_thompson")


tbl_params_mixture <- crossing(
  tbl_gammas, tbl_betas, tbl_w_mix,
  simulate_data, nr_participants, nr_trials, cond_on_choices, mixturetype
)

if (fit_or_load == "fit")  {
  l_model_recovery_mixture <- pmap(
    tbl_params_mixture, recover_mixture,
    lambda = lambda, nr_vars = 0
  )
  saveRDS(l_model_recovery_mixture, "exploration-R/data/model-recovery-mixture.RDS")
} else if (fit_or_load == "load")  {
  l_model_recovery_mixture <- readRDS("exploration-R/data/model-recovery-mixture.RDS")
}


tbl_results_mixture <- cbind(tbl_params_mixture, map(l_model_recovery_mixture, ~ cbind(
  summarize_model_recovery(.x$tbl_lls, "aic") %>% pivot_wider(names_from = model, values_from = n),
  summarize_model_recovery(.x$tbl_lls, "bic") %>% pivot_wider(names_from = model, values_from = n)
)) %>% reduce(rbind))

tbl_table_mixture <- tbl_results_mixture %>% 
  mutate(
    aic_total = aic_softmax + aic_thompson + aic_ucb,
    bic_total = bic_softmax + bic_thompson + bic_ucb,
    prop_aic = aic_ucb / aic_total,
    prop_bic = bic_ucb / bic_total
  ) %>%
  select(-c(starts_with("aic"), starts_with("bic"), cond_on_choices))
names(tbl_table_mixture) <- c(
  "Gamma Mean", "Gamma SD", "Beta Mean", "Beta SD", 
  "w Thompson Mean", "w Thompson SD",
  "Sim. by Participant", "Nr. Participants", 
  "Nr. Trials", "Prop. AIC", "Prop. BIC"
)

# 
# reactable(
#   tbl_table_mixture,
#   defaultPageSize = 24,
#   defaultColDef = colDef(
#     minWidth = 150,
#     align = "center",
#     cell = color_tiles(tbl_table_mixture, span = 8:9, colors = badtogood_cols),
#   )
# )





# Delta & Decay -----------------------------------------------------------



tbl_gammas <- tibble(
  gamma_mn = c(.08, .16),#[1],
  gamma_sd = c(.05, .1)#[1]
)
tbl_deltas <- tibble(
  delta_mn = c(.55),#[1],
  delta_sd = c(.2)#[1]
)
simulate_data <- c(TRUE)
nr_participants <- c(200)
nr_trials <- c(400)
cond_on_choices <- c(TRUE)
is_decay <- c(FALSE, TRUE)

tbl_params_delta <- crossing(
  tbl_gammas, tbl_deltas, simulate_data, nr_participants, 
  nr_trials, cond_on_choices, is_decay
)


if (fit_or_load == "fit")  {
  l_model_recovery_delta <- pmap(
    tbl_params_delta, recover_delta, lambda = lambda
  )
  saveRDS(l_model_recovery_delta, "exploration-R/data/model-recovery-delta.RDS")
} else if (fit_or_load == "load")  {
  l_model_recovery_delta <- readRDS("exploration-R/data/model-recovery-delta.RDS")
}


tbl_results_delta <- cbind(tbl_params_delta, map(l_model_recovery_delta, ~ cbind(
  summarize_model_recovery(.x$tbl_lls, "aic") %>% pivot_wider(names_from = model, values_from = n),
  summarize_model_recovery(.x$tbl_lls, "bic") %>% pivot_wider(names_from = model, values_from = n)
)) %>% reduce(rbind))

tbl_table_delta <- tbl_results_delta %>% 
  mutate(
    aic_total = aic_softmax + aic_thompson + aic_ucb,
    bic_total = bic_softmax + bic_thompson + bic_ucb,
    prop_aic = aic_ucb / aic_total,
    prop_bic = bic_ucb / bic_total
  ) %>%
  select(-c(starts_with("aic"), starts_with("bic"), cond_on_choices))
names(tbl_table_delta) <- c(
  "Gamma Mean", "Gamma SD", "Delta Mean", "Delta SD", 
  "Sim. by Participant", "Nr. Participants", 
  "Nr. Trials", "Prop. AIC", "Prop. BIC"
)
# 
# 
# reactable(
#   tbl_table_delta,
#   defaultPageSize = 24,
#   defaultColDef = colDef(
#     minWidth = 150,
#     align = "center",
#     cell = color_tiles(tbl_table_delta, span = 8:9, colors = badtogood_cols),
#   )
# )
# 
