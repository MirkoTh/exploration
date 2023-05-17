
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


fit_or_load <- "load"


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
    aic_total = aic_softmax + aic_thompson + aic_ucb + aic_ucb_thompson + aic_ru_thompson + aic_delta + aic_decay,
    bic_total = bic_softmax + bic_thompson + bic_ucb + bic_ucb_thompson + bic_ru_thompson + bic_delta + bic_decay,
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

tbl_results_softmax <- tbl_results_softmax %>%
  pivot_longer(c(starts_with("aic"), starts_with("bic"))) %>%
  mutate(model_in = "Kalman & Softmax") %>%
  rename(model_out = name)

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
    aic_total = aic_softmax + aic_thompson + aic_ucb + aic_ucb_thompson + aic_ru_thompson + aic_delta + aic_decay,
    bic_total = bic_softmax + bic_thompson + bic_ucb + bic_ucb_thompson + bic_ru_thompson + bic_delta + bic_decay,
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
tbl_results_thompson <- tbl_results_thompson %>%
  pivot_longer(c(starts_with("aic"), starts_with("bic"))) %>%
  mutate(model_in = "Kalman & Thompson") %>%
  rename(model_out = name)

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
    aic_total = aic_softmax + aic_thompson + aic_ucb + aic_ucb_thompson + aic_ru_thompson + aic_delta + aic_decay,
    bic_total = bic_softmax + bic_thompson + bic_ucb + bic_ucb_thompson + bic_ru_thompson + bic_delta + bic_decay,
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

tbl_results_ucb <- tbl_results_ucb %>%
  pivot_longer(c(starts_with("aic"), starts_with("bic"))) %>%
  mutate(model_in = "Kalman & UCB") %>%
  rename(model_out = name)

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
    aic_total = aic_softmax + aic_thompson + aic_ucb + aic_ucb_thompson + aic_ru_thompson + aic_delta + aic_decay,
    bic_total = bic_softmax + bic_thompson + bic_ucb + bic_ucb_thompson + bic_ru_thompson + bic_delta + bic_decay,
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

tbl_results_mixture <- tbl_results_mixture %>%
  pivot_longer(c(starts_with("aic"), starts_with("bic"))) %>%
  mutate(
    model_in = fct_inorder(factor(mixturetype)),
    model_in = fct_relabel(model_in, ~ c("Kalman with RU & Thompson", "Kalman with UCB & Thompson"))
  ) %>%
  rename(model_out = name)





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
    aic_total = aic_softmax + aic_thompson + aic_ucb + aic_ucb_thompson + aic_ru_thompson + aic_delta + aic_decay,
    bic_total = bic_softmax + bic_thompson + bic_ucb + bic_ucb_thompson + bic_ru_thompson + bic_delta + bic_decay,
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

tbl_results_delta <- tbl_results_delta %>%
  pivot_longer(c(starts_with("aic"), starts_with("bic"))) %>%
  filter(gamma_sd == .05) %>%
  mutate(
    model_in = fct_inorder(factor(is_decay)),
    model_in = fct_relabel(model_in, ~ c("Delta & Softmax", "Decay & Softmax"))
  ) %>%
  rename(model_out = name)



# plot model confusion matrices -------------------------------------------

tbl_results_all <- rbind(
  tbl_results_softmax %>% select(model_in, model_out, value),
  tbl_results_thompson %>% select(model_in, model_out, value),
  tbl_results_ucb %>% select(model_in, model_out, value),
  tbl_results_mixture %>% select(model_in, model_out, value),
  tbl_results_delta %>% select(model_in, model_out, value)
) %>% mutate(
  metric = str_match(model_out, "(^[a-z]*)_")[, 2],
  metric = fct_relabel(fct_inorder(factor(metric)), ~ c("AIC", "BIC")),
  model_out = str_match(model_out, "^[a-z]*_(.*)$")[, 2],
  model_out = fct_inorder(factor(model_out)),
  model_out = fct_relabel(model_out, ~ c(
    "Kalman &\nSoftmax", "Kalman &\nThompson", "Kalman &\nUCB", 
    "Kalman & RU\nand Thompson", "Kalman & UCB\nand Thompson", 
    "Delta &\nSoftmax", "Decay &\nSoftmax"
    )
),
  model_in = fct_relabel(fct_inorder(factor(model_in)), ~ c(
    "Kalman &\nSoftmax", "Kalman &\nThompson", "Kalman &\nUCB",
    "Kalman & RU\nand Thompson", "Kalman & UCB\nand Thompson", 
    "Delta &\nSoftmax", "Decay &\nSoftmax"
    )
)
)

pl_bic_aic <- ggplot(tbl_results_all, aes(model_in, model_out)) +
  geom_tile(aes(fill = value)) + 
  geom_label(aes(label = value)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_wrap(~ metric) +
  labs(x = "Model In", y = "Model Out") +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_viridis_c(guide = "none")

save_my_pdf_and_tiff(pl_bic_aic, "figures/model-recovery-simulated", 12, 6.5)

