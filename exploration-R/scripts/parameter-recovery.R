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
library(reactable)
library(reactablefmtr)


home_grown <- c("exploration-R/utils/utils.R", "exploration-R/utils/plotting.R")
walk(home_grown, source)


# Generate Random Walk Data -----------------------------------------------

fit_or_load <- "load"

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




# Kalman & Softmax: fit Xi and Epsilon Variances --------------------------


tbl_gammas <- tibble(
  gamma_mn = c(.16, .5, 1),
  gamma_sd = c(.03, .1, .2)
)
simulate_data <- c(TRUE, FALSE)
nr_participants <- c(200)
nr_trials <- c(200, 300)
cond_on_choices <- c(TRUE)


tbl_params_softmax <- crossing(
  tbl_gammas, simulate_data, nr_participants, nr_trials, cond_on_choices
)

if (fit_or_load == "fit")  {
  l_results_softmax <- pmap(
    tbl_params_softmax, kalman_softmax_experiment, 
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




# Kalman & Softmax: Fit Xi Variance ----------------------------------------


# take same combination of hyperparameters as before

if (fit_or_load == "fit")  {
  l_results_softmax_1var <- pmap(
    tbl_params_softmax, 
    kalman_softmax_experiment, 
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





# Kalman & Softmax: Fix Variances ------------------------------------------


if (fit_or_load == "fit")  {
  l_results_softmax_0var <- pmap(
    tbl_params_softmax, 
    kalman_softmax_experiment, 
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
  # filter(
  #   gamma_ml < 2.9
  # ) %>%
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





# Kalman & Thompson: fit Xi and Epsilon Variances --------------------------


# simulate_data <- c(TRUE, FALSE)#[1]
# nr_participants <- c(200)
# nr_trials <- c(300, 500)
# cond_on_choices <- c(TRUE)
# 
# 
# tbl_params_thompson <- crossing(
#   simulate_data, nr_participants, nr_trials, cond_on_choices
# )
# 
# 
# if (fit_or_load == "fit")  {
#   l_results_thompson <- pmap(
#     tbl_params_thompson, kalman_thompson_experiment, 
#     lambda = lambda, nr_vars = 2
#   )
#   saveRDS(l_results_thompson, "exploration-R/data/recovery-thompson-two-variances.RDS")
# } else if (fit_or_load == "load")  {
#   l_results_thompson <- readRDS("exploration-R/data/recovery-thompson-two-variances.RDS")
# }
# 
# counter <- 1
# l_results_c <- list()
# for (tbl_r in l_results_thompson) {
#   l_results_c[[counter]] <- as_tibble(cbind(
#     tbl_r %>% select(-c(simulate_data, nr_trials)), tbl_params_thompson[counter, ]
#   ))
#   counter = counter + 1
# }
# 
# tbl_cor_thompson <- reduce(l_results_c, rbind) %>%
#   unnest_wider(params_decision) %>%
#   filter(sigma_xi_sq_ml < 29 & sigma_epsilon_sq_ml < 29) %>%
#   group_by(simulate_data, nr_participants, nr_trials) %>%
#   summarize(
#     r_sigma_xi = cor(sigma_xi_sq, sigma_xi_sq_ml),
#     r_sigma_epsilon = cor(sigma_epsilon_sq, sigma_epsilon_sq_ml)
#   ) %>% ungroup()
# 
# tbl_cor_thompson_long <- tbl_cor_thompson %>% 
#   mutate(
#     simulate_data = factor(simulate_data),
#     simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE")
#   ) %>% rename("Sigma Xi" = r_sigma_xi, "Sigma Epsilon" = r_sigma_epsilon) %>%
#   pivot_longer(cols = c(`Sigma Xi`, `Sigma Epsilon`))
# 
# pd <- position_dodge(width = 1)
# plot_cor_recovery(tbl_cor_thompson_long, pd, "thompson")
# #   facet_wrap(~ name) +
# # possibly adopt plot_cor_recovery function, if error here
# 
# 
# # cors between pars
# f_clean_cor <- function(x) {
#   x <- x[x$sigma_xi_sq_ml < 29 & x$sigma_xi_sq_ml < 29, ]
#   cor(x[, c("sigma_xi_sq_ml", "sigma_epsilon_sq_ml")])
# }
# l_cors_params <- map(l_results_c, f_clean_cor)
# 
# counter <- 1
# for (tbl_r in l_cors_params) {
#   l_cors_params[[counter]] <- as_tibble(cbind(
#     tbl_r, tbl_params_thompson[counter, ]
#   ))
#   counter = counter + 1
# }
# 
# 
# l_heatmaps_par_cor <- map(l_cors_params, plot_my_heatmap_thompson)
# grid.draw(marrangeGrob(l_heatmaps_par_cor, nrow = 2, ncol = 2))
# 



# Kalman & Thompson: Fit Xi Variance --------------------------------------

# 
# 
# if (fit_or_load == "fit")  {
#   l_results_thompson_1var <- pmap(
#     tbl_params_thompson, kalman_thompson_experiment, 
#     lambda = lambda, nr_vars = 1
#   )
#   saveRDS(l_results_thompson_1var, "exploration-R/data/recovery-thompson-one-variance.RDS")
# } else if (fit_or_load == "load")  {
#   l_results_thompson_1var <- readRDS("exploration-R/data/recovery-thompson-one-variance.RDS")
# }
# 
# counter <- 1
# l_results_c <- list()
# for (tbl_r in l_results_thompson_1var) {
#   l_results_c[[counter]] <- as_tibble(cbind(
#     tbl_r %>% select(-c(simulate_data, nr_trials)), tbl_params_thompson[counter, ]
#   ))
#   counter = counter + 1
# }
# 
# tbl_cor_thompson_1var <- reduce(l_results_c, rbind) %>%
#   unnest_wider(params_decision) %>%
#   filter(sigma_xi_sq_ml < 29) %>%
#   group_by(simulate_data, nr_participants, nr_trials) %>%
#   summarize(
#     r_sigma_xi = cor(sigma_xi_sq, sigma_xi_sq_ml),
#   ) %>% ungroup()
# 
# tbl_cor_thompson_long_1var <- tbl_cor_thompson_1var %>% 
#   mutate(
#     simulate_data = factor(simulate_data),
#     simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE")
#   ) %>% rename("Sigma Xi" = r_sigma_xi) %>%
#   pivot_longer(cols = c(`Sigma Xi`))
# 
# pd <- position_dodge(width = 1)
# plot_cor_recovery(tbl_cor_thompson_long_1var, pd, "thompson")
# #   facet_wrap(~ name) +
# # possibly adopt plot_cor_recovery function, if error here



# Kalman & UCB: Fix Variances ---------------------------------------------



tbl_gammas <- tibble(
  gamma_mn = c(.16, .5, 1),#[1],
  gamma_sd = c(.03, .1, .2)#[1]
)
tbl_betas <- tibble(
  beta_mn = c(.17, 1.5),#[1],
  beta_sd = c(.05, .25)#[1]
)
simulate_data <- c(TRUE, FALSE)#[1]
nr_participants <- c(200)
nr_trials <- c(200, 300)
cond_on_choices <- c(TRUE)


tbl_params_ucb <- crossing(
  tbl_gammas, tbl_betas, simulate_data, nr_participants, nr_trials, cond_on_choices
)


if (fit_or_load == "fit")  {
  l_results_ucb_0var <- pmap(
    tbl_params_ucb, kalman_ucb_experiment,
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

tbl_cor_ucb_0var <- reduce(l_results_c_0var, rbind) %>%
  group_by(gamma_mn, beta_mn, simulate_data, nr_trials) %>%
  # filter(
  #   gamma_ml < 2.9 |
  #     beta_ml < 2.9
  # ) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_beta = cor(beta, beta_ml)
  ) %>% ungroup()

tbl_cor_ucb_0var_long <- tbl_cor_ucb_0var %>% 
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



# Kalman & (RU & Thompson): Fix Variances ----------------------------------


tbl_gammas <- tibble(
  gamma_mn = c(.16, 1),#[1],
  gamma_sd = c(.03, .2)#[1]
)
tbl_betas <- tibble(
  beta_mn = c(.17, 1.5),#[1],
  beta_sd = c(.05, .25)#[1]
)
tbl_w_mix <- tibble(
  w_mix_mn = c(.5, .75),
  w_mix_sd = c(.2, .15)
)
  
simulate_data <- c(TRUE, FALSE)#[1]
nr_participants <- c(200)
nr_trials <- c(200, 300)
cond_on_choices <- c(TRUE)


tbl_params_ru_thompson <- crossing(
  tbl_gammas, tbl_betas, tbl_w_mix,
  simulate_data, nr_participants, nr_trials, cond_on_choices
)


if (fit_or_load == "fit")  {
  l_results_ru_thompson_0var <- pmap(
    tbl_params_ru_thompson, kalman_ru_thompson_experiment,
    lambda = lambda, nr_vars = 0
  )
  saveRDS(l_results_ru_thompson_0var, "exploration-R/data/recovery-ru-thompson-no-variance.RDS")
} else if (fit_or_load == "load")  {
  l_results_ru_thompson_0var <- readRDS("exploration-R/data/recovery-ru-thompson-no-variance.RDS")
}

counter <- 1
l_results_mix_0var <- list()
for (tbl_r in l_results_ru_thompson_0var) {
  l_results_mix_0var[[counter]] <- as_tibble(cbind(
    tbl_r %>% 
      unnest_wider(params_decision) %>%
      select(-c(simulate_data, nr_trials)), tbl_params_ru_thompson[counter, ]
  ))
  counter <- counter + 1
}

tbl_cor_ru_thompson_0var <- reduce(l_results_mix_0var, rbind) %>%
  group_by(gamma_mn, beta_mn, w_mix_mn, simulate_data, nr_trials) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_beta = cor(beta, beta_ml),
    r_w_mix = cor(w_mix, w_mix_ml)
  ) %>% ungroup()

tbl_cor_ru_thompson_0var_long <- tbl_cor_ru_thompson_0var %>% 
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
plot_cor_recovery(tbl_cor_ru_thompson_0var_long, pd, "ucb")



# Delta & Softmax ---------------------------------------------------------


tbl_gammas <- tibble(
  gamma_mn = c(.08, .16),#[1],
  gamma_sd = c(.05, .1)#[1]
)
tbl_deltas <- tibble(
  delta_mn = c(.55, .85),#[1],
  delta_sd = c(.20, .09)#[1]
)
simulate_data <- c(TRUE, FALSE)
nr_participants <- c(200)
nr_trials <- c(200, 400, 600)
cond_on_choices <- c(TRUE)
is_decay <- c(FALSE, TRUE)

tbl_params_delta <- crossing(
  tbl_gammas, tbl_deltas, simulate_data, nr_participants, 
  nr_trials, cond_on_choices, is_decay
)

if (fit_or_load == "fit")  {
  l_results_delta_softmax <- pmap(
    tbl_params_delta, delta_experiment,
    lambda = lambda
  )
  saveRDS(l_results_delta_softmax, "exploration-R/data/recovery-delta-softmax.RDS")
} else if (fit_or_load == "load")  {
  l_results_delta_softmax <- readRDS("exploration-R/data/recovery-delta-softmax.RDS")
}

m <- -1
while(m < 0) {
  vals <- rnorm(200, .16, .1)
  m <- min(vals)
}

hist(vals)


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
  # filter(
  #   gamma_ml < 2.9 &
  #     delta_ml < 0.99
  # ) %>%
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


ggplot(
  tbl_cor_delta_softmax_long %>% filter(gamma_mn %in% c(.08, .16) & simulate_data == "Simulate By Participant"), 
  aes(gamma_mn, value, group = interaction(nr_trials, name))
) +
  geom_hline(yintercept = 1, linetype = "dotdash", color = "grey") +
  geom_hline(yintercept = .5, linetype = "dotdash", color = "grey") +
  geom_line(aes(group = interaction(nr_trials, name))) +
  facet_grid(delta_mn ~ is_decay) +
  geom_point(aes(color = nr_trials, shape = name)) +
  theme_bw() +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_y_continuous(expand = c(.01, 0)) +
  labs(x = "", y = "")

pd <- position_dodge(width = .9)
plot_cor_recovery(tbl_cor_delta_softmax_long, pd, "softmax") +
  facet_grid(interaction(delta_mn, name) ~ interaction(simulate_data, is_decay))

recovery_simulated_summary <- function(gamma_val) {
  tbl_sm0 <- tbl_cor_softmax_0var %>% 
    filter(gamma_mn == gamma_val & simulate_data & nr_trials == 300) %>% 
    select(-nr_participants) %>%
    mutate(model = "Kalman Softmax")
  tbl_ucb0 <- tbl_cor_ucb_0var %>% 
    filter(gamma_mn == gamma_val & simulate_data & beta_mn == .17 & nr_trials == 300) %>%
    mutate(model = "Kalman UCB")
  tbl_delta_sm <- tbl_cor_c_delta_softmax %>% 
    filter(gamma_mn == gamma_val & simulate_data & nr_trials == 300) %>%
    filter(!is_decay & delta_mn == .9) %>%
    mutate(model = "Delta")
  tbl_decay_sm <- tbl_cor_c_delta_softmax %>% 
    filter(gamma_mn == gamma_val & simulate_data & nr_trials == 300) %>%
    filter(is_decay & delta_mn == .55) %>%
    mutate(model = "Decay")
  bind_rows(tbl_sm0, tbl_ucb0, tbl_delta_sm, tbl_decay_sm) %>%
    select(nr_trials, model, r_gamma, r_beta, r_delta)
}

tbl_1 <- recovery_simulated_summary(.16) %>% select(-nr_trials)
tbl_2 <- recovery_simulated_summary(.5) %>% select(-nr_trials)

badtogood_cols <- c('#d65440', '#ffffff', "forestgreen")

my_nice_tbl <- function(my_tbl) {
  colnames(my_tbl) <- c("Model", "Gamma", "Beta", "Delta")
  my_tbl[, c("Gamma", "Beta", "Delta")] <- map(my_tbl[, c("Gamma", "Beta", "Delta")], ~ round(.x, digits = 2))
  reactable(
    my_tbl,
    defaultColDef = colDef(
      minWidth = 150,
      align = "center",
      cell = color_tiles(my_tbl, span = 2:4, colors = badtogood_cols)
    ),
    columns = list(
      Model = colDef(
        style = cell_style(data,
                           font_weight = "bold"))
    )
  )
}
my_nice_tbl(tbl_1)
my_nice_tbl(tbl_2)

recovery_simulated_summary <- function(gamma_val) {
  tbl_sm0 <- tbl_cor_softmax_0var %>% 
    filter(gamma_mn == gamma_val & simulate_data & nr_trials == 300) %>% 
    select(-nr_participants) %>%
    mutate(model = "Kalman Softmax")
  tbl_ucb0 <- tbl_cor_ucb_0var %>% 
    filter(gamma_mn == gamma_val & simulate_data & beta_mn == .17 & nr_trials == 300) %>%
    mutate(model = "Kalman UCB")
  tbl_delta_sm <- tbl_cor_c_delta_softmax %>% 
    filter(gamma_mn == .16 & simulate_data & nr_trials == 600) %>%
    filter(!is_decay & delta_mn == .55) %>%
    mutate(model = "Delta")
  tbl_decay_sm <- tbl_cor_c_delta_softmax %>% 
    filter(gamma_mn == .08 & simulate_data & nr_trials == 600) %>%
    filter(is_decay & delta_mn == .55) %>%
    mutate(model = "Decay")
  bind_rows(tbl_sm0, tbl_ucb0, tbl_delta_sm, tbl_decay_sm) %>%
    select(nr_trials, model, r_gamma, r_beta, r_delta)
}

tbl_1 <- recovery_simulated_summary(.16) %>% select(-nr_trials)
tbl_1_long <- tbl_1 %>% pivot_longer(-model)
tbl_1_long$name <- factor(tbl_1_long$name)
levels(tbl_1_long$name) <- c("Beta", "Delta", "Gamma")
tbl_1_long$name <- fct_relevel(tbl_1_long$name, "Beta", after = 2)
pl_heatmap_cherry <- ggplot(tbl_1_long, aes(name, model)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2)), color = "black") +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient2(high = "aquamarine2", low = "tomato", mid = .5, guide = "none") +
  labs(x = "", y = "")


rule <- "Delta Rule"
rule <- "Decay Rule"
gamma_cherry <- .08
gamma_cherry <- .16


pl_landscape_delta <- plot_landscape_with_cherry("Delta Rule", .16)
pl_landscape_decay <- plot_landscape_with_cherry("Decay Rule", .08)

grid.draw(arrangeGrob(pl_heatmap_cherry, pl_landscape_delta, nrow = 1))
grid.draw(arrangeGrob(pl_heatmap_cherry, pl_landscape_decay, nrow = 1))




plot_landscape_with_cherry <- function(rule, gamma_cherry) {
  tbl_plot <- tbl_cor_delta_softmax_long %>%
    filter(simulate_data == "Simulate By Participant" & is_decay == rule) %>%
    mutate(delta_mn = as.factor(delta_mn))
  
  tbl_cherry <- tbl_plot %>% filter(gamma_mn == gamma_cherry & nr_trials == 600 & delta_mn == .55)
  
  pl_landscape <- ggplot(tbl_plot, aes(gamma_mn, value, group = interaction(delta_mn, nr_trials))) +
    geom_line(aes(color = nr_trials, linetype = delta_mn)) +
    geom_point(color = "white", size = 3) +
    geom_point(aes(color = nr_trials, shape = delta_mn))  +
    geom_hline(yintercept = 1, linetype = "dotdash", color = "grey") +
    geom_hline(yintercept = .5, linetype = "dotdash", color = "grey") +
    facet_wrap(~ name) +
    theme_bw() +
    scale_x_continuous(expand = c(.01, .01)) +
    scale_y_continuous(expand = c(.01, .01)) +
    labs(x = "", y = "") +
    scale_color_continuous(high = "slateblue", low = "lightblue", name = "Nr. Trials") +
    scale_shape_discrete(name = "Mean Delta") +
    scale_linetype_discrete(guide = "none") +
    coord_cartesian(ylim = c(0, 1)) +
    geom_point(data = tbl_cherry, shape = 0, size = 3)
  
  return(pl_landscape)
  
}


