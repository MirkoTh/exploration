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


# Main Experiment Kalman & Softmax ----------------------------------------


tbl_gammas <- tibble(
  gamma_mn = c(.16, .5, 1),
  gamma_sd = c(.03, .1, .2)
)
simulate_data <- c(TRUE, FALSE)
nr_participants <- c(200)
nr_trials <- c(200, 400)

tbl_params_softmax <- crossing(
  tbl_gammas, simulate_data, nr_participants, nr_trials
)

fit_or_load <- "load"
if (fit_or_load == "fit")  {
  l_results_softmax <- pmap(tbl_params_softmax, simulate_and_fit_softmax, lambda = lambda, nr_vars = 2)
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
  group_by(gamma_mn, simulate_data, nr_participants, nr_trials) %>%
  filter(
    gamma_ml < 2.9 & sigma_xi_sq_ml < 29 & sigma_epsilon_sq_ml < 29
  )   %>%
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
ggplot(tbl_cor_softmax_long, aes(as.factor(gamma_mn), value, group = as.factor(nr_trials))) +
  geom_col(aes(fill = as.factor(nr_trials)), position = pd) +
  geom_label(
    aes(y = value - .1, label = str_c("r = ", round(value, 2))), 
    position = pd, label.padding = unit(.1, "lines")
  ) + geom_hline(
    yintercept = 1, color = "grey", alpha = 1, size = 1, linetype = "dotdash"
  ) + facet_grid(name ~ simulate_data) +
  theme_bw() +
  scale_fill_viridis_d(name = "Nr. Trials") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(-.1, 1.1)) +
  labs(
    x = "Gamma (Mean)",
    y = "Correlation"
  )

l_cors_params <- map(
  l_results_c, ~ cor(.x[, c("sigma_xi_sq_ml", "sigma_epsilon_sq_ml", "gamma_ml")])
)

counter <- 1
for (tbl_r in l_cors_params) {
  l_cors_params[[counter]] <- as_tibble(cbind(
    tbl_r, tbl_params_softmax[counter, ]
  ))
  counter = counter + 1
}


l_heatmaps_par_cor <- map(l_cors_params, plot_my_heatmap_softmax)
grid.draw(marrangeGrob(l_heatmaps_par_cor, nrow = 4, ncol = 3))


# Main Experiment Kalman & Thompson ---------------------------------------


simulate_data <- c(TRUE, FALSE)
nr_participants <- c(200)
nr_trials <- c(200, 400)

tbl_params_thompson <- crossing(
  simulate_data, nr_participants, nr_trials
)

l_results_thompson <- pmap(tbl_params_thompson, simulate_and_fit_thompson, lambda = lambda)

counter <- 1
l_results_c <- list()
for (tbl_r in l_results_thompson) {
  l_results_c[[counter]] <- as_tibble(cbind(
    tbl_r %>% select(-c(simulate_data, nr_trials)), tbl_params_thompson[counter, ]
  ))
  counter = counter + 1
}

tbl_cor_thompson <- reduce(l_results_c, rbind) %>%
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

pd <- position_dodge(width = 150)
ggplot(tbl_cor_thompson_long, aes(nr_trials, value, group = as.factor(simulate_data))) +
  geom_col(aes(fill = as.factor(simulate_data)), position = pd) +
  geom_label(
    aes(y = value - .1, label = str_c("r = ", round(value, 2))), 
    position = pd, label.padding = unit(.1, "lines")
  ) + geom_hline(
    yintercept = 1, color = "grey", alpha = 1, size = 1, linetype = "dotdash"
  ) +
  facet_wrap(~ name) +
  theme_bw() +
  scale_fill_viridis_d(name = "Nr. Trials") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(-.1, 1.1)) +
  labs(
    x = "Gamma (Mean)",
    y = "Correlation"
  )

l_cors_params <- map(
  l_results_c, ~ cor(.x[, c("sigma_xi_sq_ml", "sigma_epsilon_sq_ml")])
)

counter <- 1
for (tbl_r in l_cors_params) {
  l_cors_params[[counter]] <- as_tibble(cbind(
    tbl_r, tbl_params_thompson[counter, ]
  ))
  counter = counter + 1
}


l_heatmaps_par_cor <- map(l_cors_params, plot_my_heatmap_thompson)
grid.draw(marrangeGrob(l_heatmaps_par_cor, nrow = 2, ncol = 2))


# Main Experiment Kalman & Softmax: Fit Only Xi Variance--------------------


# take same combination of hyperparameters as before

fit_or_load <- "load"
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
ggplot(tbl_cor_softmax_1var_long, aes(as.factor(gamma_mn), value, group = as.factor(nr_trials))) +
  geom_col(aes(fill = as.factor(nr_trials)), position = pd) +
  geom_label(
    aes(y = value - .1, label = str_c("r = ", round(value, 2))), 
    position = pd, label.padding = unit(.1, "lines")
  ) + geom_hline(
    yintercept = 1, color = "grey", alpha = 1, size = 1, linetype = "dotdash"
  ) + facet_grid(name ~ simulate_data) +
  theme_bw() +
  scale_fill_viridis_d(name = "Nr. Trials") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(-.1, 1.1)) +
  labs(
    x = "Gamma (Mean)",
    y = "Correlation"
  )

l_cors_params <- map(
  l_results_c_1var, ~ cor(.x[, c("sigma_xi_sq_ml", "gamma_ml")])
)

counter <- 1
for (tbl_r in l_cors_params) {
  l_cors_params[[counter]] <- as_tibble(cbind(
    tbl_r, tbl_params_softmax[counter, ]
  ))
  counter = counter + 1
}


l_heatmaps_par_cor <- map(l_cors_params, plot_my_heatmap_softmax, nr_var = 1)
grid.draw(marrangeGrob(l_heatmaps_par_cor, nrow = 4, ncol = 3))


# notes
# danwitz et al. 2022 only fit softmax temperature param, but keep var_xi and var_eps fixed to true values
# daw et al. 2006 fit kalman filter with two variances being estimated, but provide no recovery studies
# speekenbrink & konstantinidis (2015) fit kalman model also with two variances being estimated, but provide no recovery studies


# todos
# fix both sigma_xi and sigma_epsilon
# add ucb choice rule



# Softmax: Fix Variances --------------------------------------------------

fit_or_load <- "load"
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
ggplot(tbl_cor_softmax_0var_long, aes(as.factor(gamma_mn), value, group = as.factor(nr_trials))) +
  geom_col(aes(fill = as.factor(nr_trials)), position = pd) +
  geom_label(
    aes(y = value - .1, label = str_c("r = ", round(value, 2))), 
    position = pd, label.padding = unit(.1, "lines")
  ) + geom_hline(
    yintercept = 1, color = "grey", alpha = 1, size = 1, linetype = "dotdash"
  ) + facet_grid(name ~ simulate_data) +
  theme_bw() +
  scale_fill_viridis_d(name = "Nr. Trials") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(-.1, 1.1)) +
  labs(
    x = "Gamma (Mean)",
    y = "Correlation"
  )
