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
library(ggbeeswarm)
library(reactable)
library(reactablefmtr)

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


plan(multisession, workers = availableCores() - 2)
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

tbl_participants_kalman_softmax <- my_participants_tbl_kalman(l_params_decision)
tbl_results_kalman_softmax <- simulate_and_fit_softmax(
  tbl_participants_kalman_softmax, nr_vars = 0, cond_on_choices = TRUE, nr_trials = nr_trials
  )

tbl_recovery_kalman_softmax <- tbl_results_kalman_softmax %>%
  unnest_wider(params_decision) %>%
  group_by(simulate_data) %>%
  # filter(
  #   gamma_ml < 2.9
  # ) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml)
  ) %>% ungroup()

tbl_cor_softmax_0var_long <- tbl_recovery_kalman_softmax %>% 
  mutate(
    simulate_data = factor(simulate_data),
    #simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE")
  ) %>% rename("Gamma" = r_gamma) %>%
  pivot_longer(cols = c(Gamma))




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

tbl_participants_kalman_ucb <- my_participants_tbl_kalman(l_params_decision)
tbl_results_kalman_ucb <- simulate_and_fit_ucb(tbl_participants_kalman_ucb, nr_vars = 0, cond_on_choices = TRUE, nr_trials = nr_trials)

tbl_recovery_kalman_ucb <- tbl_results_kalman_ucb %>%
  unnest_wider(params_decision) %>%
  group_by(simulate_data) %>%
  # filter(
  #   gamma_ml < 2.9 &
  #     beta_ml < 2.9
  # ) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_beta = cor(beta, beta_ml)
  ) %>% ungroup()

tbl_recovery_kalman_ucb_long <- tbl_recovery_kalman_ucb %>% 
  mutate(
    beta_mn = "empirical",
    gamma_mn = "empirical",
    simulate_data = factor(simulate_data),
  ) %>% 
  rename(
    "Gamma" = r_gamma,
    "Beta" = r_beta
  ) %>%
  pivot_longer(cols = c(Gamma, Beta))


## Thompson Sampling (Xi Variance) ----------------------------------------


# 
# plan(multisession, workers = availableCores() - 2)
# l_kalman_thompson_one_variance <- furrr::future_map(
#   l_participants, fit_thompson_one_variance_wrapper,
#   tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE,
#   .progress = TRUE
# )
# tbl_kalman_thompson_one_variance <- reduce(l_kalman_thompson_one_variance, rbind) %>%
#   as.data.frame() %>% as_tibble() %>% rename(xi_innovation = V1, ll = V3)
# 
# l_params_decision <- map2(
#   tbl_kalman_thompson_one_variance$xi_innovation,
#   ~ list(xi_eta_sq = ..1, choicemodel = "thompson", no = 4)
# )
# 
# tbl_participants_kalman_thompson <- my_participants_tbl_kalman(l_params_decision)
# tbl_results_kalman_softmax <- simulate_and_fit_thompson(tbl_participants_kalman_thompson, nr_vars = 1, cond_on_choices = TRUE)
# 


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
tbl_results_delta_softmax <- simulate_and_fit_delta(tbl_participants_delta, is_decay = FALSE, cond_on_choices = TRUE, nr_trials = nr_trials)

tbl_recovery_delta_softmax <- tbl_results_delta_softmax %>%
  unnest_wider(params_decision) %>%
  group_by(simulate_data) %>%
  # filter(
  #   gamma_ml < 2.9 &
  #     delta_ml < 0.99
  # ) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_delta = cor(delta, delta_ml)
  ) %>% ungroup()

tbl_recovery_delta_softmax_long <- tbl_recovery_delta_softmax %>% 
  mutate(
    gamma_mn = "empirical",
    is_decay = FALSE,
    simulate_data = factor(simulate_data),
    is_decay = factor(is_decay),
    # simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE"),
    # is_decay = fct_recode(is_decay, "Decay Rule" = "TRUE", "Delta Rule" = "FALSE")
  ) %>% 
  rename(
    "Gamma" = r_gamma,
    "Delta" = r_delta
  ) %>%
  pivot_longer(cols = c(Gamma, Delta))



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
tbl_results_decay_softmax <- simulate_and_fit_delta(tbl_participants_decay, is_decay = TRUE, cond_on_choices = TRUE, nr_trials = nr_trials)

tbl_recovery_decay_softmax <- tbl_results_decay_softmax %>%
  unnest_wider(params_decision) %>%
  group_by(simulate_data) %>%
  # filter(
  #   gamma_ml < 2.9 &
  #     delta_ml < 0.99
  # ) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_delta = cor(delta, delta_ml)
  ) %>% ungroup()

tbl_recovery_decay_softmax_long <- tbl_recovery_decay_softmax %>% 
  mutate(
    gamma_mn = "empirical",
    is_decay = FALSE,
    simulate_data = factor(simulate_data),
    is_decay = factor(is_decay),
    # simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE"),
    # is_decay = fct_recode(is_decay, "Decay Rule" = "TRUE", "Delta Rule" = "FALSE")
  ) %>% 
  rename(
    "Gamma" = r_gamma,
    "Delta" = r_delta
  ) %>%
  pivot_longer(cols = c(Gamma, Delta))

pd <- position_dodge(width = .9)
plot_cor_recovery(tbl_recovery_decay_softmax_long, pd, "softmax") +
  facet_grid(name ~ is_decay)




# Summarize Results -------------------------------------------------------

wrangle_recoveries <- function(my_tbl, modelname) {
  tbl_summary <- tibble(pars = c("simulate_data", "r_gamma", "r_beta", "r_delta"))
  out <- tbl_summary %>% left_join(
    my_tbl %>% 
      mutate(model = modelname) %>%
      pivot_longer(cols = -model),
    by = c("pars" = "name")
  ) %>%
    pivot_wider(names_from = pars, values_from = value)
  out[!is.na(out$model), ]
}

km_sm <- wrangle_recoveries(tbl_recovery_kalman_softmax, "Kalman Softmax")
km_ucb <- wrangle_recoveries(tbl_recovery_kalman_ucb, "Kalman UCB")
delta_sm <- wrangle_recoveries(tbl_recovery_delta_softmax, "Delta Softmax")
decay_sm <- wrangle_recoveries(tbl_recovery_decay_softmax, "Decay Softmax")

tbl_summary_pars <- rbind(km_sm, km_ucb, delta_sm, decay_sm)
# visualize summary recovery of four models with reactable table
badtogood_cols <- c('#d65440', '#ffffff', "forestgreen")

colnames(tbl_summary_pars) <- c("Model", "simulate_data", "Gamma", "Beta", "Delta")
tbl_summary_pars[, c("Gamma", "Beta", "Delta")] <- map(tbl_summary_pars[, c("Gamma", "Beta", "Delta")], ~ round(.x, digits = 2))
tbl_summary_pars <- tbl_summary_pars %>% select(-simulate_data)
reactable(
  tbl_summary_pars,
  defaultColDef = colDef(
    minWidth = 150,
    align = "center",
    cell = color_tiles(tbl_summary_pars, span = 2:4, colors = badtogood_cols)
  ),
  columns = list(
    Model = colDef(
      style = cell_style(data,
                         font_weight = "bold"))
  )
)


tbl_gammas <- tbl_results_kalman_softmax %>%
  select(gamma_ml) %>%
  mutate(model = "Kalman Softmax") %>%
  rbind(
    tbl_results_kalman_ucb %>%
      select(gamma_ml) %>%
      mutate(model = "Kalman UCB") 
  ) %>%
  rbind(
    tbl_results_delta_softmax %>%
      select(gamma_ml) %>%
      mutate(model = "Delta Softmax") 
  ) %>%
  rbind(
    tbl_results_decay_softmax %>%
      select(gamma_ml) %>%
      mutate(model = "Decay Softmax") 
  ) %>% rename(gamma = gamma_ml)

tbl_gammas$model <- factor(tbl_gammas$model)
tbl_gammas$model <- fct_inorder(tbl_gammas$model)
pl_gammas <- ggplot(tbl_gammas, aes(model, gamma, group = model)) +
  geom_violin(aes(fill = model), alpha = .25) +
  geom_quasirandom(aes(color = model), cex = 1.75, alpha = .5, method = "quasirandom") +
  geom_boxplot(width = .25, aes(color = model), alpha = .7) +
  stat_summary(geom = "point", fun = "mean", color = "black", size = 3, shape = 23) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.003, .003)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_cartesian(ylim = c(0, .4)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Model", y = "Gamma")
  


tbl_deltas <- tbl_results_delta_softmax %>%
  select(delta_ml) %>%
  mutate(model = "Delta Softmax") %>%
  rbind(
    tbl_results_decay_softmax %>%
      select(delta_ml) %>%
      mutate(model = "Decay Softmax") 
  ) %>% rename(delta = delta_ml)

tbl_deltas$model <- factor(tbl_deltas$model)
tbl_deltas$model <- fct_inorder(tbl_deltas$model)
pl_deltas <- ggplot(tbl_deltas, aes(model, delta, group = model)) +
  geom_violin(aes(fill = model), alpha = .25) +
  geom_quasirandom(aes(color = model), cex = 1.75, alpha = .5, method = "quasirandom") +
  geom_boxplot(width = .25, aes(color = model), alpha = .7) +
  stat_summary(geom = "point", fun = "mean", color = "black", size = 3, shape = 23) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.003, .003)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Model", y = "Delta")

tbl_beta <- tbl_results_kalman_ucb %>%
  select(beta_ml) %>%
  mutate(model = "Kalman UCB") %>%
  rename(beta = beta_ml)

tbl_beta$model <- factor(tbl_beta$model)
tbl_beta$model <- fct_inorder(tbl_beta$model)
pl_beta <- ggplot(tbl_beta, aes(model, beta, group = model)) +
  geom_violin(aes(fill = model), alpha = .25) +
  geom_quasirandom(aes(color = model), cex = 1.75, alpha = .5, method = "quasirandom") +
  geom_boxplot(width = .25, aes(color = model), alpha = .7) +
  stat_summary(geom = "point", fun = "mean", color = "black", size = 3, shape = 23) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(.003, .003)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Model", y = "Beta")

pl_params_empirical <- arrangeGrob(pl_gammas, pl_deltas, pl_beta, nrow = 1, widths = c(1, .5, .35))
save_my_pdf(pl_params_empirical, "figures/estimated-parameters-empirical-same-stimuli.pdf", 12, 4)
#save_my_pdf(pl_params_empirical, "figures/estimated-parameters-empirical-individual-stimuli.pdf", 12, 4)


