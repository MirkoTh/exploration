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
library(ggbeeswarm)


home_grown <- c("exploration-R/utils/utils.R", "exploration-R/utils/plotting.R")
walk(home_grown, source)


# Generate Random Walk Data -----------------------------------------------

fit_or_load <- "load"

mu1 <- c(-60, -20, 20, 60)
nr_trials <- 200
sigma_xi_sq <- 16
sigma_epsilon_sq <- 16
lambda <- .9836
n_reps <- 10

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
  gamma_mn = c(.08, .16, 1),#[1],
  gamma_sd = c(.05, .1, .2)#[1]
)
simulate_data <- c(TRUE, FALSE)
nr_participants <- c(200)
nr_trials <- c(200, 400)
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
  # filter(
  #   gamma_ml < 2.9 & sigma_xi_sq_ml < 29 & sigma_epsilon_sq_ml < 29
  # ) %>%
  summarize(
    r_sigma_xi = cor(sigma_xi_sq, sigma_xi_sq_ml),
    r_sigma_epsilon = cor(sigma_epsilon_sq, sigma_epsilon_sq_ml),
    r_gamma = cor(gamma, gamma_ml)
  ) %>% ungroup()

tbl_cor_softmax_long <- tbl_cor_softmax %>% 
  filter(!is.na(gamma_mn)) %>%
  mutate(
    simulate_data = factor(simulate_data),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE")
  ) %>% rename("Sigma Xi" = r_sigma_xi, "Sigma Epsilon" = r_sigma_epsilon, "Gamma" = r_gamma) %>%
  pivot_longer(cols = c(Gamma, `Sigma Xi`, `Sigma Epsilon`))
# 
# pd <- position_dodge(width = .9)
# plot_cor_recovery(tbl_cor_softmax_long, pd, "softmax")


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
  filter(!is.na(gamma_mn)) %>%
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
  l_results_softmax_0var_all <- list()
  for (i in 1:n_reps) {
    l_results_softmax_0var <- pmap(
      tbl_params_softmax, 
      kalman_softmax_experiment, 
      lambda = lambda,
      nr_vars = 0
    )
    l_results_softmax_0var_all[[i]] <- l_results_softmax_0var
    saveRDS(l_results_softmax_0var_all, "exploration-R/data/recovery-softmax-no-variance.RDS")
  }
} else if (fit_or_load == "load")  {
  l_results_softmax_0var <- readRDS("exploration-R/data/recovery-softmax-no-variance.RDS")
}

add_repl_id <- function(x, y) {x$replication_id <- y; return(x)}

l_results_softmax_0var <- map2(
  l_results_softmax_0var, 1:length(l_results_softmax_0var), 
  ~ map2(.x, rep(.y, length(.x)), add_repl_id)
)
l_results_softmax_0var <- reduce(l_results_softmax_0var, c)


counter <- 1
counter_design <- 1
l_results_c_0var <- list()
for (tbl_r in l_results_softmax_0var) {
  l_results_c_0var[[counter]] <- as_tibble(cbind(
    tbl_r %>% select(-c(simulate_data, nr_trials)), tbl_params_softmax[counter_design, ]
  ))
  counter <- counter + 1
  counter_design <- counter %% nrow(tbl_params_softmax)
  counter_design[counter_design == 0] <- nrow(tbl_params_softmax)
}

tbl_cor_softmax_0var <- reduce(l_results_c_0var, rbind) %>%
  unnest_wider(params_decision) %>%
  group_by(replication_id, gamma_mn, simulate_data, nr_participants, nr_trials) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml)
  ) %>% ungroup()


tbl_cor_softmax_0var_long <- tbl_cor_softmax_0var %>% 
  filter(!is.na(gamma_mn)) %>%
  mutate(
    simulate_data = factor(simulate_data),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE"),
    nr_trials = factor(nr_trials, labels = c("200 Trials", "400 Trials"))
  ) %>%
  rename("Gamma" = r_gamma) %>%
  pivot_longer(cols = c(Gamma))

# pd <- position_dodge(width = .9)
# plot_cor_recovery(tbl_cor_softmax_0var_long, pd, "softmax")
ggplot(tbl_cor_softmax_0var_long, aes(as.factor(gamma_mn), value)) +
  geom_violin(alpha = .25) +
  geom_quasirandom(aes(color = nr_trials), method = "quasirandom", cex = 1.75, alpha = .5, width = .1) +
  facet_wrap(nr_trials ~ simulate_data) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_viridis_d(name = "Nr. Trials") +
  labs(x = "Gamma Mean Gen.", y = "Correlation In - Out", title = "Gamma (inv. Temp.)") + 
  theme(strip.background = element_rect(fill = "white"))


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




# Kalman & Thompson: Fit Xi Variance --------------------------------------


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
#   facet_wrap(~ name) +
# possibly adopt plot_cor_recovery function, if error here



# Kalman & UCB: Fix Variances ---------------------------------------------



tbl_gammas <- tibble(
  gamma_mn = c(.08, .16, 1),#[1],
  gamma_sd = c(.05, .1, .2)#[1]
)
tbl_betas <- tibble(
  beta_mn = c(.17, 1.5),#[1],
  beta_sd = c(.05, .25)#[1]
)
simulate_data <- c(TRUE, FALSE)#[1]
nr_participants <- c(200)
nr_trials <- c(200, 400)
cond_on_choices <- c(TRUE)


tbl_params_ucb <- crossing(
  tbl_gammas, tbl_betas, simulate_data, nr_participants, nr_trials, cond_on_choices
)


if (fit_or_load == "fit")  {
  l_results_ucb_0var_all <- list()
  for (i in 1:n_reps) {
    l_results_ucb_0var <- pmap(
      tbl_params_ucb, kalman_ucb_experiment,
      lambda = lambda, nr_vars = 0
    )
    l_results_ucb_0var_all[[i]] <- l_results_ucb_0var
    saveRDS(l_results_ucb_0var_all, "exploration-R/data/recovery-ucb-no-variance.RDS")
  }
} else if (fit_or_load == "load")  {
  l_results_ucb_0var <- readRDS("exploration-R/data/recovery-ucb-no-variance.RDS")
}


l_results_ucb_0var <- map2(
  l_results_ucb_0var, 1:length(l_results_ucb_0var), 
  ~ map2(.x, rep(.y, length(.x)), add_repl_id)
)
l_results_ucb_0var <- reduce(l_results_ucb_0var, c)



counter <- 1
counter_design <- 1
l_results_c_0var <- list()
for (tbl_r in l_results_ucb_0var) {
  l_results_c_0var[[counter]] <- as_tibble(cbind(
    tbl_r %>% 
      unnest_wider(params_decision) %>%
      select(-c(simulate_data, nr_trials)), tbl_params_ucb[counter_design, ]
  ))
  counter <- counter + 1
  counter_design <- counter %% nrow(tbl_params_ucb)
  counter_design[counter_design == 0] <- nrow(tbl_params_ucb)
}

tbl_cor_ucb_0var <- reduce(l_results_c_0var, rbind) %>%
  group_by(replication_id, gamma_mn, beta_mn, simulate_data, nr_trials) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_beta = cor(beta, beta_ml)
  ) %>% ungroup()

tbl_cor_ucb_0var_long <- tbl_cor_ucb_0var %>% 
  mutate(
    simulate_data = factor(simulate_data),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE"),
    nr_trials = factor(nr_trials, labels = c("200 Trials", "400 Trials")),
    beta_mn = factor(beta_mn, labels = c("Beta = .17", "Beta = 1.5"))
  ) %>% 
  rename(
    "Gamma" = r_gamma,
    "Beta" = r_beta
  ) %>%
  pivot_longer(cols = c(Gamma, Beta))

# pd <- position_dodge(width = .9)
# plot_cor_recovery(tbl_cor_ucb_0var_long, pd, "ucb")

plot_ucb_param_recovery <- function(param, ttl) {
  ggplot(tbl_cor_ucb_0var_long %>% filter(name == param), aes(as.factor(gamma_mn), value)) +
    geom_hline(yintercept = .5, color = "grey", linetype = "dotdash") +
    geom_hline(yintercept = 1, color = "grey", linetype = "dotdash") +
    geom_violin(alpha = .25) +
    geom_quasirandom(aes(color = nr_trials), method = "quasirandom", cex = 1.75, alpha = .5, width = .1) +
    facet_grid(interaction(simulate_data, nr_trials, sep = " & ") ~ beta_mn) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_bw() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(.01, .01)) +
    scale_color_viridis_d(name = "Nr. Trials") +
    labs(x = "Gamma Mean Gen.", y = "Correlation In - Out", title = ttl) + 
    theme(strip.background = element_rect(fill = "white"))
}

plot_ucb_param_recovery("Gamma", "Gamma (inv. temp.)")
plot_ucb_param_recovery("Beta", "Beta (inf. bonus)")





# Kalman & Mixture: Fix Variances ------------------------------------------
# mixture can be ucb & thompson or only ru & thompson (with means = 0)

tbl_gammas <- tibble(
  gamma_mn = c(.08, .16),
  gamma_sd = c(.05, .1)
)
tbl_betas <- tibble(
  beta_mn = c(.17, 1.5),
  beta_sd = c(.05, .25)
)
tbl_w_mix <- tibble(
  w_mix_mn = c(.4, .6),
  w_mix_sd = c(.2, .2)
)

simulate_data <- c(FALSE) # TRUE, 
nr_participants <- c(200)
nr_trials <- c(400) # 200, 
cond_on_choices <- c(TRUE)
mixturetype <- c("ucb_thompson") #"ru_thompson"


tbl_params_mixture <- crossing(
  tbl_gammas, tbl_betas, tbl_w_mix,
  simulate_data, nr_participants, nr_trials, cond_on_choices, mixturetype
)


if (fit_or_load == "fit")  {
  l_results_mixture_0var_all <- list()
  for (i in 1:n_reps) {
    l_results_mixture_0var <- pmap(
      tbl_params_mixture, kalman_mixture_experiment,
      lambda = lambda, nr_vars = 0
    )
    l_results_mixture_0var_all[[i]] <- l_results_mixture_0var
    saveRDS(l_results_mixture_0var_all, "exploration-R/data/recovery-mixture-no-variance.RDS")
  }
} else if (fit_or_load == "load")  {
  l_results_mixture_0var <- readRDS("exploration-R/data/recovery-mixture-no-variance.RDS")
}


l_results_mixture_0var <- map2(
  l_results_mixture_0var_all, 1:length(l_results_mixture_0var_all), 
  ~ map2(.x, rep(.y, length(.x)), add_repl_id)
)
l_results_mixture_0var <- reduce(l_results_mixture_0var, c)

counter <- 1
counter_design <- 1
l_results_mix_0var <- list()
for (tbl_r in l_results_mixture_0var) {
  l_results_mix_0var[[counter]] <- as_tibble(cbind(
    tbl_r %>% 
      unnest_wider(params_decision) %>%
      select(-c(simulate_data, nr_trials)), tbl_params_mixture[counter_design, ]
  ))
  counter <- counter + 1
  counter_design <- counter %% nrow(tbl_params_mixture)
  counter_design[counter_design == 0] <- nrow(tbl_params_mixture)
}

tbl_cor_mixture_0var <- reduce(l_results_mix_0var, rbind) %>%
  group_by(replication_id, gamma_mn, beta_mn, w_mix_mn, simulate_data, nr_trials) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_beta = cor(beta, beta_ml),
    r_w_mix = cor(w_mix, w_mix_ml),
    r_gamma_beta = cor(gamma_ml, beta_ml),
    r_gamma_w_mix = cor(gamma_ml, w_mix_ml),
    r_beta_w_mix = cor(beta_ml, w_mix_ml)
  ) %>% ungroup()

tbl_cor_mixture_0var_long <- tbl_cor_mixture_0var %>% 
  mutate(
    simulate_data = factor(simulate_data),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE"),
    nr_trials = factor(nr_trials, labels = c("200 Trials", "400 Trials")),
    w_mix_mn = factor(w_mix_mn, labels = c("w = .5", "w = .75")),
    beta_mn = factor(beta_mn, labels = c("Beta = .17", "Beta = 1.5"))
  ) %>% 
  rename(
    "Gamma" = r_gamma,
    "Beta" = r_beta,
    "w_mix" = r_w_mix
  ) %>%
  pivot_longer(cols = c(Gamma, Beta, w_mix))
# 
# pd <- position_dodge(width = .9)
# plot_cor_recovery(tbl_cor_mixture_0var_long, pd, "ucb") +
#   facet_grid(interaction(simulate_data, name) ~ interaction(beta_mn, w_mix_mn))

plot_ru_thompson_param_recovery <- function(param, ttl) {
  ggplot(tbl_cor_mixture_0var_long %>% filter(name == param), aes(as.factor(gamma_mn), value)) +
    geom_hline(yintercept = .5, color = "grey", linetype = "dotdash") +
    geom_hline(yintercept = 1, color = "grey", linetype = "dotdash") +
    geom_violin(alpha = .25) +
    geom_quasirandom(aes(color = nr_trials), method = "quasirandom", cex = 1.75, alpha = .5, width = .1) +
    facet_grid(interaction(beta_mn, w_mix_mn) ~ interaction(simulate_data, nr_trials, sep = " & ")) +
    coord_cartesian(ylim = c(-1, 1)) +
    theme_bw() +
    scale_x_discrete(expand = c(.02, .02)) +
    scale_y_continuous(expand = c(.01, .01)) +
    scale_color_viridis_d(name = "Nr. Trials") +
    labs(x = "Gamma Mean Gen.", y = "Correlation In - Out", title = ttl) + 
    theme(strip.background = element_rect(fill = "white"))
}
plot_ru_thompson_param_recovery("Gamma", "Gamma (inv. temp.)")
plot_ru_thompson_param_recovery("Beta", "Beta (inf. bonus)")
plot_ru_thompson_param_recovery("w_mix", "Mixture (Thompson)")




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
nr_trials <- c(200, 400)
cond_on_choices <- c(TRUE)
is_decay <- c(FALSE, TRUE)

tbl_params_delta <- crossing(
  tbl_gammas, tbl_deltas, simulate_data, nr_participants, 
  nr_trials, cond_on_choices, is_decay
)

if (fit_or_load == "fit")  {
  l_results_delta_softmax_all <- list()
  for (i in 1:n_reps) {
    l_results_delta_softmax <- pmap(
      tbl_params_delta, delta_experiment,
      lambda = lambda
    )
    l_results_delta_softmax_all[[i]] <- l_results_delta_softmax
    saveRDS(l_results_delta_softmax_all, "exploration-R/data/recovery-delta-softmax.RDS")
  }
} else if (fit_or_load == "load")  {
  l_results_delta_softmax <- readRDS("exploration-R/data/recovery-delta-softmax.RDS")
}


l_results_delta_softmax <- map2(
  l_results_delta_softmax, 1:length(l_results_delta_softmax), 
  ~ map2(.x, rep(.y, length(.x)), add_repl_id)
)
l_results_delta_softmax <- reduce(l_results_delta_softmax, c)

counter <- 1
counter_design <- 1
l_results_c_delta_softmax <- list()
for (tbl_r in l_results_delta_softmax) {
  l_results_c_delta_softmax[[counter]] <- as_tibble(cbind(
    tbl_r %>% 
      unnest_wider(params_decision) %>%
      select(-c(simulate_data, nr_trials, is_decay)), tbl_params_delta[counter_design, ]
  ))
  counter <- counter + 1
  counter_design <- counter %% nrow(tbl_params_delta)
  counter_design[counter_design == 0] <- nrow(tbl_params_delta)
}

tbl_cor_c_delta_softmax <- reduce(l_results_c_delta_softmax, rbind) %>%
  group_by(replication_id, delta_mn, gamma_mn, simulate_data, nr_trials, is_decay) %>%
  summarize(
    r_gamma = cor(gamma, gamma_ml),
    r_delta = cor(delta, delta_ml)
  ) %>% ungroup()

tbl_cor_delta_softmax_long <- tbl_cor_c_delta_softmax %>% 
  mutate(
    simulate_data = factor(simulate_data),
    is_decay = factor(is_decay),
    simulate_data = fct_recode(simulate_data, "Simulate By Participant" = "TRUE", "Simulate Once" = "FALSE"),
    is_decay = fct_recode(is_decay, "Decay Rule" = "TRUE", "Delta Rule" = "FALSE"),
    nr_trials = factor(nr_trials, labels = c("200 Trials", "400 Trials")),
    delta_mn = fct_recode(factor(delta_mn), "Delta = .55" = "0.55", "Delta = .85" = "0.85"),
    gamma_mn = fct_recode(factor(gamma_mn), "Gamma = .08" = "0.08", "Gamma = .16" = "0.16")
  ) %>% 
  rename(
    "Gamma" = r_gamma,
    "Delta" = r_delta
  ) %>%
  pivot_longer(cols = c(Gamma, Delta))


ggplot(
  tbl_cor_delta_softmax_long %>% 
    filter(gamma_mn %in% c("Gamma = .08", "Gamma = .16") & 
             simulate_data == "Simulate By Participant"
           ), 
  aes(gamma_mn, value, group = interaction(nr_trials, name))
) +
  geom_hline(yintercept = 1, linetype = "dotdash", color = "grey") +
  geom_hline(yintercept = .5, linetype = "dotdash", color = "grey") +
  geom_line(aes(group = interaction(nr_trials, name))) +
  facet_grid(delta_mn ~ is_decay) +
  geom_point(aes(color = nr_trials, shape = name)) +
  theme_bw() +
  scale_x_discrete(expand = c(.01, .01)) +
  scale_y_continuous(expand = c(.01, 0)) +
  labs(x = "", y = "")

pd <- position_dodge(width = .9)
plot_cor_recovery(tbl_cor_delta_softmax_long, pd, "softmax") +
  facet_grid(interaction(delta_mn, name) ~ interaction(simulate_data, is_decay))


plot_delta_param_recovery <- function(dc, param, ttl) {
  ggplot(
    tbl_cor_delta_softmax_long %>% filter(is_decay == dc & name == param),
    aes(as.factor(gamma_mn), value)) +
    geom_hline(yintercept = .5, color = "grey", linetype = "dotdash") +
    geom_hline(yintercept = 1, color = "grey", linetype = "dotdash") +
    geom_violin(alpha = .25) +
    geom_quasirandom(aes(color = nr_trials), method = "quasirandom", cex = 1.75, alpha = .5, width = .1) +
    facet_grid(interaction(simulate_data, nr_trials, sep = " & ") ~ delta_mn) +
    coord_cartesian(ylim = c(-1, 1)) +
    theme_bw() +
    scale_x_discrete(expand = c(.02, .02)) +
    scale_y_continuous(expand = c(.01, .01)) +
    scale_color_viridis_d(name = "Nr. Trials") +
    labs(x = "Gamma Mean Gen.", y = "Correlation In - Out", title = ttl) + 
    theme(strip.background = element_rect(fill = "white")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_delta_param_recovery("Delta Rule", "Gamma", "Delta Rule: Parameter = Gamma (inv. temp.)")
plot_delta_param_recovery("Delta Rule", "Delta", "Delta Rule: Parameter = Delta (learning rate)")

plot_delta_param_recovery("Decay Rule", "Gamma", "Decay Rule: Parameter = Gamma (inv. temp.)")
plot_delta_param_recovery("Decay Rule", "Delta", "Decay Rule: Parameter = Delta (learning rate)")




# Summarize Parameter Recoveries of All Models ----------------------------


recovery_simulated_summary <- function(gamma_val) {
  tbl_sm0 <- tbl_cor_softmax_0var %>% 
    filter(gamma_mn == gamma_val & simulate_data & nr_trials == 300) %>% 
    select(-nr_participants) %>%
    mutate(model = "Kalman Softmax")
  tbl_ucb0 <- tbl_cor_ucb_0var %>% 
    filter(gamma_mn == gamma_val & simulate_data & beta_mn == .17 & nr_trials == 300) %>%
    mutate(model = "Kalman UCB")
  tbl_ru_thompson <- tbl_cor_ru_thompson_0var %>%
    filter(gamma_mn == gamma_val & simulate_data & beta_mn == .17 & nr_trials == 300 & w_mix_mn == .5) %>%
    mutate(model = "Kalman RU & Thompson")
  tbl_delta_sm <- tbl_cor_c_delta_softmax %>% 
    filter(gamma_mn == gamma_val & simulate_data & nr_trials == 200) %>%
    filter(!is_decay & delta_mn == .55) %>%
    mutate(model = "Delta")
  tbl_decay_sm <- tbl_cor_c_delta_softmax %>% 
    filter(gamma_mn == gamma_val & simulate_data & nr_trials == 300) %>%
    filter(is_decay & delta_mn == .55) %>%
    mutate(model = "Decay")
  bind_rows(tbl_sm0, tbl_ucb0, tbl_ru_thompson, tbl_delta_sm, tbl_decay_sm) %>%
    select(nr_trials, model, r_gamma, r_beta, r_delta, r_w_mix)
}

tbl_1 <- recovery_simulated_summary(.16) %>% select(-nr_trials)


tbl_1_long <- tbl_1 %>% pivot_longer(-model)
tbl_1_long$name <- fct_inorder(factor(tbl_1_long$name))
levels(tbl_1_long$name) <- c("Gamma", "Beta", "Delta", "w_mix")
pl_heatmap_cherry <- ggplot(tbl_1_long, aes(name, model)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2)), color = "black") +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient2(high = "aquamarine2", low = "tomato", mid = .5, guide = "none") +
  labs(x = "", y = "")



plot_landscape_with_cherry <- function(rule, gamma_cherry) {
  tbl_plot <- tbl_cor_delta_softmax_long %>%
    filter(simulate_data == "Simulate By Participant" & is_decay == rule) %>%
    mutate(delta_mn = as.factor(delta_mn))
  
  tbl_cherry <- tbl_plot %>% filter(gamma_mn == gamma_cherry & nr_trials == 200 & delta_mn == .55)
  
  pl_landscape <- ggplot(tbl_plot %>% mutate(nr_trials = factor(nr_trials)), aes(gamma_mn, value, group = interaction(delta_mn, nr_trials))) +
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
    scale_color_manual(values = c("slateblue", "lightblue"), name = "Nr. Trials") +
    scale_shape_discrete(name = "Mean Delta") +
    scale_linetype_discrete(guide = "none") +
    coord_cartesian(ylim = c(0, 1)) +
    geom_point(data = tbl_cherry, shape = 0, size = 3)
  
  return(pl_landscape)
  
}



pl_landscape_delta <- plot_landscape_with_cherry("Delta Rule", .16)
pl_landscape_decay <- plot_landscape_with_cherry("Decay Rule", .08)

grid.draw(arrangeGrob(
  pl_heatmap_cherry + ggtitle("Parameter Recovery (Cherries)"), 
  pl_landscape_delta + theme(legend.position = "none") + ggtitle("Delta Model"), 
  pl_landscape_decay + ggtitle("Decay Model")
  , nrow = 1, widths = c(1, 1, 1.15))
)

