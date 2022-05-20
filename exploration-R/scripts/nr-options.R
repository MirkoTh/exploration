# part of this script is taken from Maarten Speekenbrink's blogpost
# on fitting RL models 
# see https://speekenbrink-lab.github.io/modelling/2019/02/28/fit_kf_rl_1.html
# for part 1

# load packages and libraries

library(tidyverse)
library(furrr)
library(rutils)

paths_local <- c("utils/utils.R", "utils/plotting.R")
walk(paths_local, source)

# simulate data for conditions with different nr of response options
# means and sds are fixed over trials (not restless)

# simulation experiment conditions
opts <- seq(2, 6, by = 2) # nr of response options
mn_spacing <- 1
vars <- c(1, 4)

# individual experiment parameters
n_trials <- 200
n_rep <- 200


# Run Experiments Once ----------------------------------------------------

tbl_conditions <- make_conditions_and_rewards(opts, mn_spacing, vars, n_trials)


# plot reward distributions as a check

check_reward_distributions(tbl_conditions)

# run the model for all conditions using one gamma value
params <- list(
  gamma = .3,
  eta = .8,
  model = "Kalman"
)
l_l_m <- pmap(
  list(tbl_conditions$rewards, as.list(tbl_conditions$var)), 
  rl_softmax_sim,
  m0 = 0, params = params
)

# plot some of the results
plot_mean_trajectories(l_l_m, tbl_conditions)



# Iterate over Experiment Runs --------------------------------------------


n_workers_use <- future::availableCores() / 2
plan(multisession, workers = n_workers_use)
suppressMessages(
  l_tbl_results <- future_map(
    1:n_rep, iterate_once, .progress = TRUE,
    .options = furrr_options(seed = NULL)
  )
)

# Analyze Results ---------------------------------------------------------


tbl_results_agg <- l_tbl_results %>% reduce(rbind) %>%
  grouped_agg(c(model, gamma, eta, n_options, var), reward_tot) %>%
  relocate(n_options, .after = model) %>% relocate(var, .after = n_options) %>% ungroup()

eta_speek_konst <- c(.6, 1) # approximately in speekenbrink & konstantinidis (2015)

# some plots visualizing parameters optimizing rewards
plot_total_rewards_kalman(tbl_results_agg)
plot_total_rewards_decay(tbl_results_agg)
plot_total_rewards_decay(tbl_results_agg, p_eta = eta_speek_konst[2])


tbl_results_max <- tbl_results_agg %>% 
  group_by(model, n_options, var, eta) %>%
  mutate(rwn = row_number(desc(mean_reward_tot))) %>%
  filter(
    rwn == 1 & near(eta, eta_speek_konst[1]) |
      rwn == 1 & near(eta, eta_speek_konst[2]) |
      rwn == 1 & model == "Kalman"
  )

plot_optimal_gammas(tbl_results_max)
