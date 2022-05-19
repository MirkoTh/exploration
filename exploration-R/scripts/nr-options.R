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
n_conditions <- 3
mn_spacing <- 1
var_fixed <- 5

# individual experiment parameters
n_trials <- 200
n_rep <- 2


# Run Experiments Once ----------------------------------------------------

tbl_conditions <- conditions_and_rewards_fixed_means(
  n_conditions, mn_spacing, var_fixed, n_trials
)

# plot reward distributions as a check

check_reward_distributions(tbl_conditions)

# run the model for all conditions using one gamma value
params <- list(
  gamma = .3,
  eta = .8,
  model = "Decay"
)
l_l_m <- map(tbl_conditions$rewards, rl_softmax_sim, m0 = 0, v0 = var_fixed, 
             sigma_epsilon_sq = var_fixed, params = params)

# plot some of the results
plot_mean_trajectories(l_l_m)



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
  grouped_agg(c(model, gamma, eta, n_options), reward_tot) %>%
  relocate(n_options, .after = model)

tbl_results_max <- tbl_results_agg %>% group_by(model, n_options) %>%
  mutate(rwn = row_number(desc(mean_reward_tot))) %>%
  filter(rwn == 1)

plot_total_rewards_kalman(tbl_results_agg)
plot_total_rewards_decay(tbl_results_agg)

