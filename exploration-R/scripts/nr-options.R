# part of this script is taken from Maarten Speekenbrink's blogpost
# on fitting RL models 
# see https://speekenbrink-lab.github.io/modelling/2019/02/28/fit_kf_rl_1.html
# for part 1

# load packages and libraries
library(tidyverse)

paths_local <- c("utils/utils.R")
walk(paths_local, source)

# simulate data for conditions with different nr of response options
# means and sds are fixed over trials (not restless)

# conditions
n_conditions <- 3
mn_spacing <- 1
var_fixed <- 5

# experiment
n_trials <- 200

tbl_conditions <- conditions_and_rewards_fixed_means(
  n_conditions, mn_spacing, var_fixed, n_trials
)

# plot reward distributions as a check

check_reward_distributions(tbl_conditions)

# run the model for all conditions
l_l_m <- map(tbl_conditions$rewards, rl_softmax_sim, m0 = 0, v0 = var_fixed, 
             sigma_epsilon_sq = var_fixed, gamma = 1)

# plot some of the results
plot_mean_trajectories(l_l_m)




# fit models to maximize rewards
# learning rules: kf or decay rule
# different number of response options



