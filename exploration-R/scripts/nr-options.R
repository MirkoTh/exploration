# part of this script is taken from Maarten Speekenbrink's blogpost
# on fitting RL models 
# see https://speekenbrink-lab.github.io/modelling/2019/02/28/fit_kf_rl_1.html
# for part 1

library(tidyverse)

paths_local <- c("R/utils.R")
walk(paths_local, source)

# simulate data for conditions with different nr of response options
# means and sds are fixed over trials (not restless)

# conditions
n_conditions <- 3
mn_spacing <- 1
sd_fixed <- 2

# experiment
n_trials <- 100

tbl_conditions <- conditions_and_rewards_fixed_means(
  n_conditions, mn_spacing, sd_fixed, n_trials
)
