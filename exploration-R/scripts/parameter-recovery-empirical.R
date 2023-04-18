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

tbl_rewards <- tibble(NA)

tbl_rb %>% count(id2)

fit_softmax_no_variance_wrapper(tbl_results = tbl_rb %>% filter(id2 == 3), tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE)
fit_softmax_one_variance_wrapper(tbl_results = tbl_rb %>% filter(id2 == 3), tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE)


# model fitting -----------------------------------------------------------

l_participants <- tbl_rb %>% split(., .$"id2")


# candidate models
# softmax no variance
plan(multisession, workers = 4)#availableCores() - 2)
l_kalman_softmax_no_variance <- furrr::future_map(
  l_participants, fit_softmax_no_variance_wrapper, 
  tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE
  )
tbl_kalman_softmax_no_variance <- reduce(l_kalman_softmax_no_variance, rbind) %>%
  as.data.frame() %>% as_tibble()

# ucb with softmax
plan(multisession, workers = 4)#availableCores() - 2)
l_kalman_ucb_no_variance <- furrr:future_map(
  l_participants, fit_ucb_no_variance_wrapper,
  tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE
)
tbl_kalman_ucb_no_variance <- reduce(l_kalman_ucb_no_variance, rbind) %>%
  as.data.frame() %>% as_tibble()

tbl_kalman_ucb_no_variance %>% 
  pivot_longer(c(V1, V2)) %>%
  ggplot(aes(value, group = name)) +
  geom_histogram(aes(fill = name))

# thompson sampling with one variance?
plan(multisession, workers = 4)#availableCores() - 2)
l_kalman_thompson_one_variance <- furrr::future_map(
  l_participants, fit_thompson_one_variance_wrapper,
  tbl_rewards = tbl_rewards, condition_on_observed_choices = TRUE
)
tbl_kalman_thompson_one_variance <- reduce(l_kalman_thompson_one_variance, rbind) %>%
  as.data.frame() %>% as_tibble()



# delta rule learning
plan(multisession, workers = 4)#availableCores() - 2)
l_delta_softmax <- furrr::future_map(
  l_participants, fit_delta_softmax_wrapper,
  tbl_rewards = tbl_rewards, is_decay = FALSE, condition_on_observed_choices = TRUE,
  .progress = TRUE
)
tbl_delta_softmax <- reduce(l_delta_softmax, rbind) %>%
  as.data.frame() %>% as_tibble()

tbl_delta_softmax %>% 
  pivot_longer(c(V1, V2)) %>%
  ggplot(aes(value, group = name)) +
  geom_histogram(aes(fill = name))


# decay rule learning
plan(multisession, workers = 4)#availableCores() - 2)
l_decay_softmax <- furrr::future_map(
  l_participants, fit_delta_softmax_wrapper,
  tbl_rewards = tbl_rewards, is_decay = TRUE, condition_on_observed_choices = TRUE
)
tbl_decay_softmax <- reduce(l_decay_softmax, rbind) %>%
  as.data.frame() %>% as_tibble()
tbl_decay_softmax %>% 
  pivot_longer(c(V1, V2)) %>%
  ggplot(aes(value, group = name)) +
  geom_histogram(aes(fill = name))

# generate data given models ----------------------------------------------



tmp <- delta_learning(tbl_rb %>% filter(id2 == 3), 4, .99323, NULL, TRUE)


# recover models ----------------------------------------------------------


