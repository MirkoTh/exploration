library(tidyverse)
library(MASS)
library(cmdstanr)
library(loo)
library(rutils)

# home grown
dirs_home_grown <- c(
  "exploration-R/utils/utils.R", "exploration-R/utils/plotting.R",
  "exploration-R/utils/stan-models.R"
)
walk(dirs_home_grown, source)

run_model <- FALSE
n_subjects <- 80
n_trials <- 10

d_r <- 1.5
a_r <- c(0 - d_r/2, 0 + d_r/2) # fixed effect over two time points

sig_sq_0 <- .5 # within variance
var_subj_t1 <- 1 # between-subjects variance t1
var_subj_t2 <- 1 # between-subjects variance t2
reliability <- .8 # correlation between task scores at t1 and t2
cov_t1_t2 <- reliability * sqrt(var_subj_t1) * sqrt(var_subj_t2)
var_error <- .5 # error variance

mu_subj <- c(0, 0)
R_bold <- matrix(c(var_subj_t1, cov_t1_t2, cov_t1_t2, var_subj_t2), nrow = 2) # vcov matrix of subject level variability

tau_s <- MASS::mvrnorm(n_subjects, mu_subj, R_bold)

mu_rs <- matrix(rep(a_r, each = n_subjects), ncol = 2) + tau_s
tbl_sample <- tibble(
  subj = 1:nrow(mu_rs),
  mu_t1 = mu_rs[, 1],
  mu_t2 = mu_rs[, 2]
)

tbl_sim <- pmap(tbl_sample, sample_y, var = sig_sq_0, n = n_trials) %>% reduce(rbind)
# add independent random error to both time points
tbl_sim$t1 <- tbl_sim$t1 + rnorm(nrow(tbl_sim), 0, sqrt(var_error))
tbl_sim$t2 <- tbl_sim$t2 + rnorm(nrow(tbl_sim), 0, sqrt(var_error))


tbl_sim_long <- pivot_longer(tbl_sim, c(t1, t2), names_to = "timepoint", values_to = "y")


tbl_sim %>% group_by(subject) %>% summarize(mean(t1), mean(t2)) %>% apply(2, var)

plot_some_subjects(tbl_sim_long)

# between subjects
tbl_sim_long %>% group_by(subject, timepoint) %>%
  summarize(mean_y_s = mean(y)) %>%
  group_by(timepoint) %>%
  summarize(var(mean_y_s))

tbl_sim_long %>% group_by(subject, timepoint) %>%
  summarize(mean_y_s = mean(y)) %>%
  group_by(timepoint) %>%
  mutate(
    mean_tp = mean(mean_y_s),
    squared_dev = (mean_tp - mean_y_s)^2
    ) %>%
  group_by(timepoint) %>%
  summarize(sum_squares_r = sum(squared_dev) / (length(unique(subject)) - 1))
  


# within subjects
tbl_sim_long %>% group_by(timepoint, subject) %>%
  mutate(
    mean_y_subject_tp = mean(y),
    delta = y - mean_y_subject_tp
    ) %>%
  summarize(var_within = var(delta)) %>%
  group_by(timepoint) %>% 
  summarize(mean(var_within))

tbl_sim_long %>% group_by(timepoint, subject) %>%
  mutate(
    mean_y_subject_tp = mean(y),
    squared_dev = (y - mean_y_subject_tp)^2
  ) %>% group_by(timepoint) %>%
  summarize(sum_squares_w = sum(squared_dev) / (length(unique(subject)) * (n_trials - 1)))

tbl_sim %>% group_by(subject) %>% summarize(mn_t1 = mean(t1), mn_t2 = mean(t2)) %>%
  dplyr::select(-subject) %>%
  ICC()

tmp <- tbl_sim %>% dplyr::select(-t2) %>% group_by(subject) %>% mutate(trial = row_number(subject))
tmp %>% pivot_wider(id_cols = subject, names_from = trial, values_from = t1) %>%
  ungroup() %>%
  dplyr::select(-subject) %>%
  ICC()

tbl_sim %>% group_by(subject) %>% summarize(mn_t1 = mean(t1), mn_t2 = mean(t2)) %>%
  dplyr::select(-subject) %>%
  ICC()

# fit/load Bayesian reliability model -------------------------------------


x <- tbl_sim_long$timepoint %>% as.factor() %>% as.numeric()

l_data <- list(
  n_data = nrow(tbl_sim_long),
  n_subj = length(unique(tbl_sim_long$subject)),
  subj = as.numeric(factor(
    tbl_sim_long$subject, 
    labels = 1:length(unique(tbl_sim_long$subject))
  )),
  x = x,
  response = tbl_sim_long$y
)


file_loc <- str_c("exploration-R/data/recovery-normal.RDS")

if (run_model == TRUE) {
  stan_normal_rel <- stan_normal_reliability()
  mod_normal_rel <- cmdstan_model(stan_normal_rel)
  fit_normal_rel <- mod_normal_rel$sample(
    data = l_data, iter_sampling = 200, iter_warmup = 200, chains = 1
  )
  fit_normal_rel$save_object(file = file_loc)
} else {
  fit_normal_rel <- readRDS(file_loc)
}

pars_interest <- c("mu_ic", "mu_time", "Sigma")
tbl_draws <- fit_normal_rel$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_normal_rel$summary(variables = pars_interest)

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu", "Sigma[2,1]")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu", "Sigma[2,1]")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = c("Intercept", "Time", "Reliability")))

loo_normal_rel <- fit_normal_rel$loo(variables = "log_lik_pred")


tbl_descriptive <- grouped_agg(tbl_posterior, parameter, value)

ggplot(tbl_posterior, aes(value)) +
  geom_histogram(binwidth = .1, fill = "#66CCFF", color = "white") +
  geom_label(
    data = tbl_descriptive, 
    aes(0, n/4, label = str_c(round(mean_value, 2), " +/- ", round(se_value, 2)))
  ) + facet_wrap(~ parameter) +
  theme_bw()



# reliability recovery study ----------------------------------------------

n_subjects <- c(40, 80, 120)
n_trials <- c(5, 10, 20)
reliability <- c(.3, .6, .9)


tbl_design <- crossing(n_subjects, n_trials, reliability)

tbl_design_rep <- repeat_tibble(tbl_design, 10)

file_loc_iter <- "exploration-R/data/reliability-recovery-results.RDS"
if (run_model) {
  n_workers_available <- parallel::detectCores()
  future::plan("future::multisession", workers = n_workers_available - 2)
  
  options(warn = -1)
  l_results <- furrr::future_pmap(tbl_design_rep, reliability_pipeline, .progress = TRUE)
  options(warn = 0)
  
  saveRDS(l_results, file = file_loc_iter)
} else {
  l_results <- readRDS(file_loc_iter)
}

tbl_map <- map(l_results, "mean_value") %>% reduce(rbind) %>% as.data.frame() %>% as_tibble()
colnames(tbl_map) <- c("mn_ic", "mn_time", "mn_reliability")
tbl_results_mn <- cbind(tbl_design_rep, tbl_map)

tbl_performance <- tbl_results_mn %>% 
  mutate(delta = abs(reliability - mn_reliability)) %>%
  group_by(n_subjects, n_trials) %>% 
  summarize(
    mn_delta = mean(delta),
    se_delta = sd(delta)/sqrt(n()),
    correlation = cor(reliability, mn_reliability))

pd <- position_dodge(width = 4.5)
ggplot(tbl_performance, aes(n_trials, mn_delta, group = n_subjects)) +
  geom_col(aes(fill = as.factor(n_subjects)), position = pd) +
  geom_point(position = pd) +
  geom_errorbar(
    aes(ymin = mn_delta - 2*se_delta, ymax = mn_delta + 2*se_delta),
    width = 1, position = pd
  ) +
  geom_label(aes(
    y = mn_delta, label = str_c("r=", round(correlation, 2))
  ), position = pd) +
  scale_fill_viridis_d(name = "Nr. Subjects") +
  theme_bw() +
  labs(x = "Nr. Trials", y = "Mean Absolute Deviation")

pd <- position_dodge(width = .05)
grouped_agg(tbl_results_mn, c(n_subjects, n_trials, reliability), mn_reliability) %>% 
  mutate(n_subjects = fct_inorder(str_c(n_subjects, " Subjects"))) %>%
  ggplot(aes(reliability, mean_mn_reliability, group = as.factor(n_trials))) +
  geom_abline(slope = 1, size = 1, linetype = "dotdash", color = "grey") +
  geom_errorbar(aes(
    ymin = mean_mn_reliability - 2*se_mn_reliability, 
    ymax = mean_mn_reliability + 2*se_mn_reliability,
    color = as.factor(n_trials)
  ), width = .05, position = pd) +
  geom_line(aes(color = as.factor(n_trials)), position = pd) +
  geom_point(size = 3, color = "white", position = pd) +
  geom_point(aes(color = as.factor(n_trials)), position = pd) +
  facet_wrap(~ n_subjects) +
  scale_color_viridis_d(name = "Nr. Trials") +
  theme_bw() +
  labs(
    x = "Reliability Input",
    y = "Reliability Output (MAP)"
  )

