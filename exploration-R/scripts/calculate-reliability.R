library(tidyverse)
library(MASS)
library(cmdstanr)
library(loo)

# home grown
dirs_home_grown <- c("exploration-R/utils/utils.R", "exploration-R/utils/plotting.R")
walk(dirs_home_grown, source)


n_subjects <- 80
n_trials <- 20
n_timepoints <- 2

d_r <- 1.5
a_r <- c(0 - d_r/2, 0 + d_r/2) # fixed effect over time

sig_sq_0 <- 1 # error variance
var_subj_t1 <- .5
var_subj_t2 <- .5
reliability <- .8
cov_t1_t2 <- reliability * sqrt(var_subj_t1) * sqrt(var_subj_t2)

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
tbl_sim_long <- pivot_longer(tbl_sim, c(t1, t2), names_to = "timepoint", values_to = "y")

plot_some_subjects(tbl_sim_long)


# fit Bayesian reliability model ------------------------------------------


stan_normal_rel <- stan_normal_reliability()
mod_normal_rel <- cmdstan_model(stan_normal_rel)


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

fit_normal_rel <- mod_normal_rel$sample(
  data = l_data, iter_sampling = 200, iter_warmup = 200, chains = 1
)


file_loc <- str_c("exploration-R/data/recovery-normal.RDS")
fit_normal_rel$save_object(file = file_loc)
pars_interest <- c("mu_ic", "mu_time", "Sigma")
tbl_draws <- fit_normal_rel$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_normal_rel$summary(variables = pars_interest)

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu", "Sigma[2,1]")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu", "Sigma[2,1]")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = c("Intercept", "Time", "Reliability")))

loo_normal_rel <- fit_normal_rel$loo(variables = "log_lik_pred")



stan_normal_reliability <- function() {
  
  stan_normal_reliability <- write_stan_file("
data {
  int n_data;
  int n_subj;
  vector[n_data] response;
  array[n_data] int subj;
  array[n_data] int x; // timepoint
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  cholesky_factor_corr[2] L; //cholesky factor of covariance
  vector<lower=0>[2] L_std;
  real<lower=0> sigma_error;
  real mu_ic;
  real mu_time;
  matrix[n_subj, 2] tau_rs;
  vector[2] mu_subject;
}

transformed parameters {
  vector[n_data] mu_rs;
  
  for (n in 1:n_data) {
    mu_rs[n] = mu_ic + mu_time * (x[n] - 1.5) + tau_rs[subj[n], x[n]];
  }
}

model {
  for (n in 1:n_data) {
    response[n] ~ normal(mu_rs[n], sigma_error);
    
  }

  L ~ lkj_corr_cholesky(1);
  L_std ~ normal(0, 2.5);
  matrix[2, 2] L_Sigma = diag_pre_multiply(L_std, L);
  

  for (s in 1:n_subj) {
    tau_rs[s] ~ multi_normal_cholesky(mu_subject, L_Sigma);
  }
  
  sigma_error ~ gamma(1, 1);
  mu_ic ~ normal(0, 1);
  mu_time ~ normal(0, 1);
  mu_subject ~ normal(0, 1);
}
 
  
generated quantities {
 corr_matrix[2] Sigma;
 array[n_data] real log_lik_pred;

 Sigma = multiply_lower_tri_self_transpose(L);

 for (n in 1:n_data) {
   log_lik_pred[n] = normal_lpdf(response[n] | mu_rs[n], sigma_error);
 }
}

")
  return(stan_normal_reliability)
}



