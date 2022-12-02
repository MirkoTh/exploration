library(tidyverse)
library(MASS)


n_subjects <- 80
n_trials <- 20
n_timepoints <- 2

d_r <- 1.5
a_r <- c(0 - d_r/2, 0 + d_r/2) # fixed effect over time

sig_sq_0 <- 1 # error variance
var_subj_t1 <- .5
var_subj_t2 <- .5
cov_t1_t2 <- reliability * sqrt(var_subj_t1) * sqrt(var_subj_t2)
reliability <- .8

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

# plot subset of subjects as a check
ggplot(
  tbl_sim_long %>% 
    filter(subject %in% sample(1:n_subjects, 5, replace = FALSE)),
  aes(timepoint, y, group = as.factor(subject))
  ) + geom_point(
    aes(color = as.factor(subject)), position = position_dodge(width = .2)
    ) + scale_color_viridis_d(name = "Subject ID") +
  theme_bw() +
  labs(x = "Timepoint", y = "y")



sample_y <- function(subj, mu_t1, mu_t2, var, n) {
  y_t1 <- rnorm(n, mu_t1, var)
  y_t2 <- rnorm(n, mu_t2, var)
  tibble(
    subject = subj,
    t1 = y_t1,
    t2 = y_t2
  )
}



stan_sim <- function() {
  
  stan_normal_sim <- write_stan_file("
data {
  int n_data;
  int n_subj;
  vector[n_data] response;
  array[n_data] int subj;
  matrix[n_data, 2] x; // ic, timepoint
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  matrix[n_subj, 2] b;
  vector[2] mu;
  vector <lower=0>[2] sigma_subject;
  real<lower=0> sigma;
}

transformed parameters {
  array[2] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = scale_cont * mu[2];
  vector[n_data] mn;

  for (n in 1:n_data) {
    mn[n] = b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2];
  }
}

model {
  for (n in 1:n_data) {
    response[n] ~ normal(mu_rs[n], sigma_error);
    mu_rs[n] = mu_ic + mu_time * (x[n, 2] - 1.5) + tau_rs[subj[n], x[n, 2]]
  }

  L ~ lkj_corr_cholesky(1);
  L_std ~ normal(0, 2.5);
  matrix[D, D] L_Sigma = diag_pre_multiply(L_std, L);

  for (s in 1:n_subj) {
    tau_rs[s] ~ multi_normal_cholesky([0, 0], L_Sigma)
  }
  
  sigma_error ~ uniform(0.001, 10);
  sigma_subject[1] ~ uniform(0.001, 10);
  sigma_subject[2] ~ uniform(0.001, 10);
  a_r ~ normal(0, 1);
  mu[2] ~ student_t(1, 0, 1);
}

")
  return(stan_normal_sim)
}



