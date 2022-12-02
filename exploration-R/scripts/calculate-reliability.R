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

