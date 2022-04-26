library(tidyverse)
library(MASS)
library(rutils)

ic_slope <- function(x) {
  intercept <- 0
  slope <- 1
  tbl <- tibble(
    x_vals = seq(1, 20, length.out = 20),
    noise = rnorm(20, 0, 1),
    y_vals = intercept + slope * x_vals + noise
  )
  m1 <- lm(y_vals ~ x_vals, tbl)
  s <- summary(m1)
  return(c(s$coefficients[1, 1], s$coefficients[2, 1]))
}

v_iter <- seq(1, 100)
l_params <- map(v_iter, ic_slope)
tbl_params <- tibble(as.data.frame(reduce(l_params, rbind)))
names(tbl_params) <- c("Intercept", "Slope")
c <- round(cor(tbl_params$Intercept, tbl_params$Slope), 2)

ggplot(tbl_params, aes(Intercept, Slope)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", color = "purple") +
  geom_label(aes(-.8, .95, label = str_c("Correlation = ", c))) +
  theme_bw()

n_trials <- 100
p_decay <- .9836
p_drift_mn <- .35

# manipulate means
tbl_mns <- tibble(
  trial_id = seq(1, n_trials, by = 1),
  mn_1 = rep(0, n_trials),
  mn_2 = rep(3, n_trials),
  mn_1_ds = rep(0, n_trials),
  mn_2_ds = rep(3, n_trials),
  mn_1_dv = rep(0, n_trials),
  mn_2_dv = rep(3, n_trials),
)
for (i in 2:n_trials) {
  tbl_mns[i, "mn_1_ds"] <- tbl_mns[i-1, "mn_1_ds"] * p_decay + rnorm(1, 0, 1)
  tbl_mns[i, "mn_2_ds"] <- tbl_mns[i-1, "mn_2_ds"] * p_decay + rnorm(1, 0, 1)
  tbl_mns[i, "mn_1_dv"] <- tbl_mns[i-1, "mn_1_dv"] * p_decay + p_drift_mn + rnorm(1, 0, 1)
  tbl_mns[i, "mn_2_dv"] <- tbl_mns[i-1, "mn_2_dv"] * p_decay - p_drift_mn + rnorm(1, 0, 1)
}
tbl_mns_long <- tbl_mns %>% pivot_longer(
  c(mn_1, mn_2, mn_1_ds, mn_2_ds, mn_1_dv, mn_2_dv),
  names_to = "Arm", values_to = "Mean"
) %>% mutate(
  Arm = fct_inorder(Arm),
  Arm = fct_relabel(
    Arm, ~ c(
      "Stable 1", "Stable 2", "RW Stable 1", 
      "RW Stable 2", "RW Varying 1", "RW Varying 2"
    )),
  rw = factor(str_detect(Arm, "RW"), labels = c("No RW", "RW")),
  stability = factor(str_detect(Arm, "Stable"), labels = c("Varying", "Fixed")),
  arm_nr = factor(rep(c(1, 2), n_trials * 3))
)
plot_mns_over_trials <- function(tbl_mns_long) {
  ggplot(tbl_mns_long %>% filter(trial_id <= 50), aes(trial_id, Mean, group = Arm)) +
  geom_line(aes(color = arm_nr)) +
  geom_point(color = "White", size = 3) +
  geom_point(aes(color = arm_nr), shape = 1, alpha = .25) +
  scale_color_brewer(name = "Arm Nr.", palette = "Set1") +
  theme_bw() +
  facet_grid(rw ~ stability) +
  labs(
    x = "Trial ID",
    y = "Mean of Bandit"
  )
}
plot_mns_over_trials(tbl_mns_long) +
  geom_label(
    data = tibble(
      Arm = "Stable 1",
      Mean = 3,
      trial_id = 1, 
      rw = "No RW",
      stability = "Varying"
    ), aes(25, 5, label = str_c("Correlation = ", 0)), size = 3
  )

# manipulate covariances
m_cov <- matrix(c(1, .8, .8, 1), nrow = 2, byrow = TRUE)

for (i in 2:n_trials) {
  mns_prev_f <- c(tbl_mns[i-1, "mn_1_ds"], tbl_mns[i-1, "mn_2_ds"]) %>% as_vector()
  mns_prev_v <- c(
    tbl_mns[i-1, "mn_1_dv"] + p_drift_mn, 
    tbl_mns[i-1, "mn_2_dv"] - p_drift_mn
    ) %>% as_vector()
  y_vals_f <- mvrnorm(1, mns_prev_f * p_decay, m_cov)
  y_vals_v <- mvrnorm(1, mns_prev_v * p_decay, m_cov)
  tbl_mns[i, "mn_1_ds"] <- y_vals_f[1]
  tbl_mns[i, "mn_2_ds"] <- y_vals_f[2]
  tbl_mns[i, "mn_1_dv"] <- y_vals_v[1]
  tbl_mns[i, "mn_2_dv"] <- y_vals_v[2]
}
tbl_mns_long_cov <- tbl_mns %>% pivot_longer(
  c(mn_1, mn_2, mn_1_ds, mn_2_ds, mn_1_dv, mn_2_dv),
  names_to = "Arm", values_to = "Mean"
) %>% mutate(
  Arm = fct_inorder(Arm),
  Arm = fct_relabel(
    Arm, ~ c(
      "Stable 1", "Stable 2", "RW Stable 1", 
      "RW Stable 2", "RW Varying 1", "RW Varying 2"
    )),
  rw = factor(str_detect(Arm, "RW"), labels = c("No RW", "RW")),
  stability = factor(str_detect(Arm, "Stable"), labels = c("Varying", "Fixed")),
  arm_nr = factor(rep(c(1, 2), n_trials * 3))
)

plot_mns_over_trials(tbl_mns_long_cov) +
  geom_label(
    data = tibble(
      Arm = "Stable 1",
      Mean = 3,
      trial_id = 1, 
      rw = "No RW",
      stability = "Varying"
      ), aes(25, -3, label = str_c("Correlation = ", m_cov[1, 2])), size = 3
  ) +
  geom_label(
    data = tibble(
      Arm = "Stable 2",
      Mean = 3,
      trial_id = 1, 
      rw = "RW",
      stability = "Varying"
    ), aes(25, -3, label = "possible, but likely\ntoo difficult"),
    size = 3, alpha = .5, color = "gray"
  )

# two reward distributions for a 2-a-bdt
tbl_2ab <- tibble(
  x = c(rnorm(3, 0, 1), rnorm(7, 1, 1)),
  Arm = factor(c(rep(1, 3), rep(2, 7)))
)
tbl_2ab_agg <- tbl_2ab %>%
  grouped_agg(Arm, x) %>% mutate(y = c(.0035, .0175))
pl_base <- ggplot() + xlim(-3, 5)
pl_base + 
  geom_function(fun = ~ dnorm(.x, 1, 1), colour = "#377EB8") + 
  geom_function(fun = ~ dnorm(.x, 0, 1), colour = "#E41A1C") +
  geom_errorbar(data = tbl_2ab_agg, aes(
    y = .01, xmin = mean_x - 1.96 * se_x, xmax = mean_x + 1.96 * se_x,
    color = Arm), width = .025, position = "dodge") +
  theme_bw() +
  labs(
    x = "x", y = "Probability Density" 
  ) + geom_rug(
    data = tbl_2ab, aes(x, group = Arm, color = Arm),
    size = 1, length = unit(0.025, "npc")
    ) + geom_point(
      data = tbl_2ab_agg, aes(
        y = y, x = mean_x, group = Arm
        ), color = "white", size = 3
      ) + 
  geom_point(
    data = tbl_2ab_agg, aes(y = y, x = mean_x, color = Arm)) +
  scale_color_brewer(palette = "Set1")
  
  