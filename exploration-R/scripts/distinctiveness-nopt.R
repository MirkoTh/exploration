library(tidyverse)
library(rutils)


n_trials <- 12
n_options_max <- 4
ri <- 2
v_means <- seq(1, n_options_max, by = 2)
v_sd <- c(1)
conditions <- fct_inorder(c("Massed", "Distinct I", "Distinct II"), ordered = TRUE)

# set up conditions table
tbl_2 <- tibble(
  trial_id = rep(
    seq(1, n_trials, by = 1), length(conditions)),
  condition = rep(conditions, each = n_trials),
  option_selected = factor(c(
    rep(c(1, 2), each = n_trials/2),
    rep(c(1, 2), n_trials/2),
    c(1, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1)
  )),
  value_sampled = rnorm(v_means[option_selected], v_sd),
  distance_t = abs(n_trials - trial_id) + ri,
  distance_t_log = log(distance_t)
)
tbl_2 <- tbl_2 %>% group_by(condition, option_selected) %>%
  arrange(trial_id) %>%
  mutate(trial_id_option_bw = row_number(desc(trial_id))) %>%
  ungroup() %>% arrange(condition, option_selected)
# 
l_tbl_2 <- split(tbl_2, interaction(tbl_2$option_selected, tbl_2$condition))
l_idx <- as.list(rep(c(1:nrow(l_tbl_2[[1]])), length(l_tbl_2)))
l_map <- list(l_idx, rep(l_tbl_2, each = length(l_tbl_2)))
calc_discriminability <- function(n, tb, c, alpha) {
  distances_all <- tb$distance_t_log
  # map over all distances besides n
  all_positions <- seq(1, length(distances_all), by = 1)
  1/sum(map_dbl(distances_all, ~ exp(-c*abs(distances_all[n] - .x)^alpha)))
}

tbl_2$discriminability <- pmap_dbl(l_map, calc_discriminability, 1, 1)

x11()
ggplot(tbl_2, aes(trial_id, as.numeric(as.character(option_selected)), group = condition)) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 2) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = condition)) +
  facet_wrap(~ condition) +
  theme_bw() +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_x_continuous(breaks = seq(1, n_trials, by = 2)) +
  #scale_y_continuous(breaks = c(1, 2)) +
  coord_cartesian(ylim = c(0, 3)) +
  labs(
    x = "Trial ID",
    y = "Forced Choice Option"
  )

# plot discriminability of last three items, which we are going to ask people for
tbl_last_3 <- tbl_2 %>% filter(trial_id_option_bw <= 3)
ggplot(tbl_2, aes(trial_id, discriminability, group = condition)) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = condition)) +
  facet_wrap(option_selected ~ condition) +
  theme_bw() +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_x_continuous(breaks = seq(1, n_trials, by = 2)) +
  labs(
    x = "Trial ID",
    y = "Discriminability in Memory"
  )

pd <- position_dodge(width = .95)
grouped_agg(tbl_last_3, c(condition, option_selected), discriminability) %>%
  ggplot(aes(condition, mean_discriminability, group = option_selected)) +
  geom_col(aes(fill = condition), position = pd, alpha = .5) +
  geom_point(aes(color = condition), position = pd) +
  facet_wrap(~ option_selected) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    x = "Condition",
    y = "Mean Discriminability",
    title = "Last Three Items Per Response Option"
  )
