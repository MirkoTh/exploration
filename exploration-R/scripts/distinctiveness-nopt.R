library(tidyverse)
library(rutils)

l_utils <- c("exploration-R/utils/utils.R")
walk(l_utils, source)

n_options_max <- 4
params_fixed <- list(
  "n_trials" = 12,
  "n_options_max" = n_options_max,
  "ri" = 2,
  "v_means" = seq(1, n_options_max*2, by = 2),
  "v_sd" = c(1),
  "conditions" = fct_inorder(c("Massed", "Distinct I", "Distinct II"), ordered = TRUE)
)

# set up conditions table
distinct_ii <- c(1, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1)
tbl_2 <- make_condition_trials(2, params_fixed, distinct_ii)
distinct_ii <- c(1, 2, 3, 4, 2, 3, 1, 4, 2, 3, 4, 1)
tbl_4 <- make_condition_trials(4, params_fixed, distinct_ii)
tbl_setsize <- rbind(tbl_2)


tbl_setsize <- tbl_setsize %>% group_by(condition, setsize, option_selected) %>%
  mutate(trial_id_option_bw = row_number(desc(trial_id))) %>%
  ungroup() %>% arrange(setsize, condition, option_selected)
l_tbl_setsize <- split(tbl_setsize, interaction(tbl_setsize$condition, tbl_setsize$setsize))
# drop empty list elements
l_tbl_setsize <- l_tbl_setsize[map_lgl(l_tbl_setsize, ~ nrow(.x) > 0)]


tbl_setsize$discriminability <- unlist(map(l_tbl_setsize, calc_discriminability, 1, 1))

ggplot(tbl_setsize, aes(trial_id, as.numeric(as.character(option_selected)), group = condition)) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 2) +
  # geom_hline(yintercept = 3) +
  # geom_hline(yintercept = 4) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = condition)) +
  facet_wrap(setsize ~ condition) +
  theme_bw() +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_x_continuous(breaks = seq(1, params_fixed$n_trials, by = 2)) +
  scale_y_continuous(breaks = c(1, 2)) + # , 3, 4
  coord_cartesian(ylim = c(0, 3)) +
  labs(
    x = "Trial ID",
    y = "Forced Choice Option"
  )

# plot discriminability of last three items, which we are going to ask people for
tbl_last_3 <- tbl_setsize %>% filter(trial_id_option_bw <= 3)
ggplot(
  tbl_setsize %>% filter(option_selected == 1) %>% mutate(
    setsize = str_c("Set Size = ", setsize)
  ) , aes(trial_id, discriminability, group = condition)) +
  geom_line(aes(color = condition)) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = condition)) +
  facet_wrap(setsize ~ condition) +
  theme_bw() +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_x_continuous(breaks = seq(1, params_fixed$n_trials, by = 2)) +
  labs(
    x = "Trial ID",
    y = "Discriminability in Memory",
    title = "Response Option = 1"
  )

pd <- position_dodge(width = .95)
grouped_agg(tbl_last_3  %>% mutate(
  option_selected = str_c("Response Option = ", option_selected)
  ), c(condition, option_selected), discriminability) %>%
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











