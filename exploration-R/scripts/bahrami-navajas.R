rm(list = ls())



# Import Packages and Load Data -------------------------------------------


library(tidyverse)
library(rutils)
library(grid)
library(gridExtra)
library(zoo)
library(TTR)
library(future)
library(furrr)
library(ggbeeswarm)
library(reactable)
library(reactablefmtr)
library(docstring)

home_grown <- c("exploration-R/utils/utils.R", "exploration-R/utils/plotting.R")
walk(home_grown, source)


tbl_rb <- read_csv(
  "open-data/bahrami-navajas-2020-4arlb-DataAllSubjectsRewards.csv"
)
tbl_rb <- tbl_rb %>%
  rename(rewards = reward, choices = choice) %>%
  filter(rewards != "NaN") %>%
  group_by(id) %>%
  mutate(trial_id = seq_along(choices)) %>%
  ungroup()

valid_responses <- tbl_rb %>% filter(rewards != "NaN") %>% count(id) %>% arrange(n)
ggplot(valid_responses, aes(n)) + geom_histogram()
n_thx <- 138
# arbitrary cut-off of 138 valid responses

tbl_rb %>% filter(rewards != "NaN") %>% group_by(id) %>% count(choices) %>% arrange(desc(n))
# no reference available about stimulus sets, i.e., no paper
# do not drop participants with 0 switches


tbl_ids <- tbl_rb %>%
  count(payoff_group, id) %>%
  filter(n >= n_thx) %>%
  group_by(payoff_group) %>%
  mutate(id_within_payoff_group = row_number(id))


rewards_trial <- tbl_rb[, str_starts(colnames(tbl_rb), "reward_")]
tbl_rb$max_reward <- pmap_dbl(rewards_trial, ~ max(c(..1, ..2, ..3, ..4)))
idx_second_highest <- pmap_dbl(rewards_trial, ~ which(rank(c(..1, ..2, ..3, ..4), ties.method = "random") == 3))
tbl_rb$second_highest <- pmap_dbl(cbind(rewards_trial, idx_second_highest), ~ c(..1, ..2, ..3, ..4)[..5])


tbl_rb <- tbl_rb %>% 
  inner_join(tbl_ids, by = c("id", "payoff_group")) %>%
  mutate(
    diff_max_second_highest = max_reward - second_highest,
    trial_id_cut = cut(trial_id, breaks = c(-1, seq(25.5, 150.5, by = 25))),
    regret_empirical = max_reward - rewards
  )




pl_walking_arms <- tbl_rb %>%
  rename("1" = reward_c1, "2" = reward_c2, "3" = reward_c3, "4" = reward_c4) %>%
  filter(id_within_payoff_group == 1) %>%
  pivot_longer(cols = c(`1`, `2`, `3`, `4`)) %>%
  ggplot(aes(trial_id, value, group = name)) +
  geom_vline(xintercept = seq(25, 150, by = 25)) +
  geom_line(aes(color = name)) +
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = name)) +
  facet_wrap(~ payoff_group) + 
  theme_bw() +
  scale_color_brewer(name = "Arm", palette = "Set1") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Trial ID", y = "Reward") +
  theme(strip.background = element_rect(fill = "white"))


tbl_rb_bin <- grouped_agg(tbl_rb, c(id, id_within_payoff_group, trial_id_cut, payoff_group), regret_empirical)



# todos
# correlations between regrets across different bins
tbl_corr_diff <- tbl_rb_bin %>%
  select(-c(n, nunique_regret_empirical, se_regret_empirical)) %>%
  pivot_wider(names_from = trial_id_cut, values_from = mean_regret_empirical) %>%
  group_by(payoff_group) %>%
  summarize(
    cor(`(-1,25.5]`, `(25.5,50.5]`),
    cor(`(25.5,50.5]`, `(50.5,75.5]`),
    cor(`(50.5,75.5]`, `(75.5,100]`),
    cor(`(75.5,100]`, `(100,126]`),
    cor(`(100,126]`, `(126,150]`),
    `sd_(-1,25.5]` = sd(`(-1,25.5]`),
    `sd_(25.5,50.5]` = sd(`(25.5,50.5]`),
    `sd_(50.5,75.5]` = sd(`(50.5,75.5]`),
    `sd_(75.5,100]` = sd(`(75.5,100]`),
    `sd_(100,126]` = sd(`(100,126]`),
    `sd_(126,150]` = sd(`(126,150]`)
  )
colnames(tbl_corr_diff)[2:6] <- levels(tbl_rb_bin$trial_id_cut)[2:6]
tbl_corrs <- tbl_corr_diff %>% 
  select(-starts_with("sd_")) %>% 
  pivot_longer(-payoff_group, names_to = "trial_id_cut", values_to = "r")
tbl_sds <- tbl_corr_diff %>% 
  select(c(payoff_group, starts_with("sd_"))) %>% 
  pivot_longer(-payoff_group, names_to = "trial_id_cut", values_to = "sd") %>%
  mutate(trial_id_cut = fct_inorder(str_remove(trial_id_cut, "^sd_")))

tbl_describe <- tbl_corrs %>% left_join(tbl_sds, by = c("payoff_group", "trial_id_cut"))

tbl_describe$trial_id_cut <- fct_inorder(tbl_describe$trial_id_cut)
tbl_rb_bin <- tbl_rb_bin %>% left_join(tbl_describe, by = c("trial_id_cut", "payoff_group"))



pl_diff_violin <- ggplot(tbl_rb_bin, aes(trial_id_cut, mean_regret_empirical, group = r)) +
  geom_hline(yintercept = c(0, 10, 20, 30)) +
  geom_violin(aes(color = r, fill = r)) +
  ggrepel::geom_label_repel(data = tbl_rb_bin %>% filter(id_within_payoff_group == 3), aes(label = str_c("r = ", round(r, 2), "\nsd = ", round(sd, 2)))) +
  facet_wrap(~ payoff_group, nrow = 1) +
  coord_flip(ylim = c(0, 40)) + 
  theme_bw() +
  scale_y_continuous(expand = c(.1, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(y = "Mean Regret", x = "Trial (Binned)") +
  theme(strip.background = element_rect(fill = "white"))
  
grid.draw(arrangeGrob(pl_diff_violin, pl_walking_arms, nrow = 1))

# correlation between difference of highest to second to highest reward and mean empirical regret

tbl_prop_right <- as.data.frame(table(tbl_rb$payoff_group, tbl_rb$diff_max_second_highest, tbl_rb$regret_empirical > 0)) %>%
  pivot_wider(names_from = Var3, values_from = Freq) %>%
  mutate(
    prop_right = 1 - `TRUE` / (`TRUE` + `FALSE`),
    Var1 = as.numeric(as.character(Var1)),
    Var2 = as.numeric(as.character(Var2))
    ) %>%
  rename(payoff_group = Var1) %>%
  filter(Var2 > 0)

pl_difficulty <- ggplot(tbl_prop_right, aes(Var2, prop_right, group = 1)) +
  geom_line() + 
  geom_point(color = "white", size = 6) +
  geom_point(aes(size = `FALSE` + `TRUE`)) +
  facet_wrap(~ payoff_group) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Difference Best - Second Best", y = "Proportion Correct") +
  theme(strip.background = element_rect(fill = "white"))
  
grid.draw(arrangeGrob(pl_walking_arms, pl_diff_violin, pl_difficulty,
            layout_matrix = rbind(c(1, 1),
                                  c(2, 3)),
            nrow = 2, ncol = 2)
)
