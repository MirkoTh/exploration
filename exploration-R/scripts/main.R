library(tidyverse)
library(rutils)
library(grid)
library(gridExtra)
library(mvtnorm)
library(zoo)
library(TTR)

home_grown <- c("exploration-R/utils/utils.R", "exploration-R/utils/plotting.R")
walk(home_grown, source)


# Fixed-Means Bandit ------------------------------------------------------
# Data from Gershman (2018)

#tbl_exp2 <- read_csv("data2.csv")
tbl_exp2 <- read_csv("https://raw.githubusercontent.com/sjgershm/exploration/master/data2.csv")
tbl_exp2$higher_mean <- 1
tbl_exp2$higher_mean[tbl_exp2$mu2 > tbl_exp2$mu1] <- 2
tbl_exp2$chose_higher <- tbl_exp2$higher_mean == tbl_exp2$choice
tbl_exp2$mean_diff_abs <- abs(tbl_exp2$mu1 - tbl_exp2$mu2)
tbl_exp2$mean_diff_abs_cut <- factor(cut(
  tbl_exp2$mean_diff_abs, 
  c(-Inf, 5, 10, 15, Inf),
  c("< 5", "5 - 10", "10 - 15", "> 15")
))
# tbl_exp2 <- tbl_exp2 %>% filter(
#   between(RT, 300, 3500)
# )

# aggregate by subject and drop cells with < 3 obs
by_subj <- grouped_agg(tbl_exp2, c(subject, trial, mean_diff_abs_cut), c(RT, block)) %>%
  ungroup() %>% filter(n >= 3)

tbl_exp2_agg <- grouped_agg(by_subj, c(trial, mean_diff_abs_cut), mean_RT)

x_vals <- sort(unique(tbl_exp2_agg$trial))
x_show <- seq(min(x_vals), max(x_vals), by = 2)
pd <- position_dodge(width = .4)
pl_gersh_e2 <- ggplot(tbl_exp2_agg, aes(trial, mean_mean_RT, group = mean_diff_abs_cut)) +
  geom_errorbar(
    aes(
      ymin = mean_mean_RT - 1.96 * se_mean_RT, 
      ymax = mean_mean_RT + 1.96 * se_mean_RT,
      color = mean_diff_abs_cut
    ), width = .3, position = pd
  ) +
  geom_point(color = "white", size = 3, position = pd) +
  geom_point(aes(color = mean_diff_abs_cut), position = pd) +
  geom_line(aes(color = mean_diff_abs_cut), position = pd) +
  #facet_wrap(~ mean_diff_abs_cut) +
  scale_color_brewer(palette = "Set1", name = "Difference of Means") +
  scale_x_continuous(breaks = x_show, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_bw() +
  theme(legend.position = c(.7, .8)) +
  labs(
    x = "Trial",
    y = "RT (ms)",
    title = "Gershman (2018): Experiment 2"
  )


# Restless Bandit ---------------------------------------------------------
# Data from Speekenbrink & Konstantinidis (2015)


tbl_rb <- read_csv(
  "https://github.com/speekenbrink-lab/data/raw/master/Speekenbrink_Konstantinidis_2015.csv"
)
tbl_rb <- tbl_rb %>% mutate(
  trend = factor(startsWith(cond, "t"), labels = c("Trend", "No Trend")),
  volatility = factor(endsWith(cond, "n"), labels = c("Variance Changes", "Variance Stable"))
)

rb_by_trial <- grouped_agg(tbl_rb %>% filter(between(rt, 300, 3500)), c(trial, trend, volatility), c(rt)) %>%
  ungroup()
rb_by_trial$cond_long <- interaction(rb_by_trial$trend, rb_by_trial$volatility, sep = " & ")
x_vals <- sort(unique(rb_by_trial$trial))
x_show <- seq(min(x_vals), max(x_vals), by = length(x_vals)/8)


make_poly <- function(x, y) {
  cond_long <- unique(rb_by_trial$cond_long)
  tbl_polygon <- crossing(x, y, cond_long)
  tbl_polygon$val <- 0
  tbl_polygon$val[endsWith(as.character(tbl_polygon$cond_long), "Changes")] <- 1
  tbl_polygon1 <- tbl_polygon[1:(nrow(tbl_polygon)/2), ]
  tbl_polygon2 <- tbl_polygon[((nrow(tbl_polygon)/2)+1):nrow(tbl_polygon), ]
  tbl_polygon2 <- tbl_polygon2 %>% arrange(desc(y))
  tbl_polygon <- rbind(tbl_polygon1, tbl_polygon2)
  return(tbl_polygon)
}

x1 <- c(51, 51, 100, 100)
x2 <- c(151, 151, 200, 200)
y <- c(1500, 2500, 2500, 1500)
tbl_polygon_h1 <- make_poly(x1, y)
tbl_polygon_h2 <- make_poly(x2, y)

rb_by_trial <- rb_by_trial %>% filter(cond_long == "Trend & Variance Changes")
tbl_polygon_h1 <- tbl_polygon_h1 %>% filter(cond_long == "Trend & Variance Changes")
tbl_polygon_h2 <- tbl_polygon_h2 %>% filter(cond_long == "Trend & Variance Changes")

pl_speek_konst <- ggplot(rb_by_trial, aes(
  trial, mean_rt, group = cond_long)
) + geom_errorbar(
  aes(
    ymin = mean_rt - 1.96 * se_rt, 
    ymax = mean_rt + 1.96 * se_rt,
    color = cond_long
  ), width = .3, position = pd
) + geom_line(aes(color = cond_long), position = pd) +
  geom_point(color = "white", size = 3, position = pd) +
  geom_point(aes(color = cond_long), position = pd, shape = 1) +
  geom_polygon(data = tbl_polygon_h1, aes(x, y, alpha = val, group = cond_long)) +
  geom_polygon(data = tbl_polygon_h2, aes(x, y, alpha = val, group = cond_long)) +
  facet_wrap(~ cond_long) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  scale_alpha_continuous(range = c(0, .1), guide = "none") +
  scale_x_continuous(breaks = x_show, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  labs(
    x = "Trial",
    y = "RT (ms)",
    title = "Speekenbrink & Konstantinidis (2015)"
  )

grid.draw(arrangeGrob(pl_gersh_e2, pl_speek_konst, ncol = 2))


tbl_exp2 <- tbl_exp2 %>% arrange(subject, block, trial) %>% 
  group_by(subject, block) %>% mutate(
    previous_choice = lag(choice),
    repeat_choice = choice == previous_choice,
    switch_choice = choice != previous_choice
  )
tbl_exp2$repeat_choice[is.na(tbl_exp2$repeat_choice)] <- FALSE
tbl_exp2$switch_choice[is.na(tbl_exp2$switch_choice)] <- TRUE
tbl_exp2 <- tbl_exp2 %>%
  mutate(
    nr_previous_switches = cumsum(as.numeric(switch_choice)),
    nr_previous_switches = lag(nr_previous_switches)
  ) %>%
  ungroup() %>%
  replace_na(list(nr_previous_switches = 0)) %>% 
  group_by(subject) %>%
  mutate(
    avg_switches = mean(nr_previous_switches)
  ) %>% ungroup() %>%
  arrange(avg_switches) %>%
  group_by(subject, block) %>%
  mutate(
    subject = as.character(subject),
    run_nr = cumsum(switch_choice)
  ) %>% 
  group_by(subject, block, run_nr) %>%
  mutate(run_length = cumsum(repeat_choice)) %>%
  ungroup() %>% 
  mutate(run_length = lag(run_length)) %>%
  replace_na(list(run_length = 0))
tbl_exp2$subject <- fct_inorder(tbl_exp2$subject)


tbl_rep_choice_exp2 <- grouped_agg(tbl_exp2, c(subject, mean_diff_abs_cut, trial), repeat_choice) %>%
  drop_na() %>%
  grouped_agg(c(mean_diff_abs_cut, trial), mean_repeat_choice)

ggplot(tbl_rep_choice_exp2, aes(trial, mean_mean_repeat_choice, group = mean_diff_abs_cut)) +
  geom_line(aes(color = mean_diff_abs_cut)) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = mean_diff_abs_cut)) +
  geom_hline(yintercept=c(0, 1), linetype='dotdash', color=c("black", "black"), size = .6) +
  scale_color_brewer(name = "Difference of Means", palette = "Set1") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw() +
  labs(
    x = "Trial",
    y = "Proportion Repeat Choice",
    title = "Gershman (2018): Experiment 2"
  )

ggplot(
  tbl_exp2
  , aes(nr_previous_switches, group = subject)) +
  geom_histogram(color = "white", aes(fill = avg_switches)) +
  facet_wrap(~ subject) +
  scale_x_continuous(breaks = seq(2, 10, by = 2), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(guide = "none") +
  theme_bw() +
  labs(x = "Nr. Previous Switches", y = "Nr. Choices")


# nb. all four conditions are between participants
ggplot(tbl_rb, aes(trial, id), group = as.factor(deck)) +
  geom_tile(aes(fill = as.factor(deck)), color = "white") +
  scale_fill_viridis_d(name = "Deck") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(trend ~ volatility)



# Add Variables to Restless Bandit Data -----------------------------------


tbl_rb <- tbl_rb %>% 
  arrange(id2, cond, trial) %>%
  group_by(id2) %>%
  mutate(
    previous_deck = lag(deck),
    repeat_deck = deck == previous_deck,
    switch_deck = deck != previous_deck
  )
tbl_rb$repeat_deck[is.na(tbl_rb$repeat_deck)] <- FALSE
tbl_rb$switch_deck[is.na(tbl_rb$switch_deck)] <- TRUE

tbl_rb <- tbl_rb %>% group_by(id2) %>%
  mutate(run_nr = cumsum(switch_deck)) %>%
  group_by(id2, cond, run_nr) %>%
  mutate(
    run_length = cumsum(repeat_deck)
  ) %>% group_by(id2, cond) %>%
  mutate(
    nr_previous_switches_lagged = runSum(x = switch_deck, n = 10, cumulative = FALSE),
    nr_previous_switches_lagged = lag(nr_previous_switches_lagged),
    nr_previous_switches_cumsum = lag(cumsum(switch_deck)),
    nr_previous_switches_lagged = coalesce(nr_previous_switches_lagged, nr_previous_switches_cumsum),
    run_length_lagged = lag(run_length)
  ) %>% dplyr::select(-nr_previous_switches_cumsum) %>%
  replace_na(list(nr_previous_switches_lagged = 0, run_length_lagged = 0)) %>%
  mutate(avg_switches = mean(nr_previous_switches_lagged)) %>%
  ungroup() %>% arrange(avg_switches)
tbl_rb$id2 <- fct_inorder(as.character(tbl_rb$id2))

tbl_rb_run <- tbl_rb %>% group_by(id2, trend, volatility, run_nr) %>%
  summarize(run_length = max(run_length)) %>%
  ungroup()

write_csv(
  tbl_exp2 %>% mutate(repeat_choice = as.numeric(repeat_choice), switch_choice = as.numeric(switch_choice)),
  "open-data/gershman-2018-e2.csv"
)
write_csv(
  tbl_rb %>% mutate(repeat_deck = as.numeric(repeat_deck), switch_deck = as.numeric(switch_deck)),
  "open-data/speekenbrink-konstantinidis-2015.csv"
)

ggplot(
  tbl_rb
  , aes(nr_previous_switches_lagged, group = id2)) +
  geom_histogram(color = "white", aes(fill = avg_switches)) +
  facet_wrap(~ id2) +
  scale_x_continuous(breaks = seq(2, 10, by = 2), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(guide = "none") +
  theme_bw() +
  labs(x = "Nr. Previous Switches (Lagged)", y = "Nr. Choices")



# Learning ----------------------------------------------------------------


# run kalman learning model on both data sets

sigma_xi_sq_exp2 <- 0
sigma_epsilon_sq_exp2 <- 10
no_exp2 <- 2

sigma_epsilon_sq_rb <- 16
sigma_xi_sq_rb <- 16
no_rb <- 4


l_exp2 <- split(tbl_exp2[, c("choice", "reward")], interaction(tbl_exp2$subject, tbl_exp2$block))
l_rb <- split(tbl_rb[, c("deck", "payoff")], tbl_rb$id2)
l_exp2 <- map(l_exp2, function(x) {
  colnames(x) <- c("choices", "rewards")
  return(x)
})
l_rb <- map(l_rb, function(x) {
  colnames(x) <- c("choices", "rewards")
  return(x)
})

l_results_rb <- map(l_rb, kalman_learning, no = no_rb, sigma_xi_sq = sigma_xi_sq_rb, sigma_epsilon_sq = sigma_epsilon_sq_rb)
l_results_exp2 <- map(l_exp2, kalman_learning, no = no_exp2, sigma_xi_sq = sigma_xi_sq_exp2, sigma_epsilon_sq = sigma_epsilon_sq_exp2, m0 = 0, v0 = 100)



# a few checks of the learned values by the model
# l_results_rb[[1]]
# names(l_rb)[[1]]
# tbl_rb %>% filter(id2 == 1)
# 
# l_results_exp2[[734]]
# names(l_exp2)[[734]]
# tbl_exp2 %>% filter(subject == 35 & block == 17)
#
# checks look good


# Choices -----------------------------------------"------------------------


# restless bandit sp. & ko. 2015
l_choice_probs_rb <- map(l_results_rb, safely(choice_probs_rb))
l_choice_probs_results_rb <- map(l_choice_probs_rb, "result")


subjects_rb <- map_chr(names(l_choice_probs_results_rb), ~ str_extract(.x, "^[0-9]*"))
tbl_rb_features_learned <- pmap(
  list(l_choice_probs_results_rb, subjects_rb), 
  function(x, y){
    x$id2 <- y
    return(x)
  }
) %>% reduce(rbind) %>% 
  rename(p_1 = V1, p_2 = V2, p_3 = V3, p_4 = V4) %>%
  cbind(reduce(l_results_rb, rbind)) %>%
  as_tibble()

tbl_rb_features_learned <- tbl_rb_features_learned %>% 
  mutate(
    trial = rep(
      1:(nrow(tbl_rb_features_learned)/length(unique(tbl_rb_features_learned$id2))), 
      length(unique(tbl_rb_features_learned$id2))
    )) %>% left_join(
      tbl_rb %>% ungroup() %>%
        dplyr::select(
          id2, trial, trend, volatility, deck, previous_deck,
          repeat_deck, run_nr, run_length, nr_previous_switches_lagged, run_length_lagged
        ) %>% mutate(id2 = as.character(id2)),
      by = c("id2", "trial")
    ) %>% relocate(where(is.character) | where(is.factor), .before = p_1) %>% relocate(trial, .after = id2)

ggplot(
  tbl_rb_features_learned %>% 
    dplyr::select(id2, trial, p_1, p_2, p_3, p_4) %>%
    pivot_longer(cols = c(p_1, p_2, p_3, p_4)) %>%
    mutate(name = factor(name, labels = 1:4))
  , aes(trial, name)) +
  geom_tile(aes(fill = value)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_viridis_c(name = "p(choice)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_wrap(~ id2) +
  labs(
    x = "Trial ID",
    y = "Bandit Nr."
  )


# experiment 2 gershman 2018
l_choice_probs_e2 <- map(l_results_exp2, safely(choice_probs_e2))
l_choice_probs_results_e2 <- map(l_choice_probs_e2, "result")


# unpack results again into tbl
subjects_e2 <- map_chr(names(l_choice_probs_results_e2), ~ str_extract(.x, "^[0-9]*"))
blocks_e2 <- map_chr(names(l_choice_probs_results_e2), ~ str_extract(.x, "[0-9]+$"))
tbl_exp2_features_learned <- pmap(
  list(l_choice_probs_results_e2, subjects_e2, blocks_e2), 
  function(x, y, z){
    x$subject <- y
    x$block <- z
    return(x)
  }
) %>% reduce(rbind) %>%
  cbind(reduce(l_results_exp2, rbind)) %>%
  rename(p_1 = V1, p_2 = V2) %>% 
  as_tibble()
tbl_exp2_features_learned <- tbl_exp2_features_learned %>%
  mutate(
    val_diff = m_1 - m_2,
    ru = sqrt(v_1) - sqrt(v_2),
    tu = sqrt(v_1 + v_2),
    thompson = val_diff / tu,
    p_diff = p_1 - p_2,
    trial = rep(1:(max(tbl_exp2$trial) + 1), (max(as.numeric(subject)) * max(as.numeric(block))))
  ) %>% relocate(where(is.character), .before = p_1) %>%
  relocate(trial, .after = "block") %>% left_join(
    tbl_exp2 %>% 
      dplyr::select(
        subject, block, trial, choice, previous_choice,
        repeat_choice, nr_previous_switches, run_nr, run_length
      ) %>% mutate(block = as.character(block)),
    by = c("subject", "block", "trial")
  )

cor(tbl_exp2_features_learned[, c("val_diff", "ru", "thompson", "p_diff")])


tbl_exp2_features_learned <- tbl_exp2_features_learned[complete.cases(tbl_exp2_features_learned), ]
tbl_rb_features_learned <- tbl_rb_features_learned[complete.cases(tbl_rb_features_learned), ]

# on average, there should be no relation between bandit and choice prob
ggplot(
  tbl_exp2_features_learned %>% 
    dplyr::select(subject, trial, p_1, p_2) %>%
    pivot_longer(cols = c(p_1, p_2)) %>%
    mutate(name = factor(name, labels = 1:2)) %>%
    group_by(subject, trial, name) %>% summarize(value = mean(value))
  , aes(trial, name)) +
  geom_tile(aes(fill = value)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  scale_fill_viridis_c(name = "p(choice)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_wrap(~ subject) +
  labs(
    x = "Trial ID",
    y = "Bandit Nr."
  )


# Fit Choices -------------------------------------------------------------


tbl_cor <- cor(
  tbl_rb_features_learned[
    complete.cases(tbl_rb_features_learned),
    c(
      "p_1", "p_2", "p_3", "p_4", "m_1", "m_2", "m_3", "m_4", 
      "v_1", "v_2", "v_3", "v_4", "run_length_lagged", 
      "nr_previous_switches_lagged"
    )
  ]
) %>% as.data.frame() %>% as_tibble()
tbl_cor$x <- colnames(tbl_cor)
tbl_cor$x <- fct_inorder(tbl_cor$x)
levels(tbl_cor$x) <- c(
    "PMU1", "PMU2", "PMU3", "PMU4", "M1", "M2", "M3", "M4", 
    "V1", "V2", "V3", "V4", "Run Length (Lagged)", "Nr. Previous Switches (Lagged)"
)
colnames(tbl_cor) <- c(levels(tbl_cor$x), "x")
tbl_cor <- tbl_cor %>% pivot_longer(cols = -x) 
tbl_cor$name <- fct_inorder(tbl_cor$name)
tbl_cor %>%
  ggplot(aes(x, name)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis_c() +
  geom_text(aes(label = str_remove(round(value, 2), "^0+")), color = "white", size = 5) +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
    ) +
  scale_y_discrete(limits = rev) +
  scale_fill_viridis_c(name = "Correlation")




# Fit Switches ------------------------------------------------------------

tbl_exp2_features_learned$m_prev <- pmap_dbl(
  tbl_exp2_features_learned[, c("m_1", "m_2", "previous_choice")], 
  ~ c(..1, ..2)[..3] - c(..1, ..2)[abs(..3 - 3)]
  )
tbl_exp2_features_learned$v_prev <- pmap_dbl(
  tbl_exp2_features_learned[, c("v_1", "v_2", "previous_choice")], 
  ~ c(..1, ..2)[..3] - c(..1, ..2)[abs(..3 - 3)]
)
tbl_exp2_features_learned$p_prev <- pmap_dbl(
  tbl_exp2_features_learned[, c("p_1", "p_2", "previous_choice")], 
  ~ c(..1, ..2)[..3]
)



tbl_rb_features_learned$p_prev <- pmap_dbl(
  tbl_rb_features_learned[, c("p_1", "p_2", "p_3", "p_4", "previous_deck")], 
  ~ c(..1, ..2, ..3, ..4)[..5]
)

tbl_rb_features_learned$m_diff <- pmap_dbl(
  tbl_rb_features_learned[, c("m_1", "m_2", "m_3", "m_4", "previous_deck")], 
  ~ c(..1, ..2, ..3, ..4)[..5] - max(c(..1, ..2, ..3, ..4)[-..5])
)

tbl_rb_features_learned$v_diff <- pmap_dbl(
  tbl_rb_features_learned[, c("v_1", "v_2", "v_3", "v_4", "previous_deck")], 
  ~ max(c(..1, ..2, ..3, ..4)[-..5] - c(..1, ..2, ..3, ..4)[..5])
)


write_csv(tbl_exp2_features_learned, "open-data/gershman-2018-e2-addon-features.csv")
write_csv(tbl_rb_features_learned, "open-data/speekenbrink-konstantinidis-2015-addon-features.csv")



# Entropy -----------------------------------------------------------------


library(entropy)

freqs_runs_trend <- tbl_rb_run %>% group_by(id, volatility) %>% count(run_length) 
tbl_design <- crossing(id = unique(tbl_rb_run$id), volatility = unique(tbl_rb_run$volatility), run_length = 1:max(freqs_runs_trend$run_length))
tbl_freqs <- tbl_design %>% 
  left_join(freqs_runs_trend, by = c("id", "volatility", "run_length")) %>%
  replace_na(list(n = 0))

l_tbl_freqs <- tbl_freqs %>% split(interaction(.$id, .$volatility))
l_freqs_empirical <- map(l_tbl_freqs, ~ freqs.empirical(.x$n))
l_entropy_empirical <- map(l_freqs_empirical, entropy.empirical)
tbl_entropy <- reduce(l_entropy_empirical, rbind) %>% as.data.frame()
tbl_entropy$id <- str_extract(names(l_entropy_empirical), "^[0-9]*")
tbl_entropy$cond <- str_extract(names(l_entropy_empirical), "[a-z A-Z]*$")

tbl_entropy_wide <- tbl_entropy %>%
  pivot_wider(id_cols = id, names_from = cond, values_from = V1)
tbl_entropy_cor <- tibble(r = cor(tbl_entropy_wide$`Variance Changes`, tbl_entropy_wide$`Variance Stable`))

ggplot(tbl_entropy_wide, aes(`Variance Changes`, `Variance Stable`)) +
  geom_point() +
  geom_label(data = tbl_entropy_cor, aes(x = .75, y = .75, label = str_c("r = ", round(r, 2)))) +
  geom_smooth(method = "lm")



# Run Lengths -------------------------------------------------------------


ggplot(tbl_rb_run %>% filter(run_length > 1), aes(run_length)) +
  geom_histogram(aes(fill = id), binwidth = 2) +
  facet_wrap(~ id) +
  coord_cartesian(xlim = c(0, 20)) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Run Length", y = "Nr. Runs", title = "Speekenbrink & Konstantinidis (2015)")


l_by_trend <- tbl_rb_run %>%
  filter(run_length > 1) %>%
  split(interaction(.$id, .$volatility))
l_summary <- map(
  l_by_trend, 
  ~ c(mn_run_length = mean(.x$run_length), sd_run_length = sd(.x$run_length))
)
tbl_run_stats <- reduce(l_summary, rbind) %>% as.data.frame()
tbl_run_stats$id <- str_extract(names(l_summary), "^[0-9]*")
tbl_run_stats$cond <- str_extract(names(l_summary), "[a-z A-Z]*$")

tbl_run_wide <- tbl_run_stats %>%
  pivot_wider(id_cols = id, names_from = cond, values_from = mn_run_length) %>%
  mutate(var = "Means") %>%
  rbind(
    tbl_run_stats %>%
      pivot_wider(id_cols = id, names_from = cond, values_from = sd_run_length) %>%
      mutate(var = "SDs")
  ) %>% mutate(var = factor(var))


tbl_label_e2 <- tbl_run_wide %>% group_by(var) %>% summarize(r = cor(Trend, `No Trend`))
ggplot(tbl_run_wide, aes(Trend, `No Trend`, group = var)) +
  geom_point(aes(color = var)) +
  geom_label(data = tbl_label_e2, aes(x = 20, y = 37.5, label = str_c("r = ", round(r, 2)))) +
  geom_smooth(method = "lm") +
  facet_wrap(~ var)

tbl_label_e2 <- tbl_run_wide %>% group_by(var) %>% summarize(r = cor(`Variance Changes`, `Variance Stable`))
ggplot(tbl_run_wide, aes(`Variance Changes`, `Variance Stable`, group = var)) +
  geom_point(aes(color = var)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ var)




# Nr. Switches ------------------------------------------------------------


tbl_rb %>% group_by(id, volatility) %>%
  summarise(n_switches = sum(switch_deck)) %>%
  pivot_wider(names_from = volatility, values_from = n_switches) %>%
  ggplot(aes(`Variance Changes`, `Variance Stable`)) +
  geom_point() +
  geom_smooth(method = "lm")


tbl_trend_switches <- tbl_rb %>% group_by(id, trend) %>%
  summarise(n_switches = sum(switch_deck)) %>%
  pivot_wider(names_from = trend, values_from = n_switches)
tbl_label_trend <- tibble(r = cor(tbl_trend_switches$Trend, tbl_trend_switches$`No Trend`))

pl_switches_rl <- tbl_rb %>% group_by(id, trend) %>%
  summarise(n_switches = sum(switch_deck)) %>%
  pivot_wider(names_from = trend, values_from = n_switches) %>%
  ggplot(aes(`Trend`, `No Trend`)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, color = "#440154") +
  geom_label(data = tbl_label_trend, aes(x = 100, y = 250, label = str_c("r = ", round(r, 2)))) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Trend", y = "No Trend", title = "Nr. Switches: Speekenbrink & Konstantinidis (2015)")


tbl_exp2$half <- tbl_exp2$block %% 2
tbl_exp2_switches_wide <- tbl_exp2 %>% group_by(subject, half) %>%
  summarize(n_switches = sum(switch_choice)) %>% 
  ungroup() %>%
  pivot_wider(id_cols = subject, names_from = half, values_from = n_switches)

tbl_label <- tibble(r = cor(tbl_exp2_switches_wide$`0`, tbl_exp2_switches_wide$`1`))

pl_switches_fixed <- ggplot(tbl_exp2_switches_wide, aes(`0`, `1`)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, color = "#440154") +
  geom_label(data = tbl_label, aes(x = 20, y = 45, label = str_c("r = ", round(r, 2)))) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Even Blocks", y = "Uneven Blocks", title = "Nr. Switches: Gershman (2018)")

grid.draw(arrangeGrob(pl_switches_fixed, pl_switches_rl, nrow = 1))






