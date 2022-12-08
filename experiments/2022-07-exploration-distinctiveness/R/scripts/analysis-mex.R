library(tidyverse)

dirs_load <- c(
  "experiments/2022-07-exploration-distinctiveness/R/utils/normDataWithin.R",
  "experiments/2022-07-exploration-distinctiveness/R/utils/summarSE.R",
  "experiments/2022-07-exploration-distinctiveness/R/utils/summarySEwithin.R",
  "experiments/2022-07-exploration-distinctiveness/R/utils/analysis-utils.R"
)
walk(dirs_load, source)

path_data <- c("experiments/2022-07-exploration-distinctiveness/data/2022-09-01-mex1-pilot/")
returned_timeout <- c()

l_tbls_data <- map(path_data, load_data, participants_returned = returned_timeout)
l_tbl_data <- list(
  "memory" = reduce(map(l_tbls_data, "memory"), rbind),
  "choice" = reduce(map(l_tbls_data, "choice"), rbind)
)

tbl_choice <- l_tbl_data[["choice"]]
tbl_memory <- l_tbl_data[["memory"]]
tbl_memory <- tbl_memory[1:48, ]
tbl_choice <- tbl_choice[1:168, ]
n_mem_items <- 3


# things to analyze:
# memory performance depending on presentation condition$
# tbl_tmp <- tbl_memory %>% group_by(trial_id) %>%
#   filter(test_cue_nr == 0) %>%
#   select(trial_id, test_cue_pos) %>%
#   rename(first_item_cue = test_cue_pos)
# 
# tbl_memory <- tbl_memory %>% left_join(tbl_tmp, by = "trial_id")
tbl_memory$match_first_presented <- tbl_memory$first_item_pres == tbl_memory$test_cue_pos
tbl_memory$prop_correct <- tbl_memory$n_correct / n_mem_items

tbl_memory_agg <- as_tibble(summarySEwithin(
  tbl_memory, 
  measurevar = "prop_correct", 
  withinvars = c("presentation", "match_first_presented"), #"test_cue_nr", 
  idvar = "participant_id"
)) %>% mutate(
  #test_cue_nr = factor(test_cue_nr, labels = c("First", "Second")),
  match_first_presented = factor(
    match_first_presented, labels = c("Presented Second", "Presented First")
  )
)

pd <- position_dodge(width = .8)
pl_mem_acc <- ggplot(
  tbl_memory_agg, 
  aes(presentation, prop_correct, group = match_first_presented)
  ) +
  geom_errorbar(
    aes(ymin = prop_correct - ci, ymax = prop_correct + ci),
    color = "black",  position = pd, width = .2
    ) +
  geom_point(aes(presentation, prop_correct), position = pd) +
  geom_col(aes(fill = match_first_presented, alpha = match_first_presented), position = pd) +
  scale_fill_viridis_d(name = "Location Probed") +
  scale_color_viridis_d(guide = "none") +
  scale_alpha_discrete(range = c(.2, .8), name = "Location Probed") +
  theme_bw() +
  labs(
    x = "Presentation",
    y = "Prop. Correct"
  )
save_my_pdf(
  pl_mem_acc, 
  "experiments/2022-07-exploration-distinctiveness/data/figures/memory-agg.pdf",
  5, 4
  )

# choice prob depending on mean difference
tbl_choice$mean_diff_right_true <- tbl_choice$mean_right - tbl_choice$mean_left
tbl_choice$mean_diff_right_true_cut <- cut(tbl_choice$mean_diff_right_true, 3)
mean_diff_first_presented <- function(first_item_pres, mean_left, mean_right) {
  if(first_item_pres == 0) mean_left - mean_right
  else if (first_item_pres == 1) mean_right - mean_left
}
tbl_choice$mean_diff_first_true <- pmap_dbl(tbl_choice[, c("first_item_pres", "mean_left", "mean_right")], mean_diff_first_presented)
tbl_choice$mean_diff_first_true_cut <- cut(tbl_choice$mean_diff_first_true, 3)
tbl_choice$choice_first_pres <- tbl_choice$choice == tbl_choice$first_item_pres

# very basic pattern: more right when right is higher overall
pl_choice_basic <- tbl_choice %>% group_by(participant_id, mean_diff_right_true, trial_id) %>%
  summarize(
    choice_mn = mean(choice),
    n = n()
  ) %>%
  group_by(mean_diff_right_true) %>%
  summarize(
    choice_mn = mean(choice_mn),
    n = sum(n)
  ) %>% 
  ggplot(aes(mean_diff_right_true, choice_mn, group = 1)) +
  geom_vline(xintercept = 0, alpha = .3) +
  geom_hline(yintercept = .5, alpha = .3) +
  geom_line() +
  geom_point(color = "white", size = 7) +
  geom_point(shape = 1, aes(size = n)) +
  scale_size_continuous(name = "Nr. Responses") +
  theme_bw() +
  labs(
    x = "Right - Left (True Means)",
    y = "Proportion Right Option",
    caption = "Nb. all choices"
  ) +
  coord_cartesian(ylim = c(0, 1))
save_my_pdf(
  pl_choice_basic, 
  "experiments/2022-07-exploration-distinctiveness/data/figures/choice-basic.pdf",
  5, 4
  )

tbl_choice_agg <- as_tibble(summarySEwithin(
  tbl_choice, 
  measurevar = "choice", 
  withinvars = c("presentation", "horizon", "mean_diff_right_true_cut", "choice_first_pres"), 
  idvar = "participant_id"
))

# more exploration with longer horizon?
# more exploration in interleaved compared to massed condition (i.e., when memory is worse)
# more exploration in massed condition towards option presented longer ago (i.e., towards the first presented location)
pd <- position_dodge(width = .4)
ggplot(tbl_choice_agg, aes(mean_diff_right_true_cut, choice, group = presentation)) +
  geom_line(aes(color = presentation), position = pd) +
  geom_point(size = 7, color = "white", position = pd) +
  geom_point(shape = 1, aes(color = presentation, size = N), position = pd) +
  geom_vline(xintercept = "(-6.67,6.67]", alpha = .3) +
  geom_hline(yintercept = .5, alpha = .3) +
  facet_grid(choice_first_pres ~ horizon) +
  scale_size_continuous(name = "Nr. Responses") +
  scale_color_viridis_d(name = "Presentation") +
  theme_bw() +
  labs(
    x = "Right - Left (True Means)",
    y = "Proportion Right Option"
  )

# same as above, but aggregating over horizon and choice first presented
tbl_choice_agg_crude <- as_tibble(summarySEwithin(
  tbl_choice, 
  measurevar = "choice", 
  withinvars = c("presentation", "mean_diff_right_true_cut"), 
  idvar = "participant_id"
))

pl_choice_condition <- ggplot(tbl_choice_agg_crude, aes(mean_diff_right_true_cut, choice, group = presentation)) +
  geom_line(aes(color = presentation), position = pd) +
  geom_point(size = 7, color = "white", position = pd) +
  geom_point(shape = 1, aes(color = presentation, size = N), position = pd) +
  geom_vline(xintercept = "(-6.67,6.67]", alpha = .3) +
  geom_hline(yintercept = .5, alpha = .3) +
  scale_size_continuous(name = "Nr. Responses") +
  scale_color_viridis_d(name = "Presentation") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  labs(
    x = "Right - Left (True Means)",
    y = "Proportion Right Option"
  )
save_my_pdf(
  pl_choice_condition,
  "experiments/2022-07-exploration-distinctiveness/data/figures/choices-condition.pdf",
  5, 4
)

# choice prob depending on memory performance
tbl_memory_wide_agg <- tbl_memory %>% 
  select(participant_id, session, trial_id, presentation, match_first_presented, n_correct) %>%
  pivot_wider(
    id_cols = c(participant_id, session, trial_id, presentation), 
    names_from = match_first_presented, values_from = n_correct
  ) %>%
  rename(n_correct_first = `TRUE`, n_correct_second = `FALSE`) %>%
  group_by(participant_id, session, presentation) %>%
  summarize(
    n_correct_first_mn = mean(n_correct_first),
    n_correct_second_mn = mean(n_correct_second)
  ) %>% ungroup() %>%
  mutate(
    n_correct_diff = n_correct_first_mn - n_correct_second_mn
  )

# trials with no memory test could be filled by average from same condition with memory test
tbl_choice_mem <- left_join(
  tbl_choice, tbl_memory_wide_agg, by = c("participant_id", "session", "presentation")
)

tbl_choice_mem_agg <- tbl_choice_mem %>%
  group_by(
    presentation, mean_diff_first_true_cut, n_correct_diff >= 0 # horizon, 
  ) %>% summarize(
    choice_mn = mean(choice_first_pres),
    n = n()
  ) %>% ungroup()

# here, idea is that if people know less about first-presented option
# they again choose it more often (-> intercept shift aka directed exploration)

pl_choice_mem <- ggplot(tbl_choice_mem_agg, aes(mean_diff_first_true_cut, choice_mn, group = `n_correct_diff >= 0`)) +
  geom_line(aes(color = `n_correct_diff >= 0`), position = pd) +
  geom_point(color = "white", size = 3, position = pd) +
  geom_point(aes(color = `n_correct_diff >= 0`, size = n), position = pd) +
  facet_grid( ~ presentation) + #horizon
  scale_color_viridis_d(name = "Better Memory\nfor Left Bandit") +
  scale_size_continuous(name = "Nr. Trials") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  labs(
    x = "True Mean Difference",
    y = "Choice Proportion Left Bandit"
  )
save_my_pdf(
  pl_choice_mem,
  "experiments/2022-07-exploration-distinctiveness/data/figures/choices-left-memory.pdf",
  6.5, 4.5
)

