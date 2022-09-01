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


# things to analyze:
# memory performance depending on presentation condition$
tbl_tmp <- tbl_memory %>% group_by(trial_id) %>%
  filter(test_cue_nr == 0) %>%
  select(trial_id, test_cue_pos) %>%
  rename(first_item_cue = test_cue_pos)

tbl_memory <- tbl_memory %>% left_join(tbl_tmp, by = "trial_id")
tbl_memory$match_first_presented <- tbl_memory$first_item_pres == tbl_memory$test_cue_pos

tbl_memory_agg <- as_tibble(summarySEwithin(
  tbl_memory, 
  measurevar = "n_correct", 
  withinvars = c("presentation", "test_cue_pos", "match_first_presented"), 
  idvar = "participant_id"
)) %>% mutate(
  test_cue_pos = factor(test_cue_pos, labels = c("Left", "Right")),
  match_first_presented = factor(
    match_first_presented, labels = c("Matches Second", "Matches First")
    )
)

pd <- position_dodge(width = .8)
ggplot(tbl_memory_agg, aes(presentation, n_correct, group = test_cue_pos)) +
  geom_col(aes(fill = test_cue_pos, alpha = test_cue_pos), position = pd) +
  facet_wrap(~ match_first_presented) +
  scale_fill_brewer(palette = "Set1", name = "Cue Position") +
  scale_alpha_discrete(range = c(.2, .8), name = "Cue Position") +
  theme_bw() +
  labs(
    x = "Presentation",
    y = "Nr. Correct"
  )
# choice prob depending on mean difference

tbl_choice$mean_diff_true <- tbl_choice$mean_right - tbl_choice$mean_left
tbl_choice$choice_first_pres <- as.numeric(tbl_choice$choice == tbl_choice$first_item_pres)
tbl_choice_agg <- as_tibble(summarySEwithin(
  tbl_choice, 
  measurevar = "choice", 
  withinvars = c("presentation", "horizon", "is_memtest", "mean_diff_true", "first_item_cue"), 
  idvar = "participant_id"
))

ggplot(tbl_choice_agg, aes(mean_diff_true, choice, group = presentation)) +
  geom_line(aes(color = presentation)) +
  geom_point(size = 3, color = "white") +
  geom_point(shape = 1, aes(color = presentation)) +
  facet_grid(~ horizon) +
  theme_bw() +
  labs(
    x = "Right - Left (True Means)",
    y = "Proportion Right Option"
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
    participant_id, presentation, horizon, mean_diff_true, n_correct_diff
    ) %>% summarize(
    choice_mn = mean(choice),
    choice_first_pres_mn = mean(choice_first_pres)
    ) %>% ungroup()

# here, idea is that if people know less about first-presented option
# they again choose it more often (-> intercept shift aka directed exploration)

ggplot(tbl_choice_mem_agg, aes(mean_diff_true, choice_first_pres_mn, group = n_correct_diff)) +
  geom_line(aes(color = n_correct_diff)) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = n_correct_diff), ) +
  scale_color_viridis_c() +
  theme_bw() +
  labs(
    x = "True Mean Difference",
    y = "Choice Proportion"
  )
