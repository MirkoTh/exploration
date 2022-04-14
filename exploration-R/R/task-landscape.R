library(tidyverse)

# create a list with tasks used previously

tbl_tasks <- tribble(
  ~task_name,                          ~confound_rew_inf, ~nr_arms, ~add_gen, ~experience,
  "observe-or-bet",                    FALSE,               "few",    FALSE,        TRUE,
  "two-armed-bandit",                  TRUE,                "few",    FALSE,        TRUE,
  "restless-bandit",                   TRUE,                "few",    FALSE,        TRUE,
  "grid-task",                         TRUE,                "many",   TRUE,        TRUE,
  "decision-from-experience",          TRUE,                "few",    FALSE,        TRUE,
  "decision-from-experience-sampling", FALSE,               "few",    TRUE,       TRUE,
  "decision-from-description",         FALSE,               "few",    FALSE,  FALSE
)


tbl_tasks <- format_task_tbl(tbl_tasks)
