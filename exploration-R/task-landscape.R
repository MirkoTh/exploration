library(tidyverse)
library(factoextra)
library(ggrepel)
library(formattable)

home_grown_stuff <- c("R/utils.R")
walk(home_grown_stuff, source)

# create a list with tasks used previously

tbl_tasks <- tribble(
  ~task_name,           ~nr_arms, ~moving_stats, ~add_gen, ~experience, ~confound_rew_inf, ~capacity,
  "observe-or-bet",     "few",    FALSE,        FALSE,    TRUE,         FALSE,             1,
  "two-armed-bandit",   "few",    FALSE,        FALSE,    TRUE,         TRUE,              1,
  "restless-bandit",    "few",    TRUE,         FALSE,    TRUE,         TRUE,              2,
  "grid-task",          "many",   FALSE,        TRUE,     TRUE,         TRUE,              3,
  "dfe",                "few",    FALSE,        FALSE,    TRUE,         TRUE,              1,
  "dfe-sampling",       "few",    FALSE,        FALSE,    TRUE,         FALSE,             1,
  "dfd",                "few",    FALSE,        FALSE,    FALSE,        FALSE,             0,
  "horizon-task",       "few",    FALSE,        FALSE,    FALSE,        FALSE,             0,
)

tbl_tasks <- format_task_tbl(tbl_tasks)

# a task table
plot_task_table(tbl_tasks)
x11()
plot_labels_task(tbl_tasks)
x11()
plot_pca(tbl_tasks = tbl_tasks)

