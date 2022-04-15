library(tidyverse)
library(factoextra)
library(ggrepel)
library(formattable)

home_grown_stuff <- c("exploration-R/utils.R")
walk(home_grown_stuff, source)

# create a list with tasks used previously

tbl_tasks <- tribble(
  ~task_name,           ~nr_arms, ~moving_stats, ~add_gen, ~experience, ~capacity, ~confound_rew_inf, 
  "Observe Or Bet",     "few",    FALSE,         FALSE,    TRUE,         1,        FALSE,
  "Two-Armed Bandit",   "few",    FALSE,         FALSE,    TRUE,         1,        TRUE,
  "Restless Bandit",    "few",    TRUE,          FALSE,    TRUE,         2,        TRUE,
  "Grid Task",          "many",   FALSE,         TRUE,     TRUE,         3,        TRUE,
  "DFE",                "few",    FALSE,         FALSE,    TRUE,         1,        TRUE,
  "DFE Sampling",       "few",    FALSE,         FALSE,    TRUE,         1,        FALSE,
  "DFD",                "few",    FALSE,         FALSE,    FALSE,        0,        FALSE,
)

tbl_tasks <- format_task_tbl(tbl_tasks)

# a task table
plot_task_table(tbl_tasks)
x11()
plot_labels_task(tbl_tasks)
x11()
plot_pca(tbl_tasks = tbl_tasks)

