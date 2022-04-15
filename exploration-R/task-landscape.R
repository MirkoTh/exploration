library(tidyverse)
library(factoextra)
library(ggrepel)
library(formattable)

home_grown_stuff <- c("exploration-R/utils.R")
walk(home_grown_stuff, source)

# create a list with tasks used previously

tbl_tasks <- tribble(
  ~task_name,           ~nr_arms, ~moving_stats, ~add_gen, ~experience, ~capacity, ~confound_rew_inf, ~nr_samples, 
  "Observe Or Bet",     "few",    FALSE,         FALSE,    TRUE,         1,        FALSE,             "fixed",
  "Two-Armed Bandit",   "few",    FALSE,         FALSE,    TRUE,         1,        TRUE,              "fixed",
  "Restless Bandit",    "few",    TRUE,          FALSE,    TRUE,         2,        TRUE,              "fixed",
  "Grid Task",          "many",   FALSE,         TRUE,     TRUE,         3,        TRUE,              "fixed",
  "DFE",                "few",    FALSE,         FALSE,    TRUE,         1,        TRUE,              "fixed",
  "DFE Sampling",       "few",    FALSE,         FALSE,    TRUE,         1,        FALSE,             "unlimited",
  "DFD",                "few",    FALSE,         FALSE,    FALSE,        0,        FALSE,             "fixed",
)

tbl_tasks <- format_task_tbl(tbl_tasks)

# a task table
plot_task_table(tbl_tasks)
x11()
plot_labels_task(tbl_tasks)
x11()
plot_pca(tbl_tasks = tbl_tasks)

