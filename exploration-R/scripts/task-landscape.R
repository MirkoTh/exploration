library(tidyverse)
library(factoextra)
library(ggrepel)
library(formattable)

home_grown_stuff <- c("utils/utils.R")
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

tbl_tasks2 <- tribble(
  ~task_name,           ~trials, ~nr_arms, ~moving_stats, ~capacity, ~confound_rew_inf, 
  "Two-Armed Bandit",   "k = n", "2",      FALSE,         2,        TRUE,             
  "Restless Bandit",    "k = n", "4",      TRUE,          3,        TRUE,             
  "Horizon Task",       "k = 1/3","2",     FALSE,         1,        FALSE,
  "Observe Or Bet",     "k = n - o", "2",  FALSE,         2,        TRUE,
  "DFE",                "k = 1", "2",      FALSE,         2,        TRUE,             
  "DFE Sampling",       "k = 1", "2",      FALSE,         2,        FALSE,            
)

plot_task_table2(tbl_tasks2)
plot_cut_task_table2(tbl_tasks2)



tbl_studies <- tribble(
  ~authorlist,            ~driftcondition,  ~directed,    ~valueguided,
  "Gershman (2018)",      FALSE,            TRUE,         TRUE,
  "Wilson et al. (2014)", FALSE,            TRUE,         TRUE,
  "Speekenbrink & Konstantinidis (2015)", TRUE, FALSE,    TRUE,
  "Daw et al. (2006)",    TRUE,             FALSE,        TRUE,
  "Wu et al. (2022)",     FALSE,            TRUE,         TRUE,
  "Dubois et al. (2021)", FALSE,            TRUE,         TRUE
)
