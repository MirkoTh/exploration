# # todos
# - choices
#   - fit separate decision trees for Thompson sampling and SMEB by participants
#   - use run length and nr previous switches as additional predictors
# - switches (i.e., switching behavior)
#   - same two models as above, but condition means, variances, and pmus on previous choice

library(tidyverse)
library(rpart)
library(caret)
library(dirichletprocess)
library(rpart.plot)
library(vip)
library(pdp)
library(rgl)
library(arm)
library(ISLR)


tbl_e2 <- read_csv("open-data/gershman-2018-e2-addon-features.csv")
tbl_rb <- read_csv("open-data/speekenbrink-konstantinidis-2015-addon-features.csv")

tbl_e2_switches <- tbl_e2 %>% 
  select(
    subject, block, trial, nr_previous_switches, run_nr, run_length, 
    p_prev, m_prev, v_prev, repeat_choice
  )


# Experiment 2 Gershman (2018) --------------------------------------------


tbl_cor <- cor(
  tbl_e2_switches[
    complete.cases(tbl_e2_switches),
    c("nr_previous_switches", "run_nr", "run_length", "p_prev", "m_prev", "v_prev", "repeat_choice")
  ]
) %>% as.data.frame() %>% as_tibble()
tbl_cor$x <- colnames(tbl_cor)
tbl_cor$x <- fct_inorder(tbl_cor$x)
levels(tbl_cor$x) <- c(
  "Nr. Prev. Switches", "Run Nr.", "Run Length", "PMU Prev.", "Mean Diff. Prev.", "Var. Diff. Prev", "Repeat Choice"
)
colnames(tbl_cor) <- c(levels(tbl_cor$x), "x")
tbl_cor <- tbl_cor %>% pivot_longer(cols = -x) 
tbl_cor$name <- fct_inorder(tbl_cor$name)

# are the predictors correlated?
tbl_cor %>%
  ggplot(aes(x, name)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis_c() +
  geom_text(aes(label = str_replace(round(value, 2), "(0)\\.", ".")), color = "black", size = 5) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  scale_y_discrete(limits = rev, expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_gradient2(name = "Correlation")
# some are: run_nr and nr_prev_switches -> exclude one of them
# mean prev and pmu prev, but they are not entered together into model anyway
# var prev and pmu prev, but same as above
# rest is more or less ok

baseline_one_subject <- function(subject_id, tbl_data) {
  m <- train(
    as.factor(repeat_choice) ~ p_prev, # + m_prev + v_prev,
    data = tbl_data %>% filter(subject == subject_id),
    method = "rpart",
    trControl = trainControl(method = "cv", number = 10),
    tuneLength = 20
  )
  return(m)
}

logistic_one_subject <- function(subject_id, tbl_data) {
  m <- train(
    as.factor(repeat_choice) ~ p_prev + m_prev + v_prev,
    data = tbl_data %>% filter(subject == subject_id),
    method = "bayesglm",
    trControl = trainControl(method = "cv", number = 10),
    tuneLength = 20
  )
  return(m)
}

strategies_one_subject <- function(subject_id, tbl_data) {
  m <- train(
    as.factor(repeat_choice) ~ nr_previous_switches + run_length + p_prev, #+ m_prev + v_prev,
    data = tbl_data %>% filter(subject == subject_id),
    method = "rpart",
    trControl = trainControl(method = "cv", number = 10),
    tuneLength = 20
  )
  return(m)
}
tbl_e2_switches$is_test <- tbl_e2_switches$block >= 16

subjs <- unique(tbl_e2_switches$subject)
l_ms_strategy_dev_test <- map(subjs, strategies_one_subject, tbl_data = tbl_e2_switches %>% filter(!is_test))
l_ms_baseline_dev_test <- map(subjs, baseline_one_subject, tbl_data = tbl_e2_switches %>% filter(!is_test))
l_ms_strategy_all <- map(subjs, strategies_one_subject, tbl_data = tbl_e2_switches)
l_ms_logistic_all <- map(subjs, logistic_one_subject, tbl_data = tbl_e2_switches)

tbl_comparison <- tibble(
  parametric = map_dbl(l_ms_logistic_all, ~ .x$finalModel$deviance),
  heuristic = map_dbl(l_ms_strategy_all, rpart_deviance)
) %>% 
  mutate(difference = parametric - heuristic)
tbl_agg <- tbl_comparison %>% count(n_better = difference > 0) %>%
  mutate(n_better = factor(n_better, labels = c("Logistic", "Heuristic"))) %>%
  pivot_wider(names_from = n_better, values_from = n)


ggplot(tbl_comparison, aes(difference)) +
  geom_histogram(fill = "skyblue2", color = "white") +
  geom_label(
    data = tbl_agg, aes(
      x = 30, y = 5, label = str_c(
        "Nr. Wins Logistic: ", Logistic, "\n",
        "Nr. Wins Heuristic: ", Heuristic)
      )) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-50, 50, by = 10)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Difference in Deviance", y = "Nr. Participants")


# model deviances

rpart_deviance <- function(mo) {
  mo$finalModel$frame %>%
    filter(var == "<leaf>") %>%
    mutate(
      deviance_i = -2 * (yval2[, 2] * log(pmax(1e-10, yval2[, 4])) + 
                           yval2[, 3] * log(pmax(1e-10, yval2[, 5])))
    ) %>%
    summarize(deviance_total = sum(deviance_i)) %>%
    as_vector()
}

tbl_deviances_cv <- tibble(
  subject = subjs,
  deviance_cv_baseline = map_dbl(l_ms_baseline_dev_test, rpart_deviance),
  deviance_cv_strategy = map_dbl(l_ms_strategy_dev_test, rpart_deviance),
  prop_improvement_cv = 1 - deviance_cv_strategy/deviance_cv_baseline
)

# on test set

rpart_deviance_test_set <- function(subj, tbl_e2_switches, subjs){
  subj_idx <- which(subjs == subj)
  tbl_baseline <- cbind(
    tbl_e2_switches %>% filter(is_test & subject == subj) %>% dplyr::select(repeat_choice),
    predict(l_ms_baseline_dev_test[[subj_idx]]$finalModel, newdata = tbl_e2_switches %>% filter(is_test & subject == subj))
  )
  tbl_strategy <- cbind(
    tbl_e2_switches %>% filter(is_test & subject == subj) %>% dplyr::select(repeat_choice),
    predict(l_ms_strategy_dev_test[[subj_idx]]$finalModel, newdata = tbl_e2_switches %>% filter(is_test & subject == subj))
  )
  tibble(
    subject = subj,
    deviance_test_baseline = -2 * sum(pmap_dbl(tbl_baseline, ~ log(pmax(1e-10, c(..2, ..3)[as.numeric(..1) + 1])))),
    deviance_test_strategy = -2 * sum(pmap_dbl(tbl_strategy, ~ log(pmax(1e-10, c(..2, ..3)[as.numeric(..1) + 1])))),
    prop_improvement_test = 1 - deviance_test_strategy / deviance_test_baseline
  )
}

tbl_deviance_test <- map(
  subjs, 
  rpart_deviance_test_set, 
  tbl_e2_switches = tbl_e2_switches, subjs = subjs
) %>%
  reduce(rbind)


tbl_deviances <- tbl_deviances_cv %>% 
  left_join(tbl_deviance_test, by = "subject", suffix = c("_cv", "_test"))


tbl_deviances %>% group_by(prop_improvement_cv > 0) %>% count()
tbl_deviances %>% group_by(prop_improvement_test > 0) %>% count()
tbl_deviances %>% group_by(prop_improvement_cv > 0 & prop_improvement_test > 0) %>% count()


par(mfrow=c(2, 2))
my_treeplot(l_ms_strategy_dev_test[[which(subjs == 20)]]$finalModel, "Prop. Improvement = .80")
my_treeplot(l_ms_strategy_dev_test[[which(subjs == 10)]]$finalModel, "Prop. Improvement = .51")
my_treeplot(l_ms_strategy_dev_test[[which(subjs == 24)]]$finalModel, "Prop. Improvement = .44")
my_treeplot(l_ms_strategy_dev_test[[which(subjs == 36)]]$finalModel, "Prop. Improvement = 0")

my_treeplot <- function(m, t) {
  prp(m, faclen=0, extra=105, roundint=F, digits=2, box.palette = "-GnRd", main = t)
}

draw.tree(l_ms_strategy_dev_test[[which(subjs == 7)]]$finalModel, cex = 1, digits = 2, nodeinfo = TRUE)
deviance_on_all_participants <- function(i, j, l_ms_strategy_dev_test, tbl_e2_switches, subjs) {
  subj_idx <- which(subjs == i)
  -2 * sum(pmap_dbl(cbind(
    tbl_e2_switches %>% filter(subject == j) %>% select(repeat_choice), 
    predict(l_ms_strategy_all[[subj_idx]]$finalModel, newdata = tbl_e2_switches %>% filter(subject == j))
  ), ~ log(pmax(1e-10, c(..2, ..3)[as.numeric(..1) + 1]))))
}

is <- subjs
js <- subjs
isnjs <- crossing(i = is, j = js)
isnjs$deviance <- pmap_dbl(isnjs, deviance_on_all_participants, l_ms_strategy_dev_test, tbl_e2_switches, subjs)
deviance_self <- isnjs %>% filter(i == j)
isnjs <- isnjs %>% 
  left_join(deviance_self %>% select(-i), by = "j", suffix = c("_all", "_self")) %>%
  mutate(prop_self = deviance_all / deviance_self)


ggplot(isnjs, aes(j, i)) +
  geom_tile(aes(fill = log(prop_self))) +
  scale_fill_gradient2(midpoint = 1.5, name = "Log(Deviance(j)/\nDeviance(i))") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Data Set Participant", y = "Model Fit on Participant")


# variable importance
make_my_list <- function(x, y) {
  tbl_design <- tibble(name = c("p_prev", "run_length", "nr_previous_switches")) %>% mutate(id = y)
  # tbl_design <- tibble(name = c("m_prev", "v_prev", "run_length", "nr_previous_switches")) %>% mutate(id = y)
  tbl_results <- as_tibble(data.frame(id = y, t(x$finalModel$variable.importance))) %>% pivot_longer(cols = -id) %>% select(-id)
  # tbl_results <- as_tibble(data.frame(id = y, t(x$finalModel$coefficients[2:4]))) %>% pivot_longer(cols = -id) %>% select(-id)
  tbl_design %>% left_join(tbl_results, by = "name")
}

tbl_results <- map2(l_ms, subjs, make_my_list) %>%
  reduce(rbind) %>% replace_na(list(value = 0)) %>%
  pivot_wider(id_cols = id, values_from = value, names_from = name)


nrow <- 2
ncol <- 3
my_subjects_e2 <- sample(unique(tbl_e2_switches$subject), nrow * ncol, replace = FALSE)
par(mfrow=c(nrow, ncol))
for (i in 1:(nrow*ncol)){
  rpart.plot(l_ms[[my_subjects_e2[i]]]$finalModel, cex = 1)
}


plot3d(
  x=tbl_results$p_prev, y=tbl_results$run_length, z=tbl_results$nr_previous_switches, 
  type = 's', 
  radius = 1,
  xlab="PMU Prev.", ylab="Run Length", zlab="Nr. Prev. Switches",
  #xlim = c(0, 15), ylim = c(0, 5), zlim = c(0, 5)
)

ggplot(tbl_results, aes(run_length, nr_previous_switches)) +
  geom_point()


# kmeans clustering
results_kmeans <- list()
for (k in 1:10) {
  results_kmeans[[k]] <- kmeans(tbl_results[, c("p_prev", "run_length", "nr_previous_switches")], k)
}

tbl_cluster_wss <- tibble(
  n_clusters = 1:length(results_kmeans),
  wss = map_dbl(results_kmeans, ~ sum(.x$withinss))
)

ggplot(tbl_cluster_wss, aes(n_clusters, wss)) +
  geom_point() +
  geom_line()

# dpmm clustering
its <- 1000
dp <- DirichletProcessMvnormal(tbl_results[, c("p_prev", "run_length", "nr_previous_switches")] %>% as.matrix())
dp <- Fit(dp, its)
plot(dp)




# Speekenbrink & Konstantinidis (2015) ------------------------------------

tbl_rb <- tbl_rb %>% select(-run_length) %>%
  rename(
    run_length = run_length_lagged, 
    nr_previous_switches = nr_previous_switches_lagged,
    repeat_choice = repeat_deck,
    m_prev = m_diff,
    v_prev = v_diff,
    subject = id2
  )

tbl_cor <- cor(
  tbl_rb[
    complete.cases(tbl_rb),
    c("nr_previous_switches", "run_nr", "run_length", "p_prev", "m_prev", "v_prev", "repeat_choice")
  ]
) %>% as.data.frame() %>% as_tibble()
tbl_cor$x <- colnames(tbl_cor)
tbl_cor$x <- fct_inorder(tbl_cor$x)
levels(tbl_cor$x) <- c(
  "Nr. Prev. Switches", "Run Nr.", "Run Length", "PMU Prev.", "Mean Diff. Prev.", "Var. Diff. Prev", "Repeat Choice"
)
colnames(tbl_cor) <- c(levels(tbl_cor$x), "x")
tbl_cor <- tbl_cor %>% pivot_longer(cols = -x) 
tbl_cor$name <- fct_inorder(tbl_cor$name)

# are the predictors correlated?
tbl_cor %>%
  ggplot(aes(x, name)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis_c() +
  geom_text(aes(label = str_replace(round(value, 2), "(0)\\.", ".")), color = "black", size = 5) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  scale_y_discrete(limits = rev, expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_gradient2(name = "Correlation")
# mean prev and pmu prev, but they are not entered together into model anyway
# var prev and pmu prev, but same as above
# rest is more or less ok



# id2 == 1 selected the same deck throughout the experiment
subjs <- unique(tbl_rb$subject)
subjs <- subjs[subjs != 1]
l_ms_rb <- map(subjs, strategies_one_subject, tbl_data = tbl_rb)


# for some participants the model does not converge, therefore use "safely"
tbl_results <- map2(l_ms_rb, subjs, safely(make_my_list)) %>%
  map("result") %>%
  reduce(rbind) %>% replace_na(list(value = 0)) %>%
  pivot_wider(id_cols = id, values_from = value, names_from = name)

nrow <- 4
ncol <- 3
my_subjects_rb <- sample(unique(tbl_results$id), nrow * ncol, replace = FALSE)
par(mfrow=c(nrow, ncol))
for (i in 1:(nrow*ncol)){
  rpart.plot(l_ms_rb[[my_subjects_rb[i]]]$finalModel, cex = 1)
}


plot3d(
  x=tbl_results$p_prev, y=tbl_results$run_length, z=tbl_results$nr_previous_switches, 
  type = 's', 
  radius = 1,
  xlab="PMU Prev.", ylab="Run Length", zlab="Nr. Prev. Switches",
  #xlim = c(0, 15), ylim = c(0, 5), zlim = c(0, 5)
)

ggplot(tbl_results, aes(run_length, nr_previous_switches)) +
  geom_point()

