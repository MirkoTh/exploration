# # todos
# - choices
#   - fit separate decision trees for Thompson sampling and SMEB by participants
#   - use run length and nr previous switches as additional predictors
# - switches (i.e., switching behavior)
#   - same two models as above, but condition means, variances, and pmus on previous choice

library(tidyverse)     # for awesome plotting
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application
library(dirichletprocess)
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects
library(rgl)


tbl_e2 <- read_csv("open-data/gershman-2018-e2-addon-features.csv")
tbl_rb <- read_csv("open-data/speekenbrink-konstantinidis-2015-addon-features.csv")

tbl_e2_switches <- tbl_e2 %>% 
  select(subject, nr_previous_switches, run_nr, run_length, p_prev, m_prev, v_prev, repeat_choice)



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



strategies_one_subject <- function(subject_id, tbl_data) {
  m <- train(
    as.factor(repeat_choice) ~ nr_previous_switches + run_length + p_prev, #m_prev + v_prev,
    data = tbl_data %>% filter(subject == subject_id),
    method = "rpart",
    trControl = trainControl(method = "cv", number = 10),
    tuneLength = 20
  )
  return(m)
}

subjs <- unique(tbl_e2_switches$subject)
l_ms <- map(subjs, strategies_one_subject, tbl_data = tbl_e2_switches)


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


nrow <- 4
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



