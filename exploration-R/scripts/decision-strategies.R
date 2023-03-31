# # todos
# - choices
#   - fit separate decision trees for Thompson sampling and SMEB by participants
#   - use run length and nr previous switches as additional predictors
# - switches (i.e., switching behavior)
#   - same two models as above, but condition means, variances, and pmus on previous choice
library(vip)
library(pdp)
library(rgl)
library(arm)
library(ISLR)
library(tidyverse)
library(rpart)
library(caret)
library(dirichletprocess)
library(rpart.plot)
library(furrr)
library(future)
library(gridExtra)
library(grid)


home_grown <- c("exploration-R/utils/utils.R", "exploration-R/utils/plotting.R")
walk(home_grown, source)



tbl_e2 <- read_csv("open-data/gershman-2018-e2-addon-features.csv")
tbl_rb <- read_csv("open-data/speekenbrink-konstantinidis-2015-addon-features.csv")

# using pmu as in S&K (2015) or v/tu as in Gershman (2018) is empirically almost equivalent
# they correlate with .89

tbl_e2$v <- tbl_e2$m_1 - tbl_e2$m_2
tbl_e2$ru <- sqrt(tbl_e2$v_1) - sqrt(tbl_e2$v_2)
tbl_e2$tu <- sqrt(tbl_e2$v_1 + tbl_e2$v_2)
tbl_e2$thompson <- tbl_e2$v / tbl_e2$tu

cor(tbl_e2$thompson, tbl_e2$p_diff)

tbl_e2_switches <- tbl_e2 %>% 
  dplyr::select(
    subject, block, trial, nr_previous_switches, run_nr, run_length, 
    p_prev, m_prev, v_prev, repeat_choice
  )




# Experiment 2 Gershman (2018) --------------------------------------------

tbl_cor <- cor(
  tbl_e2_switches[
    complete.cases(tbl_e2_switches),
    c("nr_previous_switches", "run_length", "p_prev", "m_prev", "v_prev", "repeat_choice") #"run_nr", 
  ]
) %>% as.data.frame() %>% as_tibble()
tbl_cor$x <- colnames(tbl_cor)
tbl_cor$x <- fct_inorder(tbl_cor$x)
levels(tbl_cor$x) <- c(
  "Nr Switches", "Run Length", "PMU", "Mean Diff", "Var Diff", "Rep Choice" # "Run Nr", 
)
colnames(tbl_cor) <- c(levels(tbl_cor$x), "x")
tbl_cor <- tbl_cor %>% pivot_longer(cols = -x) 
tbl_cor$name <- fct_inorder(tbl_cor$name)

# are the predictors correlated?
pl_var_cor <- tbl_cor %>%
  ggplot(aes(x, name)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis_c() +
  geom_text(aes(label = str_replace(round(value, 2), "(0)\\.", "."), color = value), size = 5) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  scale_y_discrete(limits = rev, expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_gradient2(name = "Correlation", high = "#440154", low = "#21918c", guide = "none") +
  scale_color_gradient2(high = "white", low = "black", guide = "none", midpoint = .3)
# some are: run_nr and nr_prev_switches -> exclude one of them
# mean prev and pmu prev, but they are not entered together into model anyway
# var prev and pmu prev, but same as above
# rest is more or less ok


baseline_one_subject <- function(subject_id, seed, tbl_data) {
  set.seed(seed)
  m <- train(
    as.factor(repeat_choice) ~ m_prev, # + "Value Diff." + "V Diff.",
    data = tbl_data %>% filter(subject == subject_id),
    method = "rpart",
    trControl = trainControl(method = "cv", number = 10),
    tuneLength = 20
  )
  return(m)
}

logistic_one_subject <- function(subject_id, seed, tbl_data) {
  set.seed(seed)
  m <- train(
    as.factor(repeat_choice) ~ p_prev + m_prev + v_prev,
    data = tbl_data %>% filter(subject == subject_id),
    method = "bayesglm",
    trControl = trainControl(method = "cv", number = 10),
    tuneLength = 20
  )
  return(m)
}

strategies_one_subject <- function(subject_id, seed, tbl_data) {
  set.seed(seed)
  m <- train(
    as.factor(repeat_choice) ~ nr_previous_switches + run_length + m_prev, #+ m_prev + v_prev,
    data = tbl_data %>% filter(subject == subject_id),
    method = "rpart",
    trControl = trainControl(method = "cv", number = 10),
    tuneLength = 20
  )
  return(m)
}
tbl_e2_switches$is_test <- tbl_e2_switches$block >= 16

subjs <- unique(tbl_e2_switches$subject)
set.seed(43219)
seeds <- round(rnorm(length(subjs), 1000000, 10000), 0)

tbl_e2_switches_z <- tbl_e2_switches %>%
  group_by(subject) %>%
  mutate(
    nr_previous_switches = scale(nr_previous_switches)[, 1],
    run_length = scale(run_length)[, 1],
    p_prev = scale(p_prev)[, 1],
    m_prev = scale(m_prev)[, 1],
    v_prev = scale(v_prev)[, 1]
  ) %>% ungroup()


plan(multisession, workers = 1)#future::availableCores() - 4)

l_ms_strategy_dev_test <- future_map2(
  subjs, seeds, strategies_one_subject, tbl_data = tbl_e2_switches %>% filter(!is_test),
  .progress = TRUE, .options = furrr_options(seed = TRUE)
  )
l_ms_baseline_dev_test <- future_map2(
  subjs, seeds, baseline_one_subject, tbl_data = tbl_e2_switches %>% filter(!is_test),
  .progress = TRUE, .options = furrr_options(seed = TRUE)
  )
l_ms_baseline_all <- future_map2(
  subjs, seeds, baseline_one_subject, tbl_data = tbl_e2_switches,
  .progress = TRUE, .options = furrr_options(seed = TRUE)
  )
l_ms_strategy_all <- future_map2(
  subjs, seeds, strategies_one_subject, tbl_data = tbl_e2_switches,
  .progress = TRUE, .options = furrr_options(seed = TRUE)
  )

# make sure logistic model converges
converge <- -1
while (converge == -1){
  tryCatch(
    warning = function(cnd) {
      cat("did not converge\n")
      converge <- -1
    }, {
      seeds <- round(rnorm(length(subjs), 1000000, 10000), 0)
      l_ms_logistic_all <- future_map2(
        subjs, seeds, logistic_one_subject, tbl_data = tbl_e2_switches_z,
        .progress = TRUE, .options = furrr_options(seed = TRUE)
        )
      converge <- "get out"
    }
  )
}

n_splits <- map_dbl(
  l_ms_strategy_all, ~ .x$finalModel$frame %>% 
    filter(var != "<leaf>") %>% 
    count() %>% as_vector()
)

mean(n_splits)
table(n_splits)
table(n_splits)/length(n_splits)

tbl_logistic <- map(
  l_ms_logistic_all, ~ .x$finalModel$coefficients
) %>% reduce(rbind) %>%
  as.data.frame() %>%
  as_tibble()

colnames(tbl_logistic) <- c("Intercept", "PMU", "Value Diff", "Var Diff")
tbl_logistic_long <- tbl_logistic %>% 
  mutate(subject = subjs) %>% 
  relocate(PMU, .after = `Var Diff`) %>%
  dplyr::select(-Intercept) %>%
  pivot_longer(-subject) %>%
  mutate(name = fct_inorder(factor(name)))
cor(tbl_logistic[, c("PMU", "Value Diff", "Var Diff")])

ggplot(tbl_logistic_long, aes(value, group = name)) +
  geom_histogram(aes(fill = name), color = "white") +
  facet_wrap(~ name) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(name = "Parameter")

pl_logistic_violin <- ggplot(tbl_logistic_long, aes(name, value, group = name)) +
  geom_violin(aes(fill = name))  + 
  geom_dotplot(
    binaxis = "y", stackdir = "center", 
    dotsize = .5, binwidth = .3, alpha = .2
  ) +
  geom_line(aes(group = subject), alpha = .2) +
  geom_boxplot(width = 0.2, alpha = .5) +
  theme_bw() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(guide = "none") +
  labs(x = "Parameter", y = "Coefficient")

# kmeans clustering
results_kmeans <- list()
for (k in 1:10) {
  results_kmeans[[k]] <- kmeans(tbl_logistic[, c("PMU", "Value Diff.", "Var. Diff.")], k)
}

tbl_cluster_wss <- tibble(
  n_clusters = 1:length(results_kmeans),
  wss = map_dbl(results_kmeans, ~ sum(.x$withinss))
)

ggplot(tbl_cluster_wss, aes(n_clusters, wss)) +
  geom_point() +
  geom_line()
tbl_logistic$cluster <- results_kmeans[[4]]$cluster
tbl_logistic %>% group_by(cluster) %>%
  summarize(
    mn_pmu = mean(PMU),
    mn_val = mean(`Value Diff.`),
    mn_var = mean(`Var. Diff.`),
    n_participants = n()
  )

plot3d(
  x=tbl_logistic$PMU, y=tbl_logistic$`Value Diff.`, z=tbl_logistic$`Var. Diff.`, 
  type = 's', 
  radius = .1,
  xlab="Thompson", ylab="Value-Guided", zlab="Directed",
  col = tbl_logistic$cluster
  #xlim = c(0, 15), ylim = c(0, 5), zlim = c(0, 5)
)
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

tbl_comparison <- tibble(
  parametric = map_dbl(l_ms_logistic_all, ~ .x$finalModel$deviance),
  heuristic = map_dbl(l_ms_strategy_all, rpart_deviance)
) %>% 
  mutate(difference = parametric - heuristic)
tbl_agg <- tbl_comparison %>% count(n_better = difference > 0) %>%
  mutate(n_better = factor(n_better, labels = c("Logistic", "Heuristic"))) %>%
  pivot_wider(names_from = n_better, values_from = n)


pl_deviance_comp <- ggplot(tbl_comparison, aes(difference)) +
  geom_histogram(fill = "#21918c", color = "white") +
  geom_label(
    data = tbl_agg, aes(
      x = -25, y = 4.5, label = str_c(
        "#Wins Logistic: ", Logistic, "\n",
        "#Wins Heuristic: ", Heuristic)
    )) +
  geom_vline(xintercept = 0, color = "grey80", linetype = "dotdash", size = .75) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-80, 80, by = 10)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Difference in Deviance", y = "Nr. Participants")


tbl_deviances_cv <- tibble(
  subject = subjs,
  deviance_cv_baseline = map_dbl(l_ms_baseline_all, rpart_deviance),
  deviance_cv_strategy = map_dbl(l_ms_strategy_all, rpart_deviance),
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

my_treeplot <- function(m, t) {
  prp(m, faclen=0, extra=105, roundint=F, digits=2, box.palette = "-GnRd", main = t)
}

l_ms_strategy_all[[which(subjs == 35)]]$finalModel$frame <- l_ms_strategy_all[[which(subjs == 35)]]$finalModel$frame %>%
  mutate(
    var = str_replace(var, "p_prev", "PMU"),
    var = str_replace(var, "nr_previous_switches", "Nr. Prev. Switches"),
    var = str_replace(var, "m_prev", "Value Diff.")
  )

l_ms_strategy_all <- map(
  l_ms_strategy_all, function(x) {
    x$finalModel$frame <- x$finalModel$frame %>%
      mutate(
        var = str_replace(var, "p_prev", "PMU"),
        var = str_replace(var, "nr_previous_switches", "Nr. Prev. Switches"),
        var = str_replace(var, "run_length", "L(Run)"),
        var = str_replace(var, "m_prev", "Value Diff.")
      )
    return(x)
  } )

deviance_on_all_participants <- function(i, j, l_ms_strategy_all, tbl_e2_switches, subjs) {
  subj_idx <- which(subjs == i)
  -2 * sum(pmap_dbl(cbind(
    tbl_e2_switches %>% filter(subject == j) %>% dplyr::select(repeat_choice), 
    predict(l_ms_strategy_all[[subj_idx]]$finalModel, newdata = tbl_e2_switches %>% filter(subject == j))
  ), ~ log(pmax(1e-10, c(..2, ..3)[as.numeric(..1) + 1]))))
}

is <- subjs
js <- subjs
isnjs <- crossing(i = is, j = js)
isnjs$deviance <- pmap_dbl(isnjs, deviance_on_all_participants, l_ms_strategy_all, tbl_e2_switches, subjs)
deviance_self <- isnjs %>% filter(i == j)
isnjs <- isnjs %>% 
  left_join(
    deviance_self %>% 
      dplyr::select(-i), by = "j", suffix = c("_all", "_self")
  ) %>%
  mutate(prop_self = deviance_all / deviance_self)


isnjs_excl_self <- isnjs %>%
  filter(i != j) %>%
  arrange(prop_self) %>%
  mutate(cumprop = rank(prop_self)/max(rank(prop_self)))
mn_isnjs <- mean(isnjs_excl_self$prop_self)
md_isnjs <- median(isnjs_excl_self$prop_self)
idx_mn <- min(which((isnjs_excl_self$prop_self >= mn_isnjs) == 1))
cumprop_mn <- isnjs_excl_self$cumprop[idx_mn]

deviance_random <- -2*(log(.5) * nrow(tbl_e2_switches %>% filter(subject == 1)))
deviance_self$random <- deviance_random
deviance_self$prop_random <- deviance_self$random / deviance_self$deviance
rd_model <- mean(deviance_self$prop_random)
idx_rd <- min(which((isnjs_excl_self$prop_self >= rd_model) == 1))
cumprop_rd <- isnjs_excl_self$cumprop[idx_rd]

pl_deviance_variability <- ggplot(isnjs_excl_self, aes(prop_self, cumprop)) +
  geom_line() +
  coord_cartesian(xlim = c(0, 5)) +
  geom_segment(aes(y = .5, yend = .5, x = 0, xend = md_isnjs)) +
  geom_segment(aes(y = 0, yend = .5, x = md_isnjs, xend = md_isnjs)) +
  geom_segment(aes(y = cumprop_mn, yend = cumprop_mn, x = 0, xend = mn_isnjs)) +
  geom_segment(aes(y = 0, yend = cumprop_mn, x = mn_isnjs, xend = mn_isnjs)) +
  geom_segment(aes(y = cumprop_rd, yend = cumprop_rd, x = 0, xend = rd_model)) +
  geom_segment(aes(y = 0, yend = cumprop_rd, x = rd_model, xend = rd_model)) +
  geom_label(aes(
    x = 3.5, y = .375, label = str_c(
      "Median = ", round(md_isnjs, 2),
      "\nMean = ", round(mn_isnjs, 2),
      "\nRandom = ", round(rd_model, 2)
      )
    )) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Prop. Deviance", y = "Empirical CDF")



# Plotting ----------------------------------------------------------------


pdf(file="figures/exemplary-heuristics.pdf", 4, 4)
par(mfrow=c(2, 2))
my_treeplot(l_ms_strategy_all[[which(subjs == 35)]]$finalModel, "Prop. Improvement = .61")
my_treeplot(l_ms_strategy_all[[which(subjs == 20)]]$finalModel, "Prop. Improvement = .54")
my_treeplot(l_ms_strategy_all[[which(subjs == 7)]]$finalModel, "Prop. Improvement = .40")
my_treeplot(l_ms_strategy_all[[which(subjs == 36)]]$finalModel, "Prop. Improvement = 0")
dev.off()

save_my_pdf(pl_var_cor, "figures/heuristics-var-corr.pdf", 5.5, 5)
pl_log_and_dev <- arrangeGrob(pl_logistic_violin, pl_deviance_comp, nrow = 1)
save_my_pdf(pl_log_and_dev, "figures/heuristics-logistic-model-comparison.pdf", 8, 4)
save_my_pdf(pl_deviance_variability, "figures/deviance-variability.pdf", 3.75, 3.5)


pl_cor_and_logistic <- arrangeGrob(pl_var_cor, pl_logistic_violin, nrow = 1)
save_my_pdf(pl_log_and_dev, "figures/heuristics-logistic-model-comparison.pdf", 8, 4)
save_my_pdf(pl_cor_and_logistic, "figures/var-corr-and-logistic-violin.pdf", 9, 4.5)

vp.BottomRight <- viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"), 
                           just=c("left","top"), 
                           y=0.5, x=0.5)

pdf(file="figures/exemplary-heuristics-and-heuristics-variability.pdf", 7.5, 5.5)
par(mfrow=c(2,2))
my_treeplot(l_ms_strategy_all[[which(subjs == 35)]]$finalModel, "Prop. Improvement = .61")
my_treeplot(l_ms_strategy_all[[which(subjs == 20)]]$finalModel, "Prop. Improvement = .54")
my_treeplot(l_ms_strategy_all[[which(subjs == 7)]]$finalModel, "Prop. Improvement = .40")
# plot the ggplot using the print command
print(pl_deviance_variability, vp=vp.BottomRight)
dev.off()

ggplot(isnjs, aes(prop_self)) + 
  geom_histogram(binwidth = .25, fill = "#21918c", color = "white") + 
  coord_cartesian(xlim = c(0, 15)) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Deviance Ratio", y = "Nr. Comparisons")

ggplot(isnjs, aes(j, i)) +
  geom_tile(aes(fill = prop_self)) +
  scale_fill_gradient2(name = "Log10(Deviance(Best Model)/\nDeviance(Other Model))", low = "#fde725", high = "#21918c") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Data Set Participant", y = "Model Fit on Participant")


# variable importance
make_my_list <- function(x, y) {
  tbl_design <- tibble(name = c("m_prev", "run_length", "nr_previous_switches")) %>% mutate(id = y)
  # tbl_design <- tibble(name = c("Value Diff.", "V Diff.", "L(Run)", "Nr. Switches")) %>% mutate(id = y)
  tbl_results <- as_tibble(data.frame(id = y, t(x$finalModel$variable.importance))) %>% 
    pivot_longer(cols = -id) %>% 
    dplyr::select(-id)
  # tbl_results <- as_tibble(data.frame(id = y, t(x$finalModel$coefficients[2:4]))) %>% pivot_longer(cols = -id) %>% select(-id)
  tbl_design %>% left_join(tbl_results, by = "name")
}

tbl_results <- map2(l_ms_strategy_all, subjs, make_my_list) %>%
  reduce(rbind) %>% replace_na(list(value = 0)) %>%
  pivot_wider(id_cols = id, values_from = value, names_from = name)
tbl_results <- tbl_results %>% 
  rename(`Val Diff` = m_prev, `L(Run)` = run_length, `Nr. Switches` = nr_previous_switches)

plot3d(
  x=tbl_results$`Val Diff`, y=tbl_results$`L(Run)`, z=tbl_results$`Nr. Switches`, 
  type = 's', 
  radius = 1,
  xlab="Val Diff", ylab="Run Length", zlab="Nr. Prev. Switches",
  #xlim = c(0, 15), ylim = c(0, 5), zlim = c(0, 5)
)

ggplot(tbl_results, aes(`L(Run)`, `Nr. Switches`)) +
  geom_point()


# kmeans clustering
results_kmeans <- list()
for (k in 1:10) {
  results_kmeans[[k]] <- kmeans(tbl_results[, c("Val Diff", "L(Run)", "Nr. Switches")], k)
}

tbl_cluster_wss <- tibble(
  n_clusters = 1:length(results_kmeans),
  wss = map_dbl(results_kmeans, ~ sum(.x$withinss))
)

ggplot(tbl_cluster_wss, aes(n_clusters, wss)) +
  geom_point() +
  geom_line()

tbl_results$cluster <- results_kmeans[[4]]$cluster

cols_viridis <- tibble(
  cluster = seq(1, 4, by = 1),
  color = c("#fde725", "#5ec962", "#21918c", "#3b528b", "#440154")[1:4],
  clustername = c("PMU & Run Length", "PMU & Switches", "All Three #1", "All Three #2", "Only PMU")[1:4]
)
tbl_results <- tbl_results %>% left_join(cols_viridis, by = "cluster")
tbl_results %>% group_by(cluster) %>% summarize(mean(`Val Diff`), mean(`L(Run)`), mean(`Nr. Switches`), n())

plot3d(
  x=tbl_results$`Val Diff`, y=tbl_results$"L(Run)", z=tbl_results$"Nr. Switches", 
  type = 's', 
  radius = 1,
  xlab="PMU Prev.", ylab="Run Length", zlab="Nr. Prev. Switches",
  col = tbl_results$color
  #xlim = c(0, 15), ylim = c(0, 5), zlim = c(0, 5)
)
legend3d("topright", legend = cols_viridis$clustername, pch = 16, col = cols_viridis$color, cex=1.7, inset=c(0.02))


par(mfrow=c(2, 2))
my_treeplot(l_ms_strategy_all[[which(subjs == 36)]]$finalModel, "Only PMU")
my_treeplot(l_ms_strategy_all[[which(subjs == 15)]]$finalModel, "PMU & Run Length")
my_treeplot(l_ms_strategy_all[[which(subjs == 5)]]$finalModel, "PMU & Switches")
my_treeplot(l_ms_strategy_all[[which(subjs == 42)]]$finalModel, "All Three #1")


# Speekenbrink & Konstantinidis (2015) ------------------------------------

tbl_rb <- tbl_rb %>% select(-"L(Run)") %>%
  rename(
    "L(Run)" = "L(Run)"_lagged, 
    "Nr. Switches" = "Nr. Switches"_lagged,
    repeat_choice = repeat_deck,
    "Value Diff." = m_diff,
    "V Diff." = v_diff,
    subject = id2
  )

tbl_cor <- cor(
  tbl_rb[
    complete.cases(tbl_rb),
    c("Nr. Switches", "run_nr", "L(Run)", "PMU", "Value Diff.", "V Diff.", "repeat_choice")
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
  x=tbl_results$PMU, y=tbl_results$"L(Run)", z=tbl_results$"Nr. Switches", 
  type = 's', 
  radius = 1,
  xlab="PMU Prev.", ylab="Run Length", zlab="Nr. Prev. Switches",
  #xlim = c(0, 15), ylim = c(0, 5), zlim = c(0, 5)
)

ggplot(tbl_results, aes("L(Run)", "Nr. Switches")) +
  geom_point()



