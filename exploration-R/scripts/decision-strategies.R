# # todos
# - choices
#   - fit separate decision trees for Thompson sampling and SMEB by participants
#   - use run length and nr previous switches as additional predictors
# - switches (i.e., switching behavior)
#   - same two models as above, but condition means, variances, and pmus on previous choice


# Helper packages
library(tidyverse)     # for awesome plotting

# Modeling packages
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects


tbl_e2 <- read_csv("open-data/gershman-2018-e2-addon-features.csv")
tbl_rb <- read_csv("open-data/speekenbrink-konstantinidis-2015-addon-features.csv")

tbl_e2_switches <- tbl_e2 %>% 
  select(subject, nr_previous_switches, run_nr, run_length, p_prev, m_prev, v_prev, repeat_choice)

tbl_e2_s1 <- tbl_e2_switches %>% filter(subject == 1)



# Preprocessing -----------------------------------------------------------


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
  geom_text(aes(label = str_replace(round(value, 2), "(0)\\.", ".")), color = "white", size = 5) +
  theme(
    axis.text.x = element_text(angle = 90), 
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  scale_y_discrete(limits = rev) +
  scale_fill_viridis_c(name = "Correlation")
# some are: run_nr and nr_prev_switches -> exclude one of them
# mean prev and pmu prev, but they are not entered together into model anyway
# var prev and pmu prev, but same as above
# rest is more or less ok

e2_dt_cv <- train(
  as.factor(repeat_choice) ~ nr_previous_switches + run_length + m_prev + v_prev,
  data = tbl_e2_switches %>% filter(subject == 4),
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 20
)
vip(e2_dt_cv, num_features = 4, bar = FALSE)
rpart.plot(e2_dt_cv$finalModel)
rules <- rpart.rules(e2_dt_cv$finalModel)
rules$.outcome



