rm(list = ls())
# Set Up Script -----------------------------------------------------------



library(tidyverse)
library(lme4)
library(rutils)
library(grid)
library(gridExtra)
library(cmdstanr)
library(bayesplot)
library(ggbeeswarm)


home_grown <- c(
  "exploration-R/utils/utils.R", 
  "exploration-R/utils/plotting.R",
  "exploration-R/utils/stan-models.R"
)
walk(home_grown, source)




# Prepare Data ------------------------------------------------------------


# difference scores are always option 1 - option 2
tbl_e1_prep <- read_csv("exploration-R/data/fan-exp1_bandit_task_scale.csv")
tbl_e1 <- tbl_e1_prep %>% 
  select(
    sub, block, trial, C, V, RU, TU, V_old, RU_old, TU_old, 
    Factor1_Somatic_Anxiety, Factor2_Cognitive_Anxiety, 
    Factor3_Negative_Affect, Factor4_Low_Self_esteem
  )
tbl_e1$VTU_old <- tbl_e1$V_old / tbl_e1$TU_old
tbl_e1$VTU <- scale(tbl_e1$VTU_old)[, 1]

tbl_cor <- tbl_e1 %>% group_by(sub) %>% summarize(r_v = cor(V, C), r_vtu = cor(VTU, C), r_v_vtu = cor(V, VTU))

l_tbl_e1 <- split(tbl_e1, tbl_e1$sub)
sub_ids <- map_dbl(l_tbl_e1, ~ as_vector(.x[1, "sub"]))
l_models <- map(l_tbl_e1, ~ glm(C ~ V + VTU + RU, data = .x))
l_vifs <- map(l_models, car::vif)
tbl_vifs <- reduce(l_vifs, rbind) %>%
  as.data.frame() %>% as_tibble() %>%
  mutate(sub = sub_ids) %>%
  relocate(sub, .before = V)
sub_incl <- tbl_vifs$sub#[(tbl_vifs$V <= 3 | tbl_vifs$VTU <= 3)]

tbl_subset <- tbl_e1 %>% filter(sub %in% sub_incl)
tbl_subset$V_cut <- cut(tbl_subset$V, c(-10, -.1, .1, 10), labels = FALSE)
tbl_subset$VTU_cut <- cut(tbl_subset$VTU, c(-10, -.1, .1, 10), labels = FALSE)
tbl_subset$RU_cut <- cut(tbl_subset$RU, c(-10, -.1, .1, 10), labels = FALSE)

tbl_subset %>% 
  filter(V_cut == 2 & VTU_cut == 2 & RU_cut == 2) %>%
  group_by(sub, V_cut, VTU_cut, RU_cut) %>%
  summarize(c_mn = mean(C)) %>%
  ggplot(aes(c_mn)) + geom_histogram()

tbl_choices_agg <- tbl_e1 %>% group_by(sub) %>% summarize(n = n(), C1 = sum(C))
cor(tbl_subset[, c("V", "VTU", "RU", "C")])

# use a random subset to test model predictions
# set seed to make results replicable
set.seed(99839)
tbl_predict <- tbl_subset %>% group_by(sub) %>%
  mutate(
    randi = runif(length(block)),
    ranking = row_number(randi)
  ) %>%
  arrange(sub, ranking) %>%
  filter(ranking <= 30)

# compare models including a (random) intercept term against model omitting it
# fit on whole data set
# compare with loo model weights

n_samples <- 5000
n_warmup <- 2000
n_chains <- 3

# vtu + ru vs. intercept + vtu + ru ---------------------------------------


mm_choice_vtu <- model.matrix(
  C ~ VTU + RU, data = tbl_subset
) %>% as_tibble()
colnames(mm_choice_vtu) <- c("ic", "VTU", "RU")
mm_choice_vtu$sub <- tbl_subset$sub

mm_choice_vtu_predict <- model.matrix(
  C ~ VTU + RU, data = tbl_predict
) %>% as_tibble()
colnames(mm_choice_vtu_predict) <- c("ic", "VTU", "RU")
mm_choice_vtu_predict$sub <- tbl_predict$sub


l_data <- list(
  n_data = nrow(mm_choice_vtu),
  n_subj = length(unique(mm_choice_vtu$sub)),
  choice = tbl_subset$C,
  subj = as.numeric(factor(
    tbl_subset$sub, 
    labels = 1:length(unique(tbl_subset$sub))
  )),
  n_data_predict = nrow(mm_choice_vtu_predict),
  choice_predict = tbl_predict$C,
  subj_predict = as.numeric(factor(
    tbl_predict$sub, 
    labels = 1:length(unique(tbl_predict$sub))
  ))
)

with_intercept <- c(TRUE, FALSE)
pars_interest <- c("mu_tf", "sigma_subject", "b")
for (ic in with_intercept) {
  path_ending <- c("no-intercept", "intercept")[ic + 1]
  # select model
  if (ic){
    l_data$x <- as.matrix(mm_choice_vtu[, c("ic", "VTU", "RU")])
    l_data$x_predict <- as.matrix(mm_choice_vtu_predict[, c("ic", "VTU", "RU")])
    choice_model_ic <- stan_choice_reduced_intercept()
    mod_choice <- cmdstan_model(choice_model_ic)
  } else if (!ic) {
    l_data$x <- as.matrix(mm_choice_vtu[, c("VTU", "RU")])
    l_data$x_predict <- as.matrix(mm_choice_vtu_predict[, c("VTU", "RU")])
    choice_model_no_ic <- stan_choice_reduced_no_intercept()
    mod_choice <- cmdstan_model(choice_model_no_ic)
  }
  
  # run model
  fit_choice_vtu <- mod_choice$sample(
    data = l_data, iter_sampling = n_samples, iter_warmup = n_warmup, chains = n_chains,
    init = 1, parallel_chains = n_chains
  )
  # save mcmc samples and summary stats
  tbl_summary_vtu <- fit_choice_vtu$summary(variables = pars_interest)
  if(max(tbl_summary_vtu$rhat) > 1.02){stop(str_c("Rhat values too large\ncloser inspect VTU ", path_ending, " model"))}
  
  tbl_draws_vtu <- fit_choice_vtu$draws(variables = pars_interest, format = "df")
  if (ic){
    pl_caterpillar <- bayesplot::mcmc_trace(tbl_draws_vtu,  pars = c("mu_tf[1]", "mu_tf[2]", "mu_tf[3]"), n_warmup = n_warmup) + ggtitle(str_c("VTU & ", path_ending))
  } else if (!ic) {
    pl_caterpillar <- bayesplot::mcmc_trace(tbl_draws_vtu,  pars = c("mu_tf[1]", "mu_tf[2]"), n_warmup = n_warmup) + ggtitle(str_c("VTU & ", path_ending))
  }
  loo <- fit_choice_vtu$loo(variables = "log_lik_pred")
  
  saveRDS(loo, file = str_c("exploration-R/data/loo-vtu-", path_ending, ".rds"))
  saveRDS(tbl_draws_vtu, file = str_c("exploration-R/data/choice-model-vtu-", path_ending, ".rds"))
  saveRDS(tbl_summary_vtu, file = str_c("exploration-R/data/choice-model-vtu-summary-", path_ending, ".rds"))
  save_my_pdf_and_tiff(pl_caterpillar, str_c("exploration-R/data/caterpillar-2-params-vtu-", path_ending), 6, 4)
}



# v + ru vs. intercept + v + ru -------------------------------------------



mm_choice_v <- model.matrix(
  C ~ V + RU, data = tbl_subset
) %>% as_tibble()
colnames(mm_choice_v) <- c("ic", "V", "RU")
mm_choice_v$sub <- tbl_subset$sub

mm_choice_v_predict <- model.matrix(
  C ~ V + RU, data = tbl_predict
) %>% as_tibble()
colnames(mm_choice_v_predict) <- c("ic", "V", "RU")
mm_choice_v_predict$sub <- tbl_predict$sub


l_data <- list(
  n_data = nrow(mm_choice_v),
  n_subj = length(unique(mm_choice_v$sub)),
  choice = tbl_subset$C,
  subj = as.numeric(factor(
    tbl_subset$sub, 
    labels = 1:length(unique(tbl_subset$sub))
  )),
  n_data_predict = nrow(mm_choice_v_predict),
  choice_predict = tbl_predict$C,
  subj_predict = as.numeric(factor(
    tbl_predict$sub, 
    labels = 1:length(unique(tbl_predict$sub))
  ))
)

for (ic in with_intercept) {
  path_ending <- c("no-intercept", "intercept")[ic + 1]
  # select model
  if (ic){
    l_data$x <- as.matrix(mm_choice_v[, c("ic", "V", "RU")])
    l_data$x_predict <- as.matrix(mm_choice_v_predict[, c("ic", "V", "RU")])
    choice_model_ic <- stan_choice_reduced_intercept()
    mod_choice <- cmdstan_model(choice_model_ic)
  } else if (!ic) {
    l_data$x_predict <- as.matrix(mm_choice_v_predict[, c("V", "RU")])
    l_data$x <- as.matrix(mm_choice_v[, c("V", "RU")])
    choice_model_no_ic <- stan_choice_reduced_no_intercept()
    mod_choice <- cmdstan_model(choice_model_no_ic)
  }
  
  # run model
  fit_choice_v <- mod_choice$sample(
    data = l_data, iter_sampling = n_samples, iter_warmup = n_warmup, chains = n_chains,
    init = 1, parallel_chains = n_chains
  )
  # save mcmc samples and summary stats
  tbl_summary_v <- fit_choice_v$summary(variables = pars_interest)
  if(max(tbl_summary_v$rhat) > 1.02){stop(str_c("Rhat values too large\ncloser inspect V ", path_ending, " model"))}
  
  tbl_draws_v <- fit_choice_v$draws(variables = pars_interest, format = "df")
  if (ic){
    pl_caterpillar <- bayesplot::mcmc_trace(tbl_draws_v,  pars = c("mu_tf[1]", "mu_tf[2]", "mu_tf[3]"), n_warmup = n_warmup) + ggtitle(str_c("V & ", path_ending))
  } else if (!ic) {
    pl_caterpillar <- bayesplot::mcmc_trace(tbl_draws_v,  pars = c("mu_tf[1]", "mu_tf[2]"), n_warmup = n_warmup) + ggtitle(str_c("V & ", path_ending))
  }
  loo <- fit_choice_v$loo(variables = "log_lik_pred")
  
  saveRDS(loo, file = str_c("exploration-R/data/loo-v-", path_ending, ".rds"))
  saveRDS(tbl_draws_v, file = str_c("exploration-R/data/choice-model-v-", path_ending, ".rds"))
  saveRDS(tbl_summary_v, file = str_c("exploration-R/data/choice-model-v-summary-", path_ending, ".rds"))
  save_my_pdf_and_tiff(pl_caterpillar, str_c("exploration-R/data/caterpillar-2-params-v-", path_ending), 6, 4)
  
}




# Model Comparison --------------------------------------------------------



loo_vtu_no_intercept <- readRDS("exploration-R/data/loo-vtu-no-intercept.rds")
loo_vtu_intercept <- readRDS("exploration-R/data/loo-vtu-intercept.rds")
loo_v_no_intercept <- readRDS("exploration-R/data/loo-v-no-intercept.rds")
loo_v_intercept <- readRDS("exploration-R/data/loo-v-intercept.rds")

# all four
loo::loo_model_weights(list(
  loo_vtu_no_intercept, loo_vtu_intercept, loo_v_no_intercept, loo_v_intercept
), method = "stacking")
# only vtu
loo::loo_model_weights(list(
  loo_vtu_no_intercept, loo_vtu_intercept
), method = "stacking")
# only v
loo::loo_model_weights(list(
  loo_v_no_intercept, loo_v_intercept
), method = "stacking")
# best from the two variable combinations
loo::loo_model_weights(list(
  loo_vtu_no_intercept, loo_v_intercept
), method = "stacking")





