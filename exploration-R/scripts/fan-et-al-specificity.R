
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





# Frequentist Models in LME4 ----------------------------------------------


m_ri <- glmer(
  C ~ 1 + (1 | sub), # VTU + VTU:Factor1_Somatic_Anxiety + 
  data = tbl_subset, 
  family = binomial(link = "logit")
)
summary(m_ri)

# full model with or without intercept does not converge

m_no_vtu <- glmer(
  C ~ 1 + V + RU + (1 + V + RU | sub), # VTU + VTU:Factor1_Somatic_Anxiety + 
  data = tbl_subset, 
  family = binomial(link = "logit")
)
summary(m_no_vtu)

m_only_vtu <- glmer(
  C ~ 1 + VTU + (1 + VTU | sub), # VTU + VTU:Factor1_Somatic_Anxiety + 
  data = tbl_subset, 
  family = binomial(link = "logit")
)
summary(m_only_vtu)
tbl_subset$preds_prob_vtu <- predict(m_only_vtu, type = "response")
tbl_subset$preds_vtu <- as.numeric(rbernoulli(nrow(tbl_subset), tbl_subset$preds_prob_vtu))

m_v_vtu <- glmer(
  preds_vtu ~ 1 + V + VTU + (1 + V + VTU | sub), # VTU + VTU:Factor1_Somatic_Anxiety + 
  data = tbl_subset, 
  family = binomial(link = "logit")
)
summary(m_v_vtu)


# predict keeping vtu constant, but see whether you can "recover" it
m_subset_v <- glmer(
  C ~ V + V:Factor1_Somatic_Anxiety + (1 + V | sub), 
  data = tbl_subset, 
  family = binomial(link = "logit")
)

tbl_subset$preds_prob_v <- predict(m_subset_v, type = "response")
tbl_subset$preds_v <- as.numeric(rbernoulli(nrow(tbl_subset), tbl_subset$preds_prob_v))


m_subset_check_v <- glmer(
  preds_v ~ VTU + VTU:Factor1_Somatic_Anxiety + V + V:Factor1_Somatic_Anxiety + (1 + VTU | sub), 
  data = tbl_subset, 
  family = binomial(link = "logit")
)

summary(m_subset_check_v)


# predict keeping v constant, but see whether you can "recover" it
m_subset_vtu <- glmer(
  C ~ -1 + VTU + (-1+ VTU | sub), # VTU:Factor1_Somatic_Anxiety + 
  data = tbl_subset, 
  family = binomial(link = "logit")
)
summary(m_subset_vtu)

tbl_subset$preds_prob_vtu <- predict(m_subset_vtu, type = "response")
tbl_subset$preds_vtu <- as.numeric(rbernoulli(nrow(tbl_subset), tbl_subset$preds_prob_vtu))


m_subset_check_vtu <- glmer(
  preds_vtu ~ VTU + VTU:Factor1_Somatic_Anxiety + V + V:Factor1_Somatic_Anxiety + (1 + VTU | sub), 
  data = tbl_subset, 
  family = binomial(link = "logit")
)

summary(m_subset_check_vtu)


m_100 <- glmer(
  C ~ V + RU*trial + VTU + (1 + V + RU*trial + VTU | sub), 
  data = tbl_e1 %>% filter(sub <= 100) %>% mutate(trial = scale(trial)[, 1]), 
  family = binomial(link = "probit")
)
summary(m_100)

# takes a while, but seems to converge without random intercept
m_1000 <- glmer(C ~ -1 + V + RU + VTU + (-1 + V + RU + VTU | sub), data = tbl_e1, family = binomial(link = "probit"))
summary(m_1000)

# omitting random intercept is not a good idea:
grouped_agg(tbl_e1, sub, C) %>%
  ggplot(aes(mean_C)) +
  geom_histogram() + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "") +
  theme(strip.background = element_rect(fill = "white"))



tbl_e1 <- tbl_e1 %>%
  mutate(
    V_cut = cut(V, 10),
    RU_cut = cut(RU, 10),
    VTU_cut = cut(VTU, 10)
  )


plot_univarite_relationship <- function(ivar, trial_id_max) {
  tbl_e1 %>%
    filter(trial <= trial_id_max) %>%
    group_by({{ivar}}) %>%
    summarize(C = mean(C), N = n()) %>%
    ungroup() %>%
    pivot_longer(cols = {{ivar}}) %>%
    ggplot(aes(value, C, group = 1)) +
    geom_line() +
    geom_point(size = 4, color = "white") +
    geom_point(aes(size = N)) +
    facet_wrap(~ name) + 
    theme_bw() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "", y = "") +
    theme(
      strip.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    coord_cartesian(ylim = c(0, 1))
}
pl_V_C2 <- plot_univarite_relationship(V_cut, 2)
pl_RU_C2 <- plot_univarite_relationship(RU_cut, 2)  
pl_VTU_C2 <- plot_univarite_relationship(VTU_cut, 2)
pl_horizon2 <- arrangeGrob(pl_V_C2, pl_RU_C2, pl_VTU_C2, nrow = 1)

pl_V_C4 <- plot_univarite_relationship(V_cut, 4)
pl_RU_C4 <- plot_univarite_relationship(RU_cut, 4)  
pl_VTU_C4 <- plot_univarite_relationship(VTU_cut, 4)
pl_horizon4 <- arrangeGrob(pl_V_C4, pl_RU_C4, pl_VTU_C4, nrow = 1)

pl_V_C7 <- plot_univarite_relationship(V_cut, 7)
pl_RU_C7 <- plot_univarite_relationship(RU_cut, 7)  
pl_VTU_C7 <- plot_univarite_relationship(VTU_cut, 7)
pl_horizon7 <- arrangeGrob(pl_V_C7, pl_RU_C7, pl_VTU_C7, nrow = 1)

pl_V_C10 <- plot_univarite_relationship(V_cut, 10)
pl_RU_C10 <- plot_univarite_relationship(RU_cut, 10)  
pl_VTU_C10 <- plot_univarite_relationship(VTU_cut, 10)
pl_horizon10 <- arrangeGrob(pl_V_C10, pl_RU_C10, pl_VTU_C10, nrow = 1)

grid.draw(arrangeGrob(pl_horizon2, pl_horizon4, pl_horizon7, pl_horizon10, nrow = 4))




# Stan Models -------------------------------------------------------------

# this data set is used for backcasts
# backcasting on full data set is not feasible in terms of storage
fit_or_load <- "load"
set.seed(4321)
tbl_predict <- tbl_subset %>% group_by(sub) %>%
  mutate(
    randi = runif(length(block)),
    ranking = row_number(randi)
  ) %>%
  arrange(sub, ranking) %>%
  filter(ranking <= 20)




# Fit 2-Parameter Model including VTU -------------------------------------



choice_model_reduced <- stan_choice_reduced()
mod_choice_reduced <- cmdstan_model(choice_model_reduced)

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
  x = as.matrix(mm_choice_vtu[, c("ic", "VTU", "RU")]),
  n_data_predict = nrow(mm_choice_vtu_predict),
  subj_predict = as.numeric(factor(
    tbl_predict$sub, 
    labels = 1:length(unique(tbl_predict$sub))
  )),
  x_predict = as.matrix(mm_choice_vtu_predict[, c("ic", "VTU", "RU")])
)

n_warmup <- 2000
n_samples <- 5000
n_chains <- 3
if (fit_or_load == "fit") {
  fit_choice_vtu <- mod_choice_reduced$sample(
    data = l_data, iter_sampling = n_samples, iter_warmup = n_warmup, chains = n_chains,
    init = 0, parallel_chains = 3
  )
  # analyze group-level posterior parameters estimates
  pars_interest <- c("mu_tf", "posterior_prediction", "sigma_subject", "b")
  tbl_draws_vtu <- fit_choice_vtu$draws(variables = pars_interest, format = "df")
  tbl_summary_vtu <- fit_choice_vtu$summary(variables = pars_interest)
  saveRDS(tbl_draws_vtu, file = "exploration-R/data/choice-model-vtu-posteriors.rds")
  saveRDS(tbl_summary_vtu, file = "exploration-R/data/choice-model-vtu-summary.rds")
  
} else if (fit_or_load == "load") {
  fit_choice_vtu <- readRDS("exploration-R/data/fit_choice_vtu.rds")
  tbl_draws_vtu <- readRDS("exploration-R/data/choice-model-vtu-posteriors.rds")
  tbl_summary_vtu <- readRDS("exploration-R/data/choice-model-vtu-summary.rds")
}


bayesplot::mcmc_trace(tbl_draws_vtu,  pars = c("mu_tf[1]", "mu_tf[2]", "mu_tf[3]"), n_warmup = 2000)

tbl_posterior_vtu <- tbl_draws_vtu %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = c("Intercept", "VTU", "RU")))


params_bf <- c("Intercept", "VTU", "RU")
l <- sd_bfs(tbl_posterior_vtu, params_bf, sqrt(2)/4)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
map(as.list(params_bf), plot_posterior, tbl_posterior_vtu, tbl_thx, bfs)


tbl_hdis_vtu <- tbl_posterior_vtu %>%
  group_by(parameter) %>%
  arrange(value) %>%
  mutate(
    rwn = row_number(value),
    quant = rwn / max(rwn),
    mean_value = mean(value)
  ) %>%
  filter(between(quant, .025, .975)) %>%
  mutate(
    lo_thx = quantile(value, .025),
    hi_thx = quantile(value, .975)
  ) %>%
  filter(rwn == min(rwn)) %>%
  select(-c(rwn, value, quant))

pl_hdi_vtu <- ggplot(tbl_hdis_vtu, aes(parameter, mean_value, group = parameter)) +
  geom_segment(aes(
    y = lo_thx, yend = hi_thx, x = parameter, xend = parameter, color = parameter), 
    size = 2, lineend = "round"
  ) + 
  geom_point(color = "black", size = 3, shape = 1) +
  theme_bw() +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  labs(x = "Posterior Parameter Value", y = "", title = "Two-Parameter Model: 95% HDIs") +
  theme(strip.background = element_rect(fill = "white"), legend.position = "off") +
  scale_color_manual(values = c("#3b528b", "#21918c", "#fde725")) +
  coord_flip(ylim = c(-.5, 3.6))



tbl_preds_vtu <- tbl_draws_vtu %>%
  select(starts_with("posterior_prediction"))

# do backcasts align with data?
pred_prob <- apply(tbl_preds_vtu, 2, mean) %>% unname()
cor(pred_prob, tbl_predict2$C)

sample_ids <- sample(1:nrow(tbl_preds_vtu), replace = TRUE, size = ncol(tbl_preds_vtu))
tbl_predict$backcast_VTU <- unlist(map2(tbl_preds_vtu, sample_ids, ~ .x[.y]))

# looks ok
class_mat <- table(tbl_predict[, c("C", "backcast")]) / sum(tbl_predict[, c("C", "backcast")])
round(class_mat, 2) %>% DT::datatable()

loo_vtu <- fit_choice_vtu$loo(variables = "posterior_prediction")



# Fit 2-Parameter Model including V ---------------------------------------


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
  x = as.matrix(mm_choice_v[, c("ic", "V", "RU")]),
  n_data_predict = nrow(mm_choice_v_predict),
  subj_predict = as.numeric(factor(
    tbl_predict$sub, 
    labels = 1:length(unique(tbl_predict$sub))
  )),
  x_predict = as.matrix(mm_choice_v_predict[, c("ic", "V", "RU")])
)

n_warmup <- 2000
n_samples <- 5000
n_chains <- 3
if (fit_or_load == "fit") {
  fit_choice_v <- mod_choice_reduced$sample(
    data = l_data, iter_sampling = n_samples, iter_warmup = n_warmup, chains = n_chains,
    init = 0, parallel_chains = 3
  )
  # analyze group-level posterior parameters estimates
  pars_interest <- c("mu_tf", "posterior_prediction")
  tbl_draws_v <- fit_choice_v$draws(variables = pars_interest, format = "df")
  tbl_summary_v <- fit_choice_v$summary(variables = pars_interest)
  saveRDS(tbl_draws_v, file = "exploration-R/data/choice-model-v-posteriors.rds")
  saveRDS(tbl_summary_v, file = "exploration-R/data/choice-model-v-summary.rds")
  
} else if (fit_or_load == "load") {
  fit_choice_v <- readRDS("exploration-R/data/fit_choice_v.rds")
  tbl_draws_v <- readRDS("exploration-R/data/choice-model-v-posteriors.rds")
  tbl_summary_v <- readRDS("exploration-R/data/choice-model-v-summary.rds")
}


bayesplot::mcmc_trace(tbl_draws_v,  pars = c("mu_tf[1]", "mu_tf[2]", "mu_tf[3]"), n_warmup = 2000)

tbl_posterior_v <- tbl_draws_v %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = c("Intercept", "V", "RU")))


params_bf <- c("Intercept", "V", "RU")
l <- sd_bfs(tbl_posterior_v, params_bf, sqrt(2)/4)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
map(as.list(params_bf), plot_posterior, tbl_posterior_v, tbl_thx, bfs)


tbl_hdis_v <- tbl_posterior_v %>%
  group_by(parameter) %>%
  arrange(value) %>%
  mutate(
    rwn = row_number(value),
    quant = rwn / max(rwn),
    mean_value = mean(value)
  ) %>%
  filter(between(quant, .025, .975)) %>%
  mutate(
    lo_thx = quantile(value, .025),
    hi_thx = quantile(value, .975)
  ) %>%
  filter(rwn == min(rwn)) %>%
  select(-c(rwn, value, quant))

pl_hdi_v <- ggplot(tbl_hdis_v, aes(parameter, mean_value, group = parameter)) +
  geom_segment(aes(
    y = lo_thx, yend = hi_thx, x = parameter, xend = parameter, color = parameter), 
    size = 2, lineend = "round"
  ) + 
  geom_point(color = "black", size = 3, shape = 1) +
  theme_bw() +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  labs(x = "Posterior Parameter Value", y = "", title = "Two-Parameter Model: 95% HDIs") +
  theme(strip.background = element_rect(fill = "white"), legend.position = "off") +
  scale_color_manual(values = c("#3b528b", "#21918c", "#fde725")) +
  coord_flip(ylim = c(-.5, 3.6))



tbl_preds_v <- tbl_draws_v %>%
  select(starts_with("posterior_prediction"))

# do backcasts align with data?
pred_prob <- apply(tbl_preds_v, 2, mean) %>% unname()
cor(pred_prob, tbl_predict2$C)

sample_ids <- sample(1:nrow(tbl_preds_v), replace = TRUE, size = ncol(tbl_preds_v))
tbl_predict$backcast_V <- unlist(map2(tbl_preds_v, sample_ids, ~ .x[.y]))

# looks ok
class_mat <- table(tbl_predict[, c("C", "backcast")]) / sum(tbl_predict[, c("C", "backcast")])
round(class_mat, 2) %>% DT::datatable()






# Fit 3-Parameter Model on Backcasts from 2-Parameter Models ---------------



tbl_predict2 <- tbl_predict %>%
  mutate(
    V = scale(V)[, 1],
    RU = scale(RU)[, 1],
    VTU = scale(VTU)[, 1]
  )

choice_model <- stan_choice_lkj()
mod_choice <- cmdstan_model(choice_model)

mm_choice_3par <- model.matrix(
  backcast ~ V + RU + VTU, data = tbl_predict2
) %>% as_tibble()
colnames(mm_choice_3par) <- c("ic", "V", "RU", "VTU")
mm_choice_3par$sub <- tbl_predict2$sub

mm_choice_3par_predict <- model.matrix(
  backcast ~ V + RU + VTU, data = tbl_predict2
) %>% as_tibble()
colnames(mm_choice_3par_predict) <- c("ic", "V", "RU", "VTU")
mm_choice_3par_predict$sub <- tbl_predict2$sub


l_data <- list(
  n_data = nrow(mm_choice_3par),
  n_subj = length(unique(mm_choice_3par$sub)),
  subj = as.numeric(factor(
    tbl_predict2$sub, 
    labels = 1:length(unique(tbl_predict2$sub))
  )),
  x = as.matrix(mm_choice_3par[, c("ic", "V", "RU", "VTU")]),
  n_data_predict = nrow(mm_choice_3par_predict),
  subj_predict = as.numeric(factor(
    tbl_predict2$sub, 
    labels = 1:length(unique(tbl_predict2$sub))
  )),
  x_predict = as.matrix(mm_choice_3par_predict[, c("ic", "V", "RU", "VTU")])
)

fit_v_or_vtu_backcasts <- "VTU"
if (fit_v_or_vtu_backcasts == "VTU") {
  l_data$choice <- tbl_predict2$backcast_V
  path_ending <- "-backcast-VTU"
} else if (fit_v_or_vtu_backcasts == "V") {
  l_data$choice <- tbl_predict2$backcast_VTU
  path_ending <- "-backcast-V"
}



if (fit_or_load == "fit") {
  fit_choice <- mod_choice$sample(
    data = l_data, iter_sampling = 10000, iter_warmup = 4000, chains = 3,
    init = 0, parallel_chains = 3, thin = 1
  )
  # analyze group-level posterior parameters estimates
  pars_interest <- c("mu", "Sigma", "posterior_prediction", "sigma_subject", "b")
  tbl_draws_full <- fit_choice$draws(variables = pars_interest, format = "df")
  tbl_summary_full <- fit_choice$summary(variables = pars_interest)
  saveRDS(tbl_draws_full, file = str_c("exploration-R/data/choice-model-full-posteriors", path_ending, ".rds"))
  saveRDS(tbl_summary_full, file = str_c("exploration-R/data/choice-model-full-summary", path_ending, ".rds"))
  
} else if (fit_or_load == "load") {
  tbl_draws_full <- readRDS(str_c("exploration-R/data/choice-model-full-posteriors", path_ending, ".rds"))
  tbl_summary_full <- readRDS(str_c("exploration-R/data/choice-model-full-summary", path_ending, ".rds"))
}


bayesplot::mcmc_trace(tbl_draws_full,  pars = c("mu[1]", "mu[2]", "mu[3]", "mu[4]"), n_warmup = 0)


tbl_posterior_full <- tbl_draws_full %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = c("Intercept", "V", "RU", "VTU")))

params_bf <- c("Intercept", "V", "RU", "VTU")
l <- sd_bfs(tbl_posterior_full, params_bf, sqrt(2)/4)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
map(as.list(params_bf), plot_posterior, tbl_posterior_full, tbl_thx, bfs)


tbl_hdis_full <- tbl_posterior_full %>%
  group_by(parameter) %>%
  arrange(value) %>%
  mutate(
    rwn = row_number(value),
    quant = rwn / max(rwn),
    mean_value = mean(value)
  ) %>%
  filter(between(quant, .025, .975)) %>%
  mutate(
    lo_thx = quantile(value, .025),
    hi_thx = quantile(value, .975)
  ) %>%
  filter(rwn == min(rwn)) %>%
  select(-c(rwn, value, quant))

tbl_hdis_full$parameter <- fct_relevel(tbl_hdis_full$parameter, "V", after = 3)
tbl_hdis_full$parameter <- fct_relevel(tbl_hdis_full$parameter, "VTU", after = 1)

pl_hdi_full <- ggplot(tbl_hdis_full, aes(parameter, mean_value, group = parameter)) +
  geom_segment(aes(
    y = lo_thx, yend = hi_thx, x = parameter, xend = parameter, color = parameter), 
    size = 2, lineend = "round"
  ) + 
  geom_point(color = "black", size = 3, shape = 1) +
  theme_bw() +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  labs(x = "Posterior Parameter Value", y = "", title = "Three-Parameter Model: 95% HDIs") +
  theme(strip.background = element_rect(fill = "white"), legend.position = "off") +
  scale_color_manual(values = c("#3b528b", "#21918c", "#fde725", "#5ec962")) +
  coord_flip(ylim = c(-.5, 3.6))

grid.draw(arrangeGrob(pl_hdi_reduced, pl_hdi_full, nrow = 2))

# inspect the parameter correlations
tbl_cor_posterior <- tbl_draws_full %>% 
  dplyr::select(c(`Sigma[2,3]`, `Sigma[2,4]`, `Sigma[3,4]`, .chain)) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("Sigma")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = c("r(V, RU)", "r(V, VTU)", "r(RU, VTU)")))

ggplot(tbl_cor_posterior, aes(parameter, value, group = parameter)) +
  geom_hline(yintercept = c(-1, 1), linetype = "dotdash", color = "grey") +
  geom_hline(yintercept = 0, color = "forestgreen", linetype = "dotdash") +
  geom_violin(aes(fill = parameter), alpha = .25) +
  geom_quasirandom(aes(color = parameter), cex = 1.75, alpha = .5, method = "quasirandom") +
  geom_boxplot(width = .25, aes(color = parameter), alpha = .7) +
  stat_summary(geom = "point", fun = "mean", color = "black", size = 3, shape = 23) +
  theme_bw() +
  scale_x_discrete(expand = c(0.03, 0.02)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  labs(x = "", y = "Correlation") + 
  theme(strip.background = element_rect(fill = "white"), legend.position = "off")



tbl_preds_full <- tbl_draws_full %>%
  select(starts_with("posterior_prediction"))

# do backcasts align with data?
pred_prob <- apply(tbl_preds_full, 2, mean) %>% unname()
cor(pred_prob, tbl_predict2$C)


sample_ids <- sample(1:nrow(tbl_preds_full), replace = TRUE, size = ncol(tbl_preds_full))
tbl_predict2$backcast <- unlist(map2(tbl_preds_full, sample_ids, ~ .x[.y]))


# looks ok
class_mat_3 <- table(tbl_predict2[, c("C", "backcast")]) / sum(tbl_predict2[, c("C", "backcast")])
round(class_mat_3, 2) %>% DT::datatable()


loo_full <- fit_choice$loo(variables = "posterior_prediction")
loo::loo_model_weights(list(loo_reduced, loo_full), method = "stacking")

