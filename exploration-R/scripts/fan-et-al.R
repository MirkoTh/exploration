library(tidyverse)
library(lme4)
library(rutils)
library(grid)
library(gridExtra)

# difference scores are always option 1 - option 2
tbl_e1_prep <- read_csv("exploration-R/data/fan-exp1_bandit_task_scale.csv")
tbl_e1 <- tbl_e1_prep %>% select(sub, block, trial, C, V, RU, TU, V_old, RU_old, TU_old)
tbl_e1$VTU_old <- tbl_e1$V_old / tbl_e1$TU_old
tbl_e1$VTU <- scale(tbl_e1$VTU_old)[, 1]

cor(tbl_e1 %>% select(V, RU, VTU))

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



