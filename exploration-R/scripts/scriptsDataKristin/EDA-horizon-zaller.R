library(tidyverse)


se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

#data <- read.csv("~/reliability/data.csv")
data <- read.csv("exploration-R/data/data.csv")
data$Horizon <- factor(data$Horizon, levels = data$Horizon, labels = data$Horizon)

# task_id <-  as.numeric(commandArgs(TRUE)[1])
Ngames = 20
NSub = 99
# horizon = as.numeric(commandArgs(TRUE)[4])
# 
# load(paste("~/reliability/baymodel", horizon, ".Rda", sep = ""))

set.seed(123)


############ What models do we want to use? ###########

## learning models:
# observed means
# bayesian integration of observations

## choice models:
# Wilson model with learning as in Wilson paper
# probit: 
# thompson
# UCB
# softmax

####### directly add the output of the learning models to the dataframe to simplify the fitting ##############

### observed means

data$mean_L <- NA
data$mean_R <- NA

data$row <- 1:nrow(data)
data$mean_L[data$Trial == 5] <- apply(as.array(data$row[data$Trial == 5]), 1, function(x) meann(data$Outcome[data$Subject == data$Subject[x]&
                                                                                                               data$Block == data$Block[x] &
                                                                                                               data$Choice == 1 & 
                                                                                                               data$Trial < 5]))
data$mean_R[data$Trial == 5] <- apply(as.array(data$row[data$Trial == 5]), 1, function(x) meann(data$Outcome[data$Subject == data$Subject[x]&
                                                                                                               data$Block == data$Block[x] &
                                                                                                               data$Choice == 0& 
                                                                                                               data$Trial < 5]))
## calculate deltas
data$delta_mean <- data$mean_L - data$mean_R


tbl_horizon <- as_tibble(data %>% filter(Trial == 5))
tbl_horizon$delta_mean_z <- scale(tbl_horizon$delta_mean)[, 1]
tbl_horizon$Info_simple <- tbl_horizon$Info * .5


tbl_horizon_uneq <- tbl_horizon %>% filter(Info %in% c(-1, 1))
tbl_horizon_eq <- tbl_horizon %>% filter(Info == 0)

tbl_horizon_uneq$delta_mean_z <- scale(tbl_horizon_uneq$delta_mean)[, 1]
tbl_horizon_uneq$Info_simple <- tbl_horizon_uneq$Info * .5
tbl_horizon_eq$delta_mean_z <- scale(tbl_horizon_eq$delta_mean)[, 1]
tbl_horizon_eq$Info_simple <- tbl_horizon_eq$Info * .5


m_uneq <- glmer(
  Choice ~ Horizon + delta_mean_z + (-1 + delta_mean_z | Subject), # VTU + VTU:Factor1_Somatic_Anxiety + 
  data = tbl_horizon_uneq, 
  family = binomial(link = "logit")
)
summary(m_uneq)


m_eq <- glmer(
  Choice ~ Horizon + delta_mean_z + (-1 + delta_mean_z | Subject), # VTU + VTU:Factor1_Somatic_Anxiety + 
  data = tbl_horizon_eq, 
  family = binomial(link = "logit")
)
summary(m_eq)

m_all <- glmer(
  Choice ~ Info_simple*delta_mean_z*Horizon + (-1 + Info_simple + delta_mean_z + Horizon | Subject), # VTU + VTU:Factor1_Somatic_Anxiety + 
  data = tbl_horizon, 
  family = binomial(link = "logit")
)
summary(m_all)

tbl_horizon$Choice[tbl_horizon$Info == 1] <- -1 * (tbl_horizon$Choice[tbl_horizon$Info == 1] - 1)
tbl_horizon$delta_mean[tbl_horizon$Info == 1] <- tbl_horizon$delta_mean[tbl_horizon$Info == 1] * (- 1)
tbl_horizon$Info[tbl_horizon$Info %in% c(-1, 1)] <- 1
tbl_horizon$delta_mean_cut <- cut(tbl_horizon$delta_mean, c(-62, -30, -20, -10, -3, 3, 10, 20, 30, 60))


tbl_horizon %>% group_by(Horizon, Info, delta_mean_cut) %>%
  summarise(c_mean = mean(Choice), n = n()) %>%
  mutate(
    Info = factor(Info),
    Info = fct_relabel(Info, ~ c("Equal Information [2 2]", "Unequal Information [3 1]"))
    ) %>%
  ggplot(aes(delta_mean_cut, c_mean, group = Horizon)) +
  geom_hline(yintercept = .5, color = "grey", linetype = "dotdash") +
  geom_line(aes(color = Horizon)) +
  geom_point(color = "white", size = 5) +
  geom_point(aes(size = n, color = Horizon)) +
  facet_wrap(~ Info) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  scale_x_discrete(expand = c(0, 0.2)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_size_area(max_size = 4.5) + scale_size_continuous(breaks =  c(50, 400), name = "Nr. Data Points") +
  scale_color_viridis_d() +
  labs(x = "Delta Mean", y = "Prop. More Informative") +
  theme(strip.background = element_rect(fill = "white")) +
  coord_cartesian(ylim = c(0, 1))
  

tbl_horizon %>% group_by(Horizon, delta_mean_cut) %>%
  summarise(c_mean = mean(Choice), n = n()) %>%
  ggplot(aes(delta_mean_cut, c_mean, group = Horizon)) +
  geom_line(aes(color = Horizon)) +
  geom_point(color = "white", size = 5) +
  geom_point(aes(size = n, color = Horizon)) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_size_area(max_size = 4.5) +
  labs(x = "Delta Mean", y = "Prop. More Informative") +
  theme(strip.background = element_rect(fill = "white"))



tbl_horizon %>% group_by(Horizon, Info) %>%
  summarise(c_mean = mean(Choice), n = n()) %>%
  ggplot(aes(Info, c_mean, group = Horizon)) +
  geom_line(aes(color = Horizon)) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = Horizon)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  scale_size_area(max_size = 4.5) +
  labs(x = "Information Condition", y = "Prop. More Informative") +
  theme(strip.background = element_rect(fill = "white"))




