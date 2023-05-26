################ checking for correlations over trials in both tasks ############################

setwd("/Users/kwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities")
library(ggplot2)
theme_set(theme_classic(base_size = 15))
library(lme4)
#library(plyr)
library(brms)
#library(tidyr)
library(tidyverse)

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

#dataSam <- read.csv("FanEtal/exp1_bandit_task_scale.csv")
dataSam <- read_csv("exploration-R/data/fan-exp1_bandit_task_scale.csv")


dataHoriz <- read.csv("ZallerEtal/data.csv")
dataHoriz$Horizon <- factor(dataHoriz$Horizon, levels = dataHoriz$Horizon, labels = dataHoriz$Horizon)

set.seed(123)


##################### Horizon task ###################################

##### add learning model stuff to data frame ###################

### observed means (altered for the purpose of looking beyond first free choice!!!!!!!)

dataHoriz$mean_L <- NA
dataHoriz$mean_R <- NA

dataHoriz$row <- 1:nrow(dataHoriz)
dataHoriz$mean_L[dataHoriz$Trial >4] <- apply(as.array(dataHoriz$row[dataHoriz$Trial > 4]), 1, function(x) meann(dataHoriz$Outcome[dataHoriz$Subject == dataHoriz$Subject[x]&
                                                                                                               dataHoriz$Block == dataHoriz$Block[x] &
                                                                                                               dataHoriz$Choice == 1 & 
                                                                                                               dataHoriz$Trial < dataHoriz$Trial[x]]))
dataHoriz$mean_R[dataHoriz$Trial > 4] <- apply(as.array(dataHoriz$row[dataHoriz$Trial > 4]), 1, function(x) meann(dataHoriz$Outcome[dataHoriz$Subject == dataHoriz$Subject[x]&
                                                                                                               dataHoriz$Block == dataHoriz$Block[x] &
                                                                                                               dataHoriz$Choice == 0& 
                                                                                                               dataHoriz$Trial < dataHoriz$Trial[x]]))
## calculate deltas
dataHoriz$delta_mean <- dataHoriz$mean_L - dataHoriz$mean_R


# add info variables that take observations from choices into account

dataHoriz$InfoL[dataHoriz$Trial > 4] <- apply(as.array(dataHoriz$row[dataHoriz$Trial > 4]), 1, function(x) length(dataHoriz$Outcome[dataHoriz$Subject == dataHoriz$Subject[x]&
                                                                                                                dataHoriz$Block == dataHoriz$Block[x] &
                                                                                                                dataHoriz$Choice == 1 & 
                                                                                                                dataHoriz$Trial < dataHoriz$Trial[x]]))

dataHoriz$InfoR[dataHoriz$Trial > 4] <- apply(as.array(dataHoriz$row[dataHoriz$Trial > 4]), 1, function(x) length(dataHoriz$Outcome[dataHoriz$Subject == dataHoriz$Subject[x]&
                                                                                                                                      dataHoriz$Block == dataHoriz$Block[x] &
                                                                                                                                      dataHoriz$Choice == 0& 
                                                                                                                                      dataHoriz$Trial < dataHoriz$Trial[x]]))
dataHoriz$delta_info <-  dataHoriz$InfoL - dataHoriz$InfoR

############### Bayesian integration (altered for the purpose of looking beyond first free choice!!!!!!!)

bayIncrAtOnce <- function(x){
  left <- dataHoriz$Outcome[dataHoriz$Subject == dataHoriz$Subject[x]&
                         dataHoriz$Block == dataHoriz$Block[x] &
                         dataHoriz$Choice == 1& 
                         dataHoriz$Trial < dataHoriz$Trial[x]]
  right <- dataHoriz$Outcome[dataHoriz$Subject == dataHoriz$Subject[x]&
                          dataHoriz$Block == dataHoriz$Block[x] &
                          dataHoriz$Choice == 0& 
                          dataHoriz$Trial < dataHoriz$Trial[x]]
  
  # initialise prior
  muLeft <- 50
  muRight <- 50
  alphaLeft <- 0.001 # where did this come from?
  alphaRight <- 0.001
  betaLeft <- 200
  betaRight <- 200
  nzero <-  0.01 # weighting of prior vs posterior, if same number as number of observations -> equal
  
  # updating left
  # taken from section 3 of: https://people.eecs.berkeley.edu/~jordan/courses/260-spring10/lectures/lecture5.pdf
  
  alphaLeft <- alphaLeft+ length(left)/2 # increased precision bc more info
  betaLeft <- betaLeft + 0.5*ifelse(length(left)>1, var(left),0) + (length(left)*nzero/2*(length(left)+nzero)) *  (mean(left) - muLeft)^2 # decreased precision bc info has variance
  tauLeft <- length(left) * qgamma(.5, alphaLeft, betaLeft) + nzero * qgamma(.5, alphaLeft, betaLeft)
  muLeft <- ((length(left)*tauLeft)/((length(left)*tauLeft) + nzero*tauLeft)) * mean(left) + ((nzero * tauLeft) / ((length(left)*tauLeft) + nzero* tauLeft)) * muLeft
  
  # updating right
  alphaRight <- alphaRight+ length(right)/2
  betaRight <- betaRight + 0.5*ifelse(length(right)>1, var(right),0) + (length(right)*nzero/2*(length(right)+nzero)) *  (mean(right) - muRight)^2
  tauRight <- length(right) * qgamma(.5, alphaRight, betaRight) + nzero * qgamma(.5, alphaRight, betaRight)
  muRight <- ((length(right)*tauRight)/((length(right)*tauRight) + nzero*tauRight)) * mean(right) + ((nzero * tauRight) / ((length(right)*tauRight) + nzero* tauRight)) * muRight
  
  
  
  return(c(muLeft,muRight,sqrt(1/tauLeft), sqrt(1/tauRight)))
}

dataHoriz$bayMeanL <- NA
dataHoriz$bayMeanR <- NA
dataHoriz$bayVarL <- NA
dataHoriz$bayVarR <- NA

for (i in dataHoriz$row[dataHoriz$Trial > 4]){
  dataHoriz[dataHoriz$row == i, grep("bay", colnames(dataHoriz))] <- bayIncrAtOnce(i)
}



############## get correlations ###################
dataHoriz$V <- dataHoriz$bayMeanL - dataHoriz$bayMeanR
dataHoriz$RU <- dataHoriz$bayVarL - dataHoriz$bayVarR
dataHoriz$VTU <- dataHoriz$V/(sqrt(dataHoriz$bayVarL^2 + dataHoriz$bayVarR^2))

cors <- ddply(dataHoriz[dataHoriz$Trial > 4, ], ~Trial+Horizon, summarise, wilson = cor(delta_mean, delta_info), VRU = cor(V, RU), VTU = cor(V, VTU))

### plot it
library(tidyr)

my_data_long <- pivot_longer(cors, 
                             cols = c(3,4,5), 
                             names_to = "parameters", 
                             values_to = "correlations")

ggplot(my_data_long, aes(Trial, correlations, color = parameters)) + geom_line(aes(linetype = Horizon)) + geom_point()+
  ggtitle("Horizon task")



######################### Sam's task ############################

## already have all the learning model stuff in the dataframe

cors <- plyr::ddply(dataSam, ~trial, summarise, 'Soft vs. RU' = cor(V, RU), 'V vs. VTU' = cor(V,VTU))
# , 'V vs. TU' = cor(V, TU), 'VTU vs. TU' = cor(VTU, TU)

my_data_long <- pivot_longer(cors, 
                             cols = c(2,3), 
                             names_to = "parameters", 
                             values_to = "correlations")
pl_fan <- ggplot(my_data_long %>% filter(trial >= 2), aes(trial, correlations, color = fct_rev(parameters))) + 
  geom_hline(yintercept = c(1, -1), color = "grey", linetype = "dotdash") +
  geom_hline(yintercept = 0, color = "forestgreen", linetype = "dotdash") +
  geom_line(aes(color = fct_rev(parameters))) +
  geom_point(size = 3, color = "white") +
  geom_point(aes(color = fct_rev(parameters))) +
  geom_label(aes(y = correlations - .1, x = trial + .1, label = round(correlations, 2))) +
  ggtitle("Fan et al. (2023)") + 
  theme_bw() +
  scale_color_brewer(palette = "Set1", name = "Variables") + 
  theme_bw() +
  scale_x_continuous(expand = c(0.1, 0.1), breaks = seq(2, 10, by = 2)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  labs(x = "Trial", y = "r") +
  theme(strip.background = element_rect(fill = "white"))

tbl_cors <- read_rds("exploration-R/data/gershman-2018-variable-correlations.Rds")
levels(tbl_cors$name) <- c("V vs. VTU", "V vs. RU")
pl_gershman <- ggplot(tbl_cors, aes(trial_id, value, group = name)) +
  geom_hline(yintercept = c(1, -1), color = "grey", linetype = "dotdash") +
  geom_hline(yintercept = 0, color = "forestgreen", linetype = "dotdash") +
  geom_line(aes(color = name)) +
  geom_point(size = 3, color = "white") +
  geom_point(aes(color = name)) +
  geom_label(aes(y = value - .1, x = trial_id + .1, label = round(value, 2), color = name)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.1, 0.1), breaks = seq(2, 10, by = 2)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  labs(x = "Trial", y = "r", title = "Gershman (2018)") +
  theme(strip.background = element_rect(fill = "white")) +
  scale_color_brewer(palette = "Set1", name = "Variables", guide = "none")

grid.draw(arrangeGrob(pl_gershman, pl_fan, nrow = 1, widths = c(.8, 1)))


