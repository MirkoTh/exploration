################# model recovery horizon task ###############################


setwd("/Users/kwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/ZallerEtAl")

library(plyr)
library(brms)
library(tidyverse)

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

#data <- read.csv("~/reliability/data.csv")
data <- read.csv("data.csv")
data$Horizon <- factor(data$Horizon, levels = data$Horizon, labels = data$Horizon)

set.seed(123)

################################## add the learning model results to the df #####################

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

############### Bayesian integration



bayIncrAtOnce <- function(x){
  left <- data$Outcome[data$Subject == data$Subject[x]&
                         data$Block == data$Block[x] &
                         data$Choice == 1& 
                         data$Trial < 5]
  right <- data$Outcome[data$Subject == data$Subject[x]&
                          data$Block == data$Block[x] &
                          data$Choice == 0& 
                          data$Trial < 5]
  
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

data$bayMeanL <- NA
data$bayMeanR <- NA
data$bayVarL <- NA
data$bayVarR <- NA

for (i in data$row[data$Trial == 5]){
  data[data$row == i, grep("bay", colnames(data))] <- bayIncrAtOnce(i)
}


####################### fit the models & simulate data & get GoF #####################
data$RU <- sqrt(data$bayVarL) - sqrt(data$bayVarR)
data$V <- data$bayMeanL - data$bayMeanR
data$VTU <- data$V/(sqrt(data$bayVarL^2 + data$bayVarR^2))

data$RU <- scale(data$RU)
data$V <- scale(data$V)
data$VTU <- scale(data$VTU)


## for improved comparability I will now also use a simple logistic regression separately for each subject
models = list(model = c("Wilson", "Sam", "UCB", "Thompson", "Softmax"),
                    eq = c(as.formula("Choice ~ delta_mean*Horizon + Info*Horizon"), 
                           as.formula("Choice ~ V*Horizon + RU*Horizon + VTU *Horizon"),
                           as.formula("Choice ~ V*Horizon + RU*Horizon"),
                           as.formula("Choice ~ VTU *Horizon"),
                           as.formula("Choice ~ V*Horizon")))

AICs <- data.frame(ID = rep(unique(data$Subject), length(models$model)^2),
                  AIC = rep(NA, length(unique(data$Subject))*length(models$model)^2),
                  GenModel = rep(models$model, each = length(unique(data$Subject))*length(models$model)),
                  FitModel = rep(rep(models$model, each = length(unique(data$Subject))), length(models$model)),
                  converged = NA,
                  best = 0)



for (i in unique(data$Subject)){
  # simulate data
  for(genModel in models$model){
    formula <- models$eq[models$model == genModel][[1]]
    trueModel <- glm(formula,
                     data = data[data$Trial ==5 & data$Subject == i, ],
                     family = binomial(link = "probit"))
    
    
    # simulate data
    simdat <- subset(data, Trial == 5 & Subject == i, -Choice)
    simdat$Choice <- predict(trueModel, type = "response")
    simdat$Choice <- ifelse(simdat$Choice < runif(nrow(simdat)), 0, 1)
    
    # fit models
    for(fitModel in models$model){
      formula <- models$eq[models$model == fitModel][[1]]
      simModel <- glm(formula,
                      data = simdat,
                      family = binomial(link = "probit"))
      
      
      # get AIC
      AICs$AIC[AICs$ID == i & AICs$GenModel == genModel & AICs$FitModel == fitModel] <- simModel$aic
      AICs$converged[AICs$ID == i & AICs$GenModel == genModel & AICs$FitModel == fitModel] <- simModel$converged

      
    }
    
    # indicate which one is best for that subject and that simulated dataset
    best = min(AICs$AIC[AICs$ID == i & AICs$GenModel == genModel])
    AICs$best[AICs$ID == i & AICs$GenModel == genModel & AICs$AIC == best] <- 1
    
    
  }
  
 
  
}


# how many converged?
table(AICs$converged) # 196 failed to converge


# get proportion of subjects for which a given model was best given which model the data was simulated from
bestModels <- ddply(AICs, ~GenModel+FitModel, summarise, Pbest = mean(best))


# plot results

library(ggplot2)

ggplot(bestModels, aes(x = GenModel, y = FitModel, fill = Pbest)) + geom_raster() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(Pbest, digits = 2)))

cors$cor[cors$true == cors$recovered]



