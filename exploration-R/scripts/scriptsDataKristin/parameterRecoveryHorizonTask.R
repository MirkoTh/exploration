################ parameter recovery for the horizon task ###########################

setwd("/Users/kwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/ZallerEtAl")

library(plyr)
library(brms)
library(tidyverse)

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

#data <- read.csv("~/reliability/data.csv")
data <- read.csv("data.csv")
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

################################################################## bayesian integration

# values are between 1 and 100 so set prior mean to 50
# prior variance = 100 as in Kalman filter


bayInt <- function(x){
  left <- data$Outcome[data$Subject == data$Subject[x]&
                         data$Block == data$Block[x] &
                         data$Choice == 1& 
                         data$Trial < 5]
  right <- data$Outcome[data$Subject == data$Subject[x]&
                          data$Block == data$Block[x] &
                          data$Choice == 0& 
                          data$Trial < 5]
  
  # initialise prior
  mu0 <- 50
  sigma0 <- 100
  
  # mean and var of observations
  mu1left <- mean(left)
  mu1right <- mean(right)
  sigma1left <- ifelse(length(left) > 1, var(left), sigma0)
  sigma1right <- ifelse(length(right) > 1, var(right), sigma0)
  
  # get mean and variance of posterior
  muLeft <- (length(left)*sigma0*mu1left + sigma1left*mu0 )/(sigma0*length(left)+sigma1left) 
  sigmaLeft <- (sigma0*sigma1left)/(sigma0 * length(left)+sigma1left)

  muRight <- (length(right)*sigma0*mu1right + sigma1right*mu0 )/(sigma0*length(right)+sigma1right)
  sigmaRight <- (sigma0*sigma1right)/(sigma0 * length(right)+sigma1right)
  
  return(c(muLeft,muRight,sigmaLeft, sigmaRight))
  
}

bayIncr <- function(x){
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
  alphaLeft <- 0.1
  alphaRight <- 0.1
  betaLeft <- 0.1
  betaRight <- 0.1
  nzero <-  2 # weighting of prior vs posterior. 1 = equal, 2: prior = 2*posterior, etc
  
  # incremental updating left
  # taken from section 3 of: https://people.eecs.berkeley.edu/~jordan/courses/260-spring10/lectures/lecture5.pdf
  for (i in 1:length(left)){
    alphaLeft <- alphaLeft + 1/2
    print(alphaLeft)
    betaLeft <- betaLeft + nzero/(2*(1+nzero)) * (left[i] - muLeft)^2
    print(betaLeft)
    tauLeft <- qgamma(.5, alphaLeft, betaLeft)
    #hist(qgamma(seq(0,1,0.0001), alphaLeft, betaLeft))
    muLeft <- (tauLeft/(tauLeft + nzero*tauLeft)) * left[i] + ((nzero * tauLeft) / (tauLeft + nzero* tauLeft)) * muLeft
    tauLeft <- tauLeft + nzero * tauLeft
    print(tauLeft)
    muLeft <- qnorm(.5, muLeft, tauLeft) # sqrt bc need to transform it into variance
    hist(qnorm(seq(0,1,0.001), muLeft,tauLeft) )
  }
  
  # same for right
  for (i in 1:length(right)){
    alphaRight <- alphaRight + 1/2
    betaRight <- betaRight + nzero/(2*(1+nzero)) * (right[i] - muRight)^2
    tauRight <- qgamma(.5, alphaRight, betaRight)
    muRight <- (tauRight/(tauRight + nzero*tauRight)) * right[i] + ((nzero * tauRight) / (tauRight + nzero* tauRight)) * muRight
    tauRight <- tauRight + nzero * tauRight
    muRight <- qnorm(.5, muRight, sqrt(1/tauRight)) # sqrt bc need to transform it into variance
  }
  
  return(c(muLeft,muRight,sqrt(1/tauLeft), sqrt(1/tauRight)))
  
}


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

# did this work well?

ddply(data, ~Info, summarise, diff = meann(bayVarL)- meann(bayVarR)) # it incorporates the amount of info well
ddply(data, ~Info, summarise, left = meann(bayVarL), right = meann(bayVarR)) 

# does it incorporate the variance in the observations well?
data$VarL <- NA
data$VarR <- NA

Max <- max(c(data$bayVarL, data$bayVarR), na.rm = T) # 30.59

for (i in data$row[data$Trial == 5]){
  left <- data$Outcome[data$Subject == data$Subject[i]&
                         data$Block == data$Block[i] &
                         data$Choice == 1& 
                         data$Trial < 5]
  right <- data$Outcome[data$Subject == data$Subject[i]&
                          data$Block == data$Block[i] &
                          data$Choice == 0& 
                          data$Trial < 5]
  data[data$row == i, 21] <- ifelse(length(left) > 1, var(left), Max*2)
  data[data$row == i, 22] <- ifelse(length(right) > 1, var(right), Max*2)
}

cor(data$VarL[data$Info == 1], data$bayVarL[data$Info == 1], use = "pairwise.complete.obs") # only 0.089
cor(data$VarR, data$bayVarR, use = "pairwise.complete.obs")# 0.079


############## Kalman filter (based on Mirko)

update_kalman_filter <- function(var_prev, var_innov, var_error) {
  # var_innov: innovation variance -> basically drift rate in restless bandit
  # 
  kg <- (var_prev + var_innov) / (var_prev + var_innov + var_error)
  var_new <- (1 - kg) * (var_prev + var_innov)
  l_params_updated <- list(kg = kg, var_new = var_new)
  return(l_params_updated)
}

kalman_learning <- function(tbl_df, no, sigma_xi_sq, sigma_epsilon_sq) {
  #' Kalman filter without choice model given chosen options by participants
  #' 
  #' @description applies Kalman filter equations for a given bandit task with existing choices by participants
  #' @param tbl_df with made choices and collected rewards as columns
  #' @param no number of response options
  #' @param sigma_xi_sq innovation variance
  #' @param sigma_epsilon_sq error variance
  #' @return a tbl with by-trial posterior means and variances for all bandits
  rewards <- tbl_df$Outcome
  choices <- tbl_df$Choice
  m0 <- 50
  nt <- length(rewards) # number of time points
  m <- matrix(m0, ncol = no, nrow = nt + 1) # to hold the posterior means
  v <- matrix(sigma_epsilon_sq, ncol = no, nrow = nt + 1) # to hold the posterior variances
  
  for(t in 1:nt) {
    kt <- rep(0, no)
    # set the Kalman gain for the chosen option
    # actually 1= left, 0 = right so this is indexed the wrong way around but it is easier this way and I will just flip it after
    kt[choices[t]+1] <- (v[t,choices[t]+1] + sigma_xi_sq)/(v[t,choices[t]+1] + sigma_epsilon_sq + sigma_xi_sq)
    # compute the posterior means
    m[t+1,] <- m[t,] + kt*(rewards[t] - m[t,])
    # compute the posterior variances
    v[t+1,] <- (1-kt)*(v[t,])
  }
  tbl_m <- as.data.frame(m)
  # constrain v from becoming to small
  v <- t(apply(v, 1, function(x) pmax(x, .0001)))
  tbl_v <- as.data.frame(v)
  colnames(tbl_m) <- str_c("m_", 1:no)
  colnames(tbl_v) <- str_c("v_", 1:no)
  tbl_return <- tibble(cbind(tbl_m, tbl_v))
  
  return(tbl_return)
}


totalBlocks <- nrow(data[data$Trial == 1, ])
dat <- subset(data, Trial < 5)
dat$totalBlock <- rep(1:totalBlocks, each = 4)
da <- subset(data, Trial == 5)
da$totalBlock <- 1:totalBlocks

da$KML <- NA
da$KMR <- NA
da$KVL <- NA
da$KVR <- NA



for (i in 1:totalBlocks){
  posterior <- kalman_learning(subset(dat, totalBlock==i), 2, 0, 100)
  da[da$totalBlock == i, grep("K", colnames(da), ignore.case = F)] <- c(posterior$m_2[5], posterior$m_1[5], posterior$v_2[5], posterior$v_1[5])
  
}










############## recovery simple Wilson model ################

# fit the data
#(I did this before so just need to load it)
load("baymodel.Rda")

# simulate data
simdat <- subset(data, Trial == 5, -Choice)
simdat$Choice <- predict(baymodel)[ ,1]
simdat$Choice <- ifelse(simdat$Choice < runif(nrow(simdat)), 0, 1)

recovModel <- brm(Choice ~ delta_mean*Horizon + Info*Horizon + (Info*Horizon+ delta_mean*Horizon| Subject), family = "binomial", 
                  data = simdat,
                  chains = 2,
                  cores = 2,
                  iter = 10000)


# get posterior estimates from both models

trueParams <- as.data.frame(colMeans(as.data.frame(posterior_samples(baymodel))))
trueParams$predictor <- NA
trueParams$predictor[grepl("Info", rownames(trueParams))& grepl("r_Subject", rownames(trueParams))] <- "Info"
trueParams$predictor[grepl("delta_mean", rownames(trueParams))& grepl("r_Subject", rownames(trueParams))] <- "delta_mean"
trueParams$predictor[grepl("Horizon", rownames(trueParams))& grepl("r_Subject", rownames(trueParams))] <- "Horizon"
trueParams$predictor[grepl("Info:Horizon", rownames(trueParams))& grepl("r_Subject", rownames(trueParams))] <- "Info*Horizon"
trueParams$predictor[grepl("Horizon10:delta_mean", rownames(trueParams))& grepl("r_Subject", rownames(trueParams))] <- "delta_mean*Horizon"
trueParams <- subset(trueParams, !is.na(predictor)& !grepl("Subject__", rownames(trueParams)))


recoveredParams <- as.data.frame(colMeans(as.data.frame(posterior_samples(recovModel))))
recoveredParams$predictor <- NA
recoveredParams$predictor[grepl("Info", rownames(recoveredParams))& grepl("r_Subject", rownames(recoveredParams))] <- "Info"
recoveredParams$predictor[grepl("delta_mean", rownames(recoveredParams))& grepl("r_Subject", rownames(recoveredParams))] <- "delta_mean"
recoveredParams$predictor[grepl("Horizon", rownames(recoveredParams))& grepl("r_Subject", rownames(recoveredParams))] <- "Horizon"
recoveredParams$predictor[grepl("Info:Horizon", rownames(recoveredParams))& grepl("r_Subject", rownames(recoveredParams))] <- "Info*Horizon"
recoveredParams$predictor[grepl("Horizon10:delta_mean", rownames(recoveredParams))& grepl("r_Subject", rownames(recoveredParams))] <- "delta_mean*Horizon"
recoveredParams <- subset(recoveredParams, !is.na(predictor)& !grepl("Subject__", rownames(recoveredParams)))

# get correlations
cors <- data.frame(true = rep(c("Info", "delta_mean", "Horizon", "Info*Horizon", "delta_mean*Horizon"), 5),
                   recovered =  rep(c("Info", "delta_mean", "Horizon", "Info*Horizon", "delta_mean*Horizon"), each = 5),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams$`colMeans(as.data.frame(posterior_samples(baymodel)))`[trueParams$predictor == cors$true[x]],
                                                              recoveredParams$`colMeans(as.data.frame(posterior_samples(recovModel)))`[recoveredParams$predictor == cors$recovered[x]]))


# plot them

library(ggplot2)

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) 

cors$cor[cors$true == cors$recovered]



## check the correlations of predictors within true model

trueParams$ID <- substr(rownames(trueParams), 11, 30)
true <-  reshape2::dcast(trueParams, ID ~ predictor, value.var = "colMeans(as.data.frame(posterior_samples(baymodel)))")
cor(true[ ,2:6])

############### recovery bayes + Sam's model ################

# # create variables for RU and V/TU and diff in V
# 
data$RU <- sqrt(data$bayVarL) - sqrt(data$bayVarR)
data$V <- data$bayMeanL - data$bayMeanR
data$VTU <- data$V/(sqrt(data$bayVarL^2 + data$bayVarR^2))

data$RU <- scale(data$RU)
data$V <- scale(data$V)
data$VTU <- scale(data$VTU)
# 
# 
# cor(data[ ,23:25], use = "pairwise.complete.obs")
# 
# # fit model
# trueModelH5 <- brm(Choice ~ V + RU+ VTU + ( VTU + V + RU| Subject),
#                  data = data[data$Trial ==5 & data$Horizon == 5, ],
#                  family = binomial(link = "probit"),
#                  chains = 2,
#                  cores = 2,
#                  iter = 4000)
# # simulate data
# simdat <- subset(data, Trial == 5, -Choice)
# simdat$Choice <- predict(trueModel)[ ,1]
# simdat$Choice <- ifelse(simdat$Choice < runif(nrow(simdat)), 0, 1)
# 
# simModel <- brm(Choice ~ V + RU + VTU, 
#                 data = simdat,
#                 family = binomial(link = "probit"),
#                 chains = 2,
#                 cores = 2)




trueParams <- data.frame(ID = unique(data$Subject),
                         V = rep(NA, length(unique(data$Subject))),
                         RU = rep(NA, length(unique(data$Subject))),
                         VTU = rep(NA, length(unique(data$Subject))),
                         Horizon = rep(NA, length(unique(data$Subject))),
                         VH = rep(NA, length(unique(data$Subject))),
                         RUH =rep(NA, length(unique(data$Subject))),
                         VTUH = rep(NA, length(unique(data$Subject))),
                         converged = rep(NA, length(unique(data$Subject))))

simParams <- data.frame(ID = unique(data$Subject),
                        V = rep(NA, length(unique(data$Subject))),
                        RU = rep(NA, length(unique(data$Subject))),
                        VTU = rep(NA, length(unique(data$Subject))),
                        Horizon = rep(NA, length(unique(data$Subject))),
                        VH = rep(NA, length(unique(data$Subject))),
                        RUH =rep(NA, length(unique(data$Subject))),
                        VTUH = rep(NA, length(unique(data$Subject))),
                        converged = rep(NA, length(unique(data$Subject))))


for (i in unique(data$Subject)){
  
  trueModel5 <- glm(Choice ~ V*Horizon + RU*Horizon + VTU *Horizon,
                    data = data[data$Trial ==5 & data$Subject == i, ],
                    family = binomial(link = "probit"))
  # save coefficients
  trueParams$V[trueParams$ID == i] <- trueModel5$coefficients[2]
  trueParams$RU[trueParams$ID == i] <- trueModel5$coefficients[4]
  trueParams$Horizon[trueParams$ID == i] <- trueModel5$coefficients[3]
  trueParams$VTU[trueParams$ID == i] <- trueModel5$coefficients[5]
  trueParams$VH[trueParams$ID == i] <- trueModel5$coefficients[6]
  trueParams$RUH[trueParams$ID == i] <- trueModel5$coefficients[7]
  trueParams$VTUH[trueParams$ID == i] <- trueModel5$coefficients[8]
  trueParams$converged[trueParams$ID == i] <- trueModel5$converged
  
  # simulate data
  simdat <- subset(data, Trial == 5 & Subject == i, -Choice)
  simdat$Choice <- predict(trueModel5, type = "response")
  simdat$Choice <- ifelse(simdat$Choice < runif(nrow(simdat)), 0, 1)
  
  simModel <- glm(Choice ~ V*Horizon + RU*Horizon + VTU *Horizon,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$V[simParams$ID == i] <- simModel$coefficients[2]
  simParams$RU[simParams$ID == i] <- simModel$coefficients[4]
  simParams$Horizon[simParams$ID == i] <- simModel$coefficients[3]
  simParams$VTU[simParams$ID == i] <- simModel$coefficients[5]
  simParams$VH[simParams$ID == i] <- simModel$coefficients[6]
  simParams$RUH[simParams$ID == i] <- simModel$coefficients[7]
  simParams$VTUH[simParams$ID == i] <- simModel$coefficients[8]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}


# how many converged?
table(simParams$converged)
table(trueParams$converged)

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged) # 20 did not converge

trueParams$bothConverged <- simParams$bothConverged


# get correlations
cors <- data.frame(true = rep(c("RU", "V", "Horizon", "VTU", "RUH", "VH", "VTUH"), 7),
                   recovered =  rep(c("RU", "V", "Horizon", "VTU", "RUH", "VH", "VTUH"), each = 7),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                             simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))


# plot them

library(ggplot2)

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) 

cors$cor[cors$true == cors$recovered]




############## recovery bayes + UCB ###############


# fit model

# trueModelH5 <- brm(Choice ~ V + RU ,
#                    data = data[data$Trial ==5 & data$Horizon == 5 & data$Subject == "0028vx980suduypyca0fxp5s.", ],
#                    family = binomial(link = "probit"),
#                    chains = 2,
#                    cores = 2,
#                    iter = 4000)
# bayesian model does not converge no matter what

trueParams <- data.frame(ID = unique(data$Subject),
                         V = rep(NA, length(unique(data$Subject))),
                         RU = rep(NA, length(unique(data$Subject))),
                         Horizon = rep(NA, length(unique(data$Subject))),
                         VH = rep(NA, length(unique(data$Subject))),
                         RUH =rep(NA, length(unique(data$Subject))),
                         converged = rep(NA, length(unique(data$Subject))))

simParams <- data.frame(ID = unique(data$Subject),
                         V = rep(NA, length(unique(data$Subject))),
                         RU = rep(NA, length(unique(data$Subject))),
                         Horizon = rep(NA, length(unique(data$Subject))),
                         VH = rep(NA, length(unique(data$Subject))),
                         RUH =rep(NA, length(unique(data$Subject))),
                        converged = rep(NA, length(unique(data$Subject))))


for (i in unique(data$Subject)){

  trueModel5 <- glm(Choice ~ V*Horizon + RU*Horizon ,
                    data = data[data$Trial ==5 & data$Subject == i, ],
                    family = binomial(link = "probit"))
  # save coefficients
  trueParams$V[trueParams$ID == i] <- trueModel5$coefficients[2]
  trueParams$RU[trueParams$ID == i] <- trueModel5$coefficients[4]
  trueParams$Horizon[trueParams$ID == i] <- trueModel5$coefficients[3]
  trueParams$VH[trueParams$ID == i] <- trueModel5$coefficients[5]
  trueParams$RUH[trueParams$ID == i] <- trueModel5$coefficients[6]
  trueParams$converged[trueParams$ID == i] <- trueModel5$converged
  
  # simulate data
  simdat <- subset(data, Trial == 5 & Subject == i, -Choice)
  simdat$Choice <- predict(trueModel5, type = "response")
  simdat$Choice <- ifelse(simdat$Choice < runif(nrow(simdat)), 0, 1)
  
  simModel <- glm(Choice ~ V*Horizon + RU*Horizon ,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$V[simParams$ID == i] <- simModel$coefficients[2]
  simParams$RU[simParams$ID == i] <- simModel$coefficients[4]
  simParams$Horizon[simParams$ID == i] <- simModel$coefficients[3]
  simParams$VH[simParams$ID == i] <- simModel$coefficients[5]
  simParams$RUH[simParams$ID == i] <- simModel$coefficients[6]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}


# how many converged?
table(simParams$converged)
table(trueParams$converged)

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged)

trueParams$bothConverged <- simParams$bothConverged


# get correlations
cors <- data.frame(true = rep(c("RU", "V", "Horizon", "RUH", "VH"), 5),
                   recovered =  rep(c("RU", "V", "Horizon", "RUH", "VH"), each = 5),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                             simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))


# plot them

library(ggplot2)

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) 

cors$cor[cors$true == cors$recovered]



############### recovery bayes + thompson ##################

trueParams <- data.frame(ID = unique(data$Subject),
                         VTU = rep(NA, length(unique(data$Subject))),
                         Horizon = rep(NA, length(unique(data$Subject))),
                         VTUH = rep(NA, length(unique(data$Subject))),
                         converged = rep(NA, length(unique(data$Subject))))

simParams <- data.frame(ID = unique(data$Subject),
                        VTU = rep(NA, length(unique(data$Subject))),
                        Horizon = rep(NA, length(unique(data$Subject))),
                        VTUH = rep(NA, length(unique(data$Subject))),
                        converged = rep(NA, length(unique(data$Subject))))


for (i in unique(data$Subject)){
  
  trueModel5 <- glm(Choice ~ VTU*Horizon ,
                    data = data[data$Trial ==5 & data$Subject == i, ],
                    family = binomial(link = "probit"))
  # save coefficients
  trueParams$VTU[trueParams$ID == i] <- trueModel5$coefficients[2]
  trueParams$Horizon[trueParams$ID == i] <- trueModel5$coefficients[3]
  trueParams$VTUH[trueParams$ID == i] <- trueModel5$coefficients[4]
  trueParams$converged[trueParams$ID == i] <- trueModel5$converged
  
  # simulate data
  simdat <- subset(data, Trial == 5 & Subject == i, -Choice)
  simdat$Choice <- predict(trueModel5, type = "response")
  simdat$Choice <- ifelse(simdat$Choice < runif(nrow(simdat)), 0, 1)
  
  simModel <- glm(Choice ~ VTU*Horizon ,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$VTU[simParams$ID == i] <- simModel$coefficients[2]
  simParams$Horizon[simParams$ID == i] <- simModel$coefficients[3]
  simParams$VTUH[simParams$ID == i] <- simModel$coefficients[4]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}


# how many converged?
table(simParams$converged)
table(trueParams$converged)

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged)

trueParams$bothConverged <- simParams$bothConverged


# get correlations
cors <- data.frame(true = rep(c("VTU", "Horizon", "VTUH"), 3),
                   recovered =  rep(c("VTU", "Horizon", "VTUH"), each = 3),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                             simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))


# plot them

library(ggplot2)

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(x = true, y = recovered, label = round(cor, digits = 2)))

cors$cor[cors$true == cors$recovered]


############### recovery bayes + Softmax #################


trueParams <- data.frame(ID = unique(data$Subject),
                         V = rep(NA, length(unique(data$Subject))),
                         Horizon = rep(NA, length(unique(data$Subject))),
                         VH = rep(NA, length(unique(data$Subject))),
                         converged = rep(NA, length(unique(data$Subject))))

simParams <- data.frame(ID = unique(data$Subject),
                        V = rep(NA, length(unique(data$Subject))),
                        Horizon = rep(NA, length(unique(data$Subject))),
                        VH = rep(NA, length(unique(data$Subject))),
                        converged = rep(NA, length(unique(data$Subject))))


for (i in unique(data$Subject)){
  
  trueModel5 <- glm(Choice ~ V*Horizon ,
                    data = data[data$Trial ==5 & data$Subject == i, ],
                    family = binomial(link = "probit"))
  # save coefficients
  trueParams$V[trueParams$ID == i] <- trueModel5$coefficients[2]
  trueParams$Horizon[trueParams$ID == i] <- trueModel5$coefficients[3]
  trueParams$VH[trueParams$ID == i] <- trueModel5$coefficients[4]
  trueParams$converged[trueParams$ID == i] <- trueModel5$converged
  
  # simulate data
  simdat <- subset(data, Trial == 5 & Subject == i, -Choice)
  simdat$Choice <- predict(trueModel5, type = "response")
  simdat$Choice <- ifelse(simdat$Choice < runif(nrow(simdat)), 0, 1)
  
  simModel <- glm(Choice ~ V*Horizon ,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$V[simParams$ID == i] <- simModel$coefficients[2]
  simParams$Horizon[simParams$ID == i] <- simModel$coefficients[3]
  simParams$VH[simParams$ID == i] <- simModel$coefficients[4]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}


# how many converged?
table(simParams$converged)
table(trueParams$converged)

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged)

trueParams$bothConverged <- simParams$bothConverged


# get correlations
cors <- data.frame(true = rep(c("V", "Horizon", "VH"), 3),
                   recovered =  rep(c("V", "Horizon", "VH"), each = 3),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                             simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))


# plot them

library(ggplot2)

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(x = true, y = recovered, label = round(cor, digits = 2)))

cors$cor[cors$true == cors$recovered]
