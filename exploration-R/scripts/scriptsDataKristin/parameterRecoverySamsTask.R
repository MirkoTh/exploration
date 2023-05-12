########################## parameter recovery for the data in Fan et al. #####################

setwd("/Users/kwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/FanEtAl")
library(ggplot2)
theme_set(theme_classic(base_size = 15))
library(lme4)
library(dplyr)
library(brms)
library(tidyr)

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

data <- read.csv("exp1_bandit_task_scale.csv")
# the way the choice is coded is confusing and I'll just reverse it at the start to be 0 = left, 1  = right

data$C <- ifelse(data$C == 0, 1, 0)

############ What models do we want to use? ###########

## learning models:
# kalman filter

## choice models:
# probit: 
# thompson
# UCB
# softmax


####### directly add the output of the learning models to the dataframe to simplify the fitting ##############

## I will also do this for the observed data, not just for the simulated data, to ensure that my implementation is correct

## from Mirko (adapted slightly to match with Fan et al paper):
kalman_learning <- function(tbl_df, no, sigma_xi_sq, sigma_epsilon_sq) {
  #' Kalman filter without choice model given chosen options by participants
  #' 
  #' @description applies Kalman filter equations for a given bandit task with existing choices by participants
  #' @param tbl_df with made choices and collected rewards as columns
  #' @param no number of response options
  #' @param sigma_xi_sq innovation variance
  #' @param sigma_epsilon_sq error variance (prior variance)
  #' @return a tbl with by-trial posterior means and variances for all bandits
  rewards <- tbl_df$reward
  choices <- tbl_df$C
  m0 <- 0
  v0 <- 100
  nt <- length(rewards) # number of time points
  m <- matrix(m0, ncol = no, nrow = nt + 1) # to hold the posterior means
  v <- matrix(v0, ncol = no, nrow = nt + 1) # to hold the posterior variances
  
  for(t in 1:nt) {
    kt <- rep(0, no)
    # set the Kalman gain for the chosen option
    # sigma xi differs between options so need to index it
    kt[choices[t]+1] <- (v[t,choices[t]+1] + sigma_xi_sq[choices[t]+1])/(v[t,choices[t]+1] + sigma_epsilon_sq + sigma_xi_sq[choices[t]+1])
    # compute the posterior means
    m[t+1,] <- m[t,] + kt*(rewards[t] - m[t,])
    # compute the posterior variances
    v[t+1,] <- (1-kt)*(v[t,]) + sigma_xi_sq
  }
  tbl_m <- as.data.frame(m)
  # constrain v from becoming to small
  v <- t(apply(v, 1, function(x) pmax(x, .0001)))
  tbl_v <- as.data.frame(v)
  colnames(tbl_m) <- paste("m_", 1:no, sep = "")
  colnames(tbl_v) <- paste("v_", 1:no, sep = "")
  tbl_return <- tibble(cbind(tbl_m, tbl_v))
  
  return(tbl_return)
}


getV <- function(m1, m2){
  return(m1-m2)
}

getRU <- function(s1, s2){
  
  return(sqrt(s1) - sqrt(s2))
}



data$KLM0 <- NA
data$KLM1 <- NA
data$KLV0 <- NA
data$KLV1 <- NA


counter <- 1
for (i in unique(paste(data$sub, data$block))){ # running this entirely takes very long, only ran a bit to check that it is the same as provided in data
  dat <- subset(data, paste(data$sub, data$block) == i)
  
  # stable or fluctuating arms? -> get innovation variance based on this
  # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
  if (dat$cond[1] == 1) {xi <- c(4,0)
  } else if (dat$cond[1] == 2) {xi <- c(0,4)
  } else if (dat$cond[1] == 3) {xi <- c(4,4)
  } else if (dat$cond[1] == 4) {xi <- c(0,0)}
  
  posterior <- kalman_learning(dat, 2, xi, 1)
  
  data$KLM0[paste(data$sub, data$block) == i] <- posterior$m_1[1:10]
  data$KLM1[paste(data$sub, data$block) == i] <- posterior$m_2[1:10]
  data$KLV0[paste(data$sub, data$block) == i] <- posterior$v_1[1:10]
  data$KLV1[paste(data$sub, data$block) == i] <- posterior$v_2[1:10]
  
  counter <- counter +1
  if (counter%%100 == 0){print(paste(counter, "/", length(unique(paste(data$sub, data$block)))))}
  
}


################# recovery sam's model ##################

trueParams <- data.frame(ID = unique(data$sub),
                         V = rep(NA, length(unique(data$sub))),
                         RU = rep(NA, length(unique(data$sub))),
                         VTU = rep(NA, length(unique(data$sub))),
                         converged = rep(NA, length(unique(data$sub))))

simParams <- data.frame(ID = unique(data$sub),
                        V = rep(NA, length(unique(data$sub))),
                        RU = rep(NA, length(unique(data$sub))),
                        VTU = rep(NA, length(unique(data$sub))),
                        converged = rep(NA, length(unique(data$sub))))


for (i in unique(data$sub)){
  if (i %% 10 == 0) {print(i)}
  
  trueModel <- glm(C ~ V+ RU + VTU,
                    data = data[data$sub == i, ],
                    family = binomial(link = "probit"))
  # save coefficients
  trueParams$V[trueParams$ID == i] <- trueModel$coefficients[2]
  trueParams$RU[trueParams$ID == i] <- trueModel$coefficients[3]
  trueParams$VTU[trueParams$ID == i] <- trueModel$coefficients[4]
  trueParams$converged[trueParams$ID == i] <- trueModel$converged
  
  # simulate data
  simdat <- subset(data, sub == i, -c(C, V, RU, VTU, est_m1, est_m2, est_s1, est_s2, reward))
  simdat$C <- predict(trueModel, type = "response")
  simdat$C <- ifelse(simdat$C < runif(nrow(simdat)), 0, 1)
  
  # calculate kalman filter stuff again
  simdat$KLM0 <- NA
  simdat$KLM1 <- NA
  simdat$KLV0 <- NA
  simdat$KLV1 <- NA
  
  for (k in unique(simdat$block)){
    dat <- subset(simdat, simdat$block == k)
    
    # stable or fluctuating arms? -> get innovation variance based on this
    # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
    if (dat$cond[1] == 1) {xi <- c(4,0)
    } else if (dat$cond[1] == 2) {xi <- c(0,4)
    } else if (dat$cond[1] == 3) {xi <- c(4,4)
    } else if (dat$cond[1] == 4) {xi <- c(0,0)}
    
    # redo reward variable based on their choices
    
    dat$reward <- ifelse(dat$C == 0, dat$reward1, dat$reward2)
    
    posterior <- kalman_learning(dat, 2, xi, 1)
    
    simdat$KLM0[simdat$block == k] <- posterior$m_1[1:10]
    simdat$KLM1[simdat$block == k] <- posterior$m_2[1:10]
    simdat$KLV0[simdat$block == k] <- posterior$v_1[1:10]
    simdat$KLV1[simdat$block == k] <- posterior$v_2[1:10]
    
  }
  
  # re-calculate V, RU, VTU
  
  simdat$RU <- scale(sqrt(simdat$KLV0) - sqrt(simdat$KLV1))
  simdat$V <- simdat$KLM0 - simdat$KLM1
  simdat$VTU <- scale(simdat$V/(sqrt(simdat$KLV0 + simdat$KLV1)))
  simdat$V <- scale(simdat$V)
  
  simModel <- glm(C ~ V+ RU + VTU,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$V[simParams$ID == i] <- simModel$coefficients[2]
  simParams$RU[simParams$ID == i] <- simModel$coefficients[3]
  simParams$VTU[simParams$ID == i] <- simModel$coefficients[4]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}

# how many converged?
table(simParams$converged)# all
table(trueParams$converged)# all but 2

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged) # 20 did not converge

trueParams$bothConverged <- simParams$bothConverged


# get correlations
cors <- data.frame(true = rep(c("RU", "V","VTU"), 3),
                   recovered =  rep(c("RU", "V", "VTU"), each = 3),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                             simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))


# plot them

library(ggplot2)

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2)))

cors$cor[cors$true == cors$recovered]



############# redo Sam's model recovery iteratively bc that improved the recovery so much for UCB ################

trueParams <- data.frame(ID = unique(data$sub),
                         V = rep(NA, length(unique(data$sub))),
                         RU = rep(NA, length(unique(data$sub))),
                         VTU = rep(NA, length(unique(data$sub))),
                         converged = rep(NA, length(unique(data$sub))))

simParams <- data.frame(ID = unique(data$sub),
                        V = rep(NA, length(unique(data$sub))),
                        RU = rep(NA, length(unique(data$sub))),
                        VTU = rep(NA, length(unique(data$sub))),
                        converged = rep(NA, length(unique(data$sub))))


##### initialisation bc I copied this code from powerRecovery.R

blocks <- max(data$block)
trials <- max(data$trial)
nsubs <- 1 # bc we do 1 subject at a time

data$VTU_old <- data$V_old/data$TU_old

for (i in unique(data$sub)){
  if (i %% 10 == 0) {print(i)}
  
  trueModel <- glm(C ~ V_old+ RU_old + VTU_old,
                   data = data[data$sub == i, ],
                   family = binomial(link = "probit"))
  # save coefficients
  trueParams$V[trueParams$ID == i] <- trueModel$coefficients[2]
  trueParams$RU[trueParams$ID == i] <- trueModel$coefficients[3]
  trueParams$VTU[trueParams$ID == i] <- trueModel$coefficients[4]
  trueParams$converged[trueParams$ID == i] <- trueModel$converged
  
  # simulate data
  
  ##### create data
  
  simdat <- subset(data, sub == i, -c(C, V, RU, VTU, est_m1, est_m2, est_s1, est_s2, reward, reward1, reward2, V_old, RU_old, VTU_old))
  
  ## create rewards
  simdat$reward1[simdat$trial == 1] <- sample(data$reward1, blocks, replace = T)
  simdat$reward2[simdat$trial == 1] <- sample(data$reward2, blocks, replace = T)
  
  # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
  # stable condition:
  # sample a value for each first trial and repeat it for the rest of the trials
  simdat$reward1[simdat$cond == 4 | simdat$cond == 2] <- rep(sample(data$reward1, nrow(simdat[(simdat$cond == 4 | simdat$cond == 2) & simdat$trial == 1, ]), replace = T), each = trials)
  simdat$reward2[ simdat$cond == 4 | simdat$cond == 1] <- rep(sample(data$reward2, nrow(simdat[(simdat$cond == 4 | simdat$cond == 1)& simdat$trial == 1, ]), replace = T), each = trials)
  
  # random walk
  for (j in 2:trials){
    simdat$reward1[simdat$trial == j & (simdat$cond == 1 | simdat$cond == 3)] <- rnorm(1,simdat$reward1[simdat$trial == j-1 & (simdat$cond == 1 | simdat$cond == 3)],4)
    simdat$reward2[simdat$trial == j & (simdat$cond == 2 | simdat$cond == 3)] <- rnorm(1,simdat$reward2[simdat$trial == j-1 & (simdat$cond == 2 | simdat$cond == 3)],4)
  }
  # add noise
  noise <- rnorm(trials*blocks, 0, 1)
  simdat$reward1 <- simdat$reward1 + noise
  noise <- rnorm(trials*blocks, 0, 1)
  simdat$reward2 <- simdat$reward2 + noise
  
  
  ## iteratively make choices
  # learning part initialisations
  m0 <- 0
  v0 <- 100
  no <- 2
  sigma_epsilon_sq <- 1
  # sigma xi sq depends on the condition 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
  sigma_xi_sq <- matrix(NA, ncol = 2, nrow = nsubs*blocks)
  sigma_xi_sq[ ,1] <- apply(as.array(simdat$cond[simdat$trial == 1]), 1, function(x) ifelse(x == 1 | x == 3, 4, 0))
  sigma_xi_sq[ ,2] <- apply(as.array(simdat$cond[simdat$trial == 1]), 1, function(x) ifelse(x == 2 | x == 3, 4, 0))
  
  # get initial posterior mean and variance for each option
  m <- matrix(0, ncol = no, nrow = nrow(simdat)) # to hold the posterior means
  v <- matrix(100, ncol = no, nrow = nrow(simdat)) # to hold the posterior variances
  # get initial V and RU
  simdat$V_old[simdat$trial == 1] <- 0
  simdat$RU_old[simdat$trial == 1] <- 0
  simdat$VTU_old[simdat$trial == 1] <- 0
  
  for(t in 1:trials) {
    # get choice
    C <- predict(trueModel, newdata = simdat[simdat$trial == t, ] , type = "response")
    C <- ifelse(runif(length(C)) < C, 1, 0)
    simdat$C[simdat$trial == t] <- C
    # get reward
    reward <- ifelse(C == 0, simdat$reward1[simdat$trial == t], simdat$reward2[simdat$trial == t])
    
    # set the Kalman gain for the chosen option
    # sigma xi differs between options so need to index it
    kt <- matrix(0, ncol = no, nrow = nsubs*blocks)
    # indexing doesn't really work if not consistently same column so the _chosen thing is my akward workaround
    v_chosen <- ifelse(C == 0, v[simdat$trial == t, 1], v[simdat$trial == t, 2])
    sigma_xi_chosen <-  ifelse(C == 0, sigma_xi_sq[ , 1], sigma_xi_sq[ ,2])
    kt_chosen <- (v_chosen + sigma_xi_chosen)/(v_chosen + sigma_epsilon_sq + sigma_xi_chosen)
    kt[ ,1] <- ifelse(C == 0, kt_chosen, 0)
    kt[ ,2] <- ifelse(C == 1, kt_chosen, 0)
    # compute the posterior means
    m[simdat$trial == (t+1),] <- m[simdat$trial == t,] + kt*(reward - m[simdat$trial == t,])
    # compute the posterior variances
    v[simdat$trial == (t+1), ] <- (1-kt)*(v[simdat$trial == t,]) + sigma_xi_sq
    
    # compute V and RU
    simdat$V_old[simdat$trial == (t+1)] <- getV(m[simdat$trial == (t+1),1], m[simdat$trial == (t+1),2])
    simdat$RU_old[simdat$trial == (t+1)] <- getRU(v[simdat$trial == (t+1),1], v[simdat$trial == (t+1),2])
    simdat$VTU_old[simdat$trial == (t+1)] <- simdat$V_old[simdat$trial == (t+1)]/(sqrt(v[simdat$trial == (t+1),1] + v[simdat$trial == (t+1),2]))
    
  }
  
  
  simdat$V <- simdat$V_old
  simdat$RU <- simdat$RU_old
  simdat$VTU <- simdat$VTU_old
  simdat$C <- as.integer(simdat$C)
  
  simModel <- glm(C ~ V+ RU + VTU,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$V[simParams$ID == i] <- simModel$coefficients[2]
  simParams$RU[simParams$ID == i] <- simModel$coefficients[3]
  simParams$VTU[simParams$ID == i] <- simModel$coefficients[4]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}

# how many converged?
table(simParams$converged)# all converged
table(trueParams$converged)# 2 didnt

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged) 

trueParams$bothConverged <- simParams$bothConverged


# get correlations
cors <- data.frame(true = rep(c("RU", "V","VTU"), 3),
                   recovered =  rep(c("RU", "V", "VTU"), each = 3),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                             simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))


# plot them

library(ggplot2)

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2)))



##################### UCB ################



trueParams <- data.frame(ID = unique(data$sub),
                         V = rep(NA, length(unique(data$sub))),
                         RU = rep(NA, length(unique(data$sub))),
                         converged = rep(NA, length(unique(data$sub))))

simParams <- data.frame(ID = unique(data$sub),
                        V = rep(NA, length(unique(data$sub))),
                        RU = rep(NA, length(unique(data$sub))),
                        converged = rep(NA, length(unique(data$sub))))


for (i in unique(data$sub)){
  if (i %% 10 == 0) {print(i)}
  
  trueModel <- glm(C ~ V+ RU,
                   data = data[data$sub == i, ],
                   family = binomial(link = "probit"))
  # save coefficients
  trueParams$V[trueParams$ID == i] <- trueModel$coefficients[2]
  trueParams$RU[trueParams$ID == i] <- trueModel$coefficients[3]
  trueParams$converged[trueParams$ID == i] <- trueModel$converged
  
  # simulate data
  simdat <- subset(data, sub == i, -c(C, V, RU, VTU, est_m1, est_m2, est_s1, est_s2, reward))
  simdat$C <- predict(trueModel, type = "response")
  simdat$C <- ifelse(simdat$C < runif(nrow(simdat)), 0, 1)
  
  # calculate kalman filter stuff again
  simdat$KLM0 <- NA
  simdat$KLM1 <- NA
  simdat$KLV0 <- NA
  simdat$KLV1 <- NA
  
  for (k in unique(simdat$block)){
    dat <- subset(simdat, simdat$block == k)
    
    # stable or fluctuating arms? -> get innovation variance based on this
    # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
    if (dat$cond[1] == 1) {xi <- c(4,0)
    } else if (dat$cond[1] == 2) {xi <- c(0,4)
    } else if (dat$cond[1] == 3) {xi <- c(4,4)
    } else if (dat$cond[1] == 4) {xi <- c(0,0)}
    
    # redo reward variable based on their choices
    
    dat$reward <- ifelse(dat$C == 0, dat$reward1, dat$reward2)
    
    posterior <- kalman_learning(dat, 2, xi, 1)
    
    simdat$KLM0[simdat$block == k] <- posterior$m_1[1:10]
    simdat$KLM1[simdat$block == k] <- posterior$m_2[1:10]
    simdat$KLV0[simdat$block == k] <- posterior$v_1[1:10]
    simdat$KLV1[simdat$block == k] <- posterior$v_2[1:10]
    
  }
  
  # re-calculate V, RU, VTU
  
  simdat$RU <- scale(sqrt(simdat$KLV0) - sqrt(simdat$KLV1))
  simdat$V <- simdat$KLM0 - simdat$KLM1
  simdat$VTU <- scale(simdat$V/(sqrt(simdat$KLV0+ simdat$KLV1)))
  simdat$V <- scale(simdat$V)
  
  simModel <- glm(C ~ V+ RU,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$V[simParams$ID == i] <- simModel$coefficients[2]
  simParams$RU[simParams$ID == i] <- simModel$coefficients[3]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}

# how many converged?
table(simParams$converged)# all
table(trueParams$converged)# all

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged) # 20 did not converge

trueParams$bothConverged <- simParams$bothConverged


# get correlations
cors <- data.frame(true = rep(c("RU", "V"), 2),
                   recovered =  rep(c("RU", "V"), each = 2),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                             simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))


# plot them

library(ggplot2)

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2)))

cors$cor[cors$true == cors$recovered]

################## UCB with glm and iteratively #################
##### initialisation bc I copied this code from powerRecovery.R

blocks <- max(data$block)
trials <- max(data$trial)
nsubs <- 1 # bc we do 1 subject at a time


trueParams <- data.frame(ID = unique(data$sub),
                         V = rep(NA, length(unique(data$sub))),
                         RU = rep(NA, length(unique(data$sub))),
                         converged = rep(NA, length(unique(data$sub))))

simParams <- data.frame(ID = unique(data$sub),
                        V = rep(NA, length(unique(data$sub))),
                        RU = rep(NA, length(unique(data$sub))),
                        converged = rep(NA, length(unique(data$sub))))



for (i in unique(data$sub)){
  if (i %% 10 == 0) {print(i)}
  
  trueModel <- glm(C ~ V_old+ RU_old,
                   data = data[data$sub == i, ],
                   family = binomial(link = "probit"))
  # save coefficients
  trueParams$V[trueParams$ID == i] <- trueModel$coefficients[2]
  trueParams$RU[trueParams$ID == i] <- trueModel$coefficients[3]
  trueParams$converged[trueParams$ID == i] <- trueModel$converged
  
  # simulate data
  
  ##### create data
  
  simdat <- subset(data, sub == i, -c(C, V, RU, VTU, est_m1, est_m2, est_s1, est_s2, reward, reward1, reward2, V_old, RU_old))
  
  ## create rewards
  simdat$reward1[simdat$trial == 1] <- sample(data$reward1, blocks, replace = T)
  simdat$reward2[simdat$trial == 1] <- sample(data$reward2, blocks, replace = T)
  
  # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
  # stable condition:
  # sample a value for each first trial and repeat it for the rest of the trials
  simdat$reward1[simdat$cond == 4 | simdat$cond == 2] <- rep(sample(data$reward1, nrow(simdat[(simdat$cond == 4 | simdat$cond == 2) & simdat$trial == 1, ]), replace = T), each = trials)
  simdat$reward2[ simdat$cond == 4 | simdat$cond == 1] <- rep(sample(data$reward2, nrow(simdat[(simdat$cond == 4 | simdat$cond == 1)& simdat$trial == 1, ]), replace = T), each = trials)
  
  # random walk
  for (j in 2:trials){
    simdat$reward1[simdat$trial == j & (simdat$cond == 1 | simdat$cond == 3)] <- rnorm(1,simdat$reward1[simdat$trial == j-1 & (simdat$cond == 1 | simdat$cond == 3)],4)
    simdat$reward2[simdat$trial == j & (simdat$cond == 2 | simdat$cond == 3)] <- rnorm(1,simdat$reward2[simdat$trial == j-1 & (simdat$cond == 2 | simdat$cond == 3)],4)
  }
  # add noise
  noise <- rnorm(trials*blocks, 0, 1)
  simdat$reward1 <- simdat$reward1 + noise
  noise <- rnorm(trials*blocks, 0, 1)
  simdat$reward2 <- simdat$reward2 + noise
  
  
  ## iteratively make choices
  # learning part initialisations
  m0 <- 0
  v0 <- 100
  no <- 2
  sigma_epsilon_sq <- 1
  # sigma xi sq depends on the condition 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
  sigma_xi_sq <- matrix(NA, ncol = 2, nrow = nsubs*blocks)
  sigma_xi_sq[ ,1] <- apply(as.array(simdat$cond[simdat$trial == 1]), 1, function(x) ifelse(x == 1 | x == 3, 4, 0))
  sigma_xi_sq[ ,2] <- apply(as.array(simdat$cond[simdat$trial == 1]), 1, function(x) ifelse(x == 2 | x == 3, 4, 0))
  
  # get initial posterior mean and variance for each option
  m <- matrix(0, ncol = no, nrow = nrow(simdat)) # to hold the posterior means
  v <- matrix(100, ncol = no, nrow = nrow(simdat)) # to hold the posterior variances
  # get initial V and RU
  simdat$V_old[simdat$trial == 1] <- 0
  simdat$RU_old[simdat$trial == 1] <- 0
  
  for(t in 1:trials) {
    # get choice
    C <- predict(trueModel, newdata = simdat[simdat$trial == t, ] , type = "response")
    C <- ifelse(runif(length(C)) < C, 1, 0)
    simdat$C[simdat$trial == t] <- C
    # get reward
    reward <- ifelse(C == 0, simdat$reward1[simdat$trial == t], simdat$reward2[simdat$trial == t])
    
    # set the Kalman gain for the chosen option
    # sigma xi differs between options so need to index it
    kt <- matrix(0, ncol = no, nrow = nsubs*blocks)
    # indexing doesn't really work if not consistently same column so the _chosen thing is my akward workaround
    v_chosen <- ifelse(C == 0, v[simdat$trial == t, 1], v[simdat$trial == t, 2])
    sigma_xi_chosen <-  ifelse(C == 0, sigma_xi_sq[ , 1], sigma_xi_sq[ ,2])
    kt_chosen <- (v_chosen + sigma_xi_chosen)/(v_chosen + sigma_epsilon_sq + sigma_xi_chosen)
    kt[ ,1] <- ifelse(C == 0, kt_chosen, 0)
    kt[ ,2] <- ifelse(C == 1, kt_chosen, 0)
    # compute the posterior means
    m[simdat$trial == (t+1),] <- m[simdat$trial == t,] + kt*(reward - m[simdat$trial == t,])
    # compute the posterior variances
    v[simdat$trial == (t+1), ] <- (1-kt)*(v[simdat$trial == t,]) + sigma_xi_sq
    
    # compute V and RU
    simdat$V_old[simdat$trial == (t+1)] <- getV(m[simdat$trial == (t+1),1], m[simdat$trial == (t+1),2])
    simdat$RU_old[simdat$trial == (t+1)] <- getRU(v[simdat$trial == (t+1),1], v[simdat$trial == (t+1),2])
    
  }
  
  
  simdat$V <- simdat$V_old
  simdat$RU <- simdat$RU_old
  simdat$C <- as.integer(simdat$C)
  
  simModel <- glm(C ~ V+ RU,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$V[simParams$ID == i] <- simModel$coefficients[2]
  simParams$RU[simParams$ID == i] <- simModel$coefficients[3]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}


# how many converged?
table(simParams$converged)# all
table(trueParams$converged)# all

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged) # 20 did not converge

trueParams$bothConverged <- simParams$bothConverged


# get correlations
cors <- data.frame(true = rep(c("RU", "V"), 2),
                   recovered =  rep(c("RU", "V"), each = 2),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[trueParams$bothConverged,grep(cors$true[x], colnames(trueParams))[1]],# converged rows, cols with correct variable name (first instance)
                                                             simParams[simParams$bothConverged, grep(cors$recovered[x], colnames(simParams))[1]]))


# plot them

library(ggplot2)

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2)))

cors$cor[cors$true == cors$recovered]



##################### UCB redo with glmer ######################


trueModel <- glmer(C ~  V + RU+ ( V+ RU | sub),
                 data = data,
                 family = binomial(link = "probit"))

ranefs <- as.data.frame(coef(trueModel)$sub)

## save random slopes
trueParams <- data.frame(ID = unique(data$sub),
                         V = ranefs$V,
                         RU = ranefs$RU)

## simulate data

simdat <- subset(data, sub == sub, -c(C, V, RU, VTU, reward))
simdat$C <- predict(trueModel, type = "response")
simdat$C <- ifelse(simdat$C < runif(nrow(simdat)), 0, 1)

# calculate kalman filter stuff again
simdat$KLM0 <- NA
simdat$KLM1 <- NA
simdat$KLV0 <- NA
simdat$KLV1 <- NA


counter = 1
for (id in unique(simdat$sub)){
  for (k in unique(simdat$block)){
    counter = counter +1
    dat <- subset(simdat, simdat$block == k & simdat$sub == id)
    if(counter %% 100 == 0){print(id)}
    
    # stable or fluctuating arms? -> get innovation variance based on this
    # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
    if (dat$cond[1] == 1) {xi <- c(4,0)
    } else if (dat$cond[1] == 2) {xi <- c(0,4)
    } else if (dat$cond[1] == 3) {xi <- c(4,4)
    } else if (dat$cond[1] == 4) {xi <- c(0,0)}
    
    # redo reward variable based on their choices
    
    dat$reward <- ifelse(dat$C == 0, dat$reward1, dat$reward2)
    
    posterior <- kalman_learning(dat, 2, xi, 1)
    
    simdat$KLM0[simdat$sub == id & simdat$block == k] <- posterior$m_1[1:10]
    simdat$KLM1[simdat$sub == id & simdat$block == k] <- posterior$m_2[1:10]
    simdat$KLV0[simdat$sub == id & simdat$block == k] <- posterior$v_1[1:10]
    simdat$KLV1[simdat$sub == id & simdat$block == k] <- posterior$v_2[1:10]
    
  }
  
  
}

# re-calculate V, RU

simdat$RU <- scale(sqrt(simdat$KLV0) - sqrt(simdat$KLV1))
simdat$V <- simdat$KLM0 - simdat$KLM1
simdat$VTU <- scale(simdat$V/(sqrt(simdat$KLV0+ simdat$KLV1)))
simdat$V <- scale(simdat$V)


simModel <- glmer(C ~ V+ RU + (V + RU |sub),
                data = simdat,
                family = binomial(link = "probit"))

ranefsSim <- as.data.frame(coef(simModel)$sub)

simParams <- data.frame(ID = unique(data$sub),
                        V = ranefsSim$V,
                        RU = ranefsSim$RU)



#### plot recovery

cors <- data.frame(true = rep(c("RU", "V"), 2),
                   recovered =  rep(c("RU", "V"), each = 2),
                   cor = NA)

cors$cor <- apply(as.array(1:nrow(cors)), 1, function(x) cor(trueParams[ ,grep(cors$true[x], colnames(trueParams))[1]],# cols with correct variable name (first instance)
                                                             simParams[ , grep(cors$recovered[x], colnames(simParams))[1]]))



library(ggplot2)

ggplot(cors, aes(x = true, y = recovered, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2)))

cors$cor[cors$true == cors$recovered]

########## UCB with glmer and iteratively #############

######## !!!!!! for now I cannot get this to work bc I cannot get the glmer to converge with unstandardised predictors and full sample 


blocks <- max(data$block)
trials <- max(data$trial)
nsubs <- length(unique(data$sub)) 

dat <- data

trueModel <- glmer(C ~ V_old+ RU_old + (V_old + RU_old | sub),
                   data = dat,
                   family = binomial(link = "probit"))



## save true params
ranefs <- as.data.frame(coef(trueModel)$sub)

trueParams <- data.frame(ID = unique(dat$sub),
                         V = ranefs$V,
                         RU = ranefs$RU)

## initialise array for simulated parameter estimates
simParams <- data.frame(ID = unique(dat$sub),
                        V = rep(NA, nsubs),
                        RU = rep(NA, nsubs))


    ##### create data
    
    da <- data.frame(sub = rep(subsIncl, each = trials*blocks),
                     block = rep(1:blocks, each = trials),
                     trial = rep(1:trials, blocks),
                     cond = rep(sample(dat$cond, nsubs*blocks, replace = T), each = trials))
    
    ## create rewards
    da$reward1[da$trial == 1] <- sample(dat$reward1, blocks, replace = T)
    da$reward2[da$trial == 1] <- sample(dat$reward2, blocks, replace = T)
    
    # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
    # stable condition:
    da$reward1[da$cond == 4 | da$cond == 2] <- rep(sample(dat$reward1, nrow(da[(da$cond == 4 | da$cond == 2) & da$trial == 1, ]), replace = T), each = trials)
    da$reward2[ da$cond == 4 | da$cond == 1] <- rep(sample(dat$reward2, nrow(da[(da$cond == 4 | da$cond == 1)& da$trial == 1, ]), replace = T), each = trials)
    
    # random walk
    for (j in 2:trials){
      da$reward1[da$trial == j & (da$cond == 1 | da$cond == 3)] <- rnorm(1,da$reward1[da$trial == j-1 & (da$cond == 1 | da$cond == 3)],4)
      da$reward2[da$trial == j & (da$cond == 2 | da$cond == 3)] <- rnorm(1,da$reward2[da$trial == j-1 & (da$cond == 2 | da$cond == 3)],4)
    }
    # add noise
    noise <- rnorm(trials*blocks*nsubs, 0, 1)
    da$reward1 <- da$reward1 + noise
    noise <- rnorm(trials*blocks*nsubs, 0, 1)
    da$reward2 <- da$reward2 + noise
    
    
    ## iteratively make choices
    # learning part initialisations
    m0 <- 0
    v0 <- 100
    no <- 2
    sigma_epsilon_sq <- 1
    # sigma xi sq depends on the condition 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
    sigma_xi_sq <- matrix(NA, ncol = 2, nrow = nsubs*blocks)
    sigma_xi_sq[ ,1] <- apply(as.array(da$cond[da$trial == 1]), 1, function(x) ifelse(x == 1 | x == 3, 4, 0))
    sigma_xi_sq[ ,2] <- apply(as.array(da$cond[da$trial == 1]), 1, function(x) ifelse(x == 2 | x == 3, 4, 0))
    
    # get initial posterior mean and variance for each option
    m <- matrix(0, ncol = no, nrow = nrow(da)) # to hold the posterior means
    v <- matrix(100, ncol = no, nrow = nrow(da)) # to hold the posterior variances
    # get initial V and RU
    da$V_old[da$trial == 1] <- 0
    da$RU_old[da$trial == 1] <- 0
    
    for(t in 1:trials) {
      # get choice
      C <- predict(trueModel, newdata = da[da$trial == t, ] , type = "response")
      C <- ifelse(runif(length(C)) < C, 1, 0)
      da$C[da$trial == t] <- C
      # get reward
      reward <- ifelse(C == 0, da$reward1[da$trial == t], da$reward2[da$trial == t])
      
      # set the Kalman gain for the chosen option
      # sigma xi differs between options so need to index it
      kt <- matrix(0, ncol = no, nrow = nsubs*blocks)
      # indexing doesn't really work if not consistently same column so the _chosen thing is my akward workaround
      v_chosen <- ifelse(C == 0, v[da$trial == t, 1], v[da$trial == t, 2])
      sigma_xi_chosen <-  ifelse(C == 0, sigma_xi_sq[ , 1], sigma_xi_sq[ ,2])
      kt_chosen <- (v_chosen + sigma_xi_chosen)/(v_chosen + sigma_epsilon_sq + sigma_xi_chosen)
      kt[ ,1] <- ifelse(C == 0, kt_chosen, 0)
      kt[ ,2] <- ifelse(C == 1, kt_chosen, 0)
      # compute the posterior means
      m[da$trial == (t+1),] <- m[da$trial == t,] + kt*(reward - m[da$trial == t,])
      # compute the posterior variances
      v[da$trial == (t+1), ] <- (1-kt)*(v[da$trial == t,]) + sigma_xi_sq
      
      # compute V and RU
      da$V_old[da$trial == (t+1)] <- getV(m[da$trial == (t+1),1], m[da$trial == (t+1),2])
      da$RU_old[da$trial == (t+1)] <- getRU(v[da$trial == (t+1),1], v[da$trial == (t+1),2])
      
    }
    
    
    # re-fit data
    da$V <- scale(da$V_old)[ ,1]
    da$RU <- scale(da$RU_old)[ ,1]
    da$C <- as.integer(da$C)
    
    if (exists("simModel")){rm(simModel)} # remove the simModel variable so that if this regression crashes, it doesn't just save the previous estimates
    
    simModel <- glmer(C ~ V + RU + (V + RU | sub),
                      data = da,
                      family = binomial(link = "probit"))
    
    # save params
    ranefsSim <- as.data.frame(coef(simModel)$sub)
    
    simParams <- data.frame(ID = unique(dat$sub),
                            V = ranefsSim$V,
                            RU = ranefsSim$RU)
    
    warn <- names(warnings())
    if (mean(grepl("checkConv", warn)) > 0) {sprintf("%i trials %i blocks did not converge", trials, blocks)}
    
    
    # correlate them with true params
    
    recovery$cor[recovery$nblocks == blocks & recovery$ntrials == trials] <- apply(as.array(1:nrow(recovery[recovery$nblocks == blocks & recovery$ntrials == trials, ])), 1, function(x) 
      cor(trueParams[ ,grep(recovery$trueParam[x], colnames(trueParams))[1]],# cols with correct variable name (first instance)
          simParams[ , grep(recovery$simParam[x], colnames(simParams))[1]]))
    




############## plot it

ggplot(recovery, aes(x = trueParam, y = simParam, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) +
  facet_grid(rows = vars(ntrials), cols = vars(nblocks))

##################### Thompson sampling #############################

trueParams <- data.frame(ID = unique(data$sub),
                         VTU = rep(NA, length(unique(data$sub))),
                         converged = rep(NA, length(unique(data$sub))))

simParams <- data.frame(ID = unique(data$sub),
                        VTU = rep(NA, length(unique(data$sub))),
                        converged = rep(NA, length(unique(data$sub))))


for (i in unique(data$sub)){
  if (i %% 10 == 0) {print(i)}
  
  trueModel <- glm(C ~ VTU,
                   data = data[data$sub == i, ],
                   family = binomial(link = "probit"))
  # save coefficients
  trueParams$VTU[trueParams$ID == i] <- trueModel$coefficients[2]
  trueParams$converged[trueParams$ID == i] <- trueModel$converged
  
  # simulate data
  simdat <- subset(data, sub == i, -c(C, V, RU, VTU, est_m1, est_m2, est_s1, est_s2, reward))
  simdat$C <- predict(trueModel, type = "response")
  simdat$C <- ifelse(simdat$C < runif(nrow(simdat)), 0, 1)
  
  # calculate kalman filter stuff again
  simdat$KLM0 <- NA
  simdat$KLM1 <- NA
  simdat$KLV0 <- NA
  simdat$KLV1 <- NA
  
  for (k in unique(simdat$block)){
    dat <- subset(simdat, simdat$block == k)
    
    # stable or fluctuating arms? -> get innovation variance based on this
    # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
    if (dat$cond[1] == 1) {xi <- c(4,0)
    } else if (dat$cond[1] == 2) {xi <- c(0,4)
    } else if (dat$cond[1] == 3) {xi <- c(4,4)
    } else if (dat$cond[1] == 4) {xi <- c(0,0)}
    
    # redo reward variable based on their choices
    
    dat$reward <- ifelse(dat$C == 0, dat$reward1, dat$reward2)
    
    posterior <- kalman_learning(dat, 2, xi, 1)
    
    simdat$KLM0[simdat$block == k] <- posterior$m_1[1:10]
    simdat$KLM1[simdat$block == k] <- posterior$m_2[1:10]
    simdat$KLV0[simdat$block == k] <- posterior$v_1[1:10]
    simdat$KLV1[simdat$block == k] <- posterior$v_2[1:10]
    
  }
  
  # re-calculate V, RU, VTU
  
  #simdat$RU <- simdat$KLV0 - simdat$KLV1
  simdat$V <- simdat$KLM0 - simdat$KLM1
  simdat$VTU <- scale(simdat$V/(sqrt(simdat$KLV0+ simdat$KLV1)))
  simdat$V <- scale(simdat$V)
  
  simModel <- glm(C ~ VTU,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$VTU[simParams$ID == i] <- simModel$coefficients[2]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}

# how many converged?
table(simParams$converged)# all but 12
table(trueParams$converged)# all but 7

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged) # 19 did not converge

trueParams$bothConverged <- simParams$bothConverged


cor(simParams$VTU[simParams$bothConverged], trueParams$VTU[simParams$bothConverged]) # 0.3152554

################### Thompson sampling iteratively with glm #################


trueParams <- data.frame(ID = unique(data$sub),
                         VTU = rep(NA, length(unique(data$sub))),
                         converged = rep(NA, length(unique(data$sub))))

simParams <- data.frame(ID = unique(data$sub),
                        VTU = rep(NA, length(unique(data$sub))),
                        converged = rep(NA, length(unique(data$sub))))


blocks <- max(data$block)
trials <- max(data$trial)
nsubs <- 1 # bc we do 1 subject at a time

data$VTU_old <- data$V_old/data$TU_old

for (i in unique(data$sub)){
  if (i %% 10 == 0) {print(i)}
  
  trueModel <- glm(C ~ VTU_old,
                   data = data[data$sub == i, ],
                   family = binomial(link = "probit"))
  # save coefficients
  trueParams$VTU[trueParams$ID == i] <- trueModel$coefficients[2]
  trueParams$converged[trueParams$ID == i] <- trueModel$converged
  
  # simulate data
  
  ##### create data
  
  simdat <- subset(data, sub == i, -c(C, V, RU, VTU, est_m1, est_m2, est_s1, est_s2, reward, reward1, reward2, V_old, RU_old, VTU_old))
  
  ## create rewards
  simdat$reward1[simdat$trial == 1] <- sample(data$reward1, blocks, replace = T)
  simdat$reward2[simdat$trial == 1] <- sample(data$reward2, blocks, replace = T)
  
  # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
  # stable condition:
  # sample a value for each first trial and repeat it for the rest of the trials
  simdat$reward1[simdat$cond == 4 | simdat$cond == 2] <- rep(sample(data$reward1, nrow(simdat[(simdat$cond == 4 | simdat$cond == 2) & simdat$trial == 1, ]), replace = T), each = trials)
  simdat$reward2[ simdat$cond == 4 | simdat$cond == 1] <- rep(sample(data$reward2, nrow(simdat[(simdat$cond == 4 | simdat$cond == 1)& simdat$trial == 1, ]), replace = T), each = trials)
  
  # random walk
  for (j in 2:trials){
    simdat$reward1[simdat$trial == j & (simdat$cond == 1 | simdat$cond == 3)] <- rnorm(1,simdat$reward1[simdat$trial == j-1 & (simdat$cond == 1 | simdat$cond == 3)],4)
    simdat$reward2[simdat$trial == j & (simdat$cond == 2 | simdat$cond == 3)] <- rnorm(1,simdat$reward2[simdat$trial == j-1 & (simdat$cond == 2 | simdat$cond == 3)],4)
  }
  # add noise
  noise <- rnorm(trials*blocks, 0, 1)
  simdat$reward1 <- simdat$reward1 + noise
  noise <- rnorm(trials*blocks, 0, 1)
  simdat$reward2 <- simdat$reward2 + noise
  
  
  ## iteratively make choices
  # learning part initialisations
  m0 <- 0
  v0 <- 100
  no <- 2
  sigma_epsilon_sq <- 1
  # sigma xi sq depends on the condition 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
  sigma_xi_sq <- matrix(NA, ncol = 2, nrow = nsubs*blocks)
  sigma_xi_sq[ ,1] <- apply(as.array(simdat$cond[simdat$trial == 1]), 1, function(x) ifelse(x == 1 | x == 3, 4, 0))
  sigma_xi_sq[ ,2] <- apply(as.array(simdat$cond[simdat$trial == 1]), 1, function(x) ifelse(x == 2 | x == 3, 4, 0))
  
  # get initial posterior mean and variance for each option
  m <- matrix(0, ncol = no, nrow = nrow(simdat)) # to hold the posterior means
  v <- matrix(100, ncol = no, nrow = nrow(simdat)) # to hold the posterior variances
  # get initial V and RU
  simdat$V_old[simdat$trial == 1] <- 0
  simdat$VTU_old[simdat$trial == 1] <- 0
  
  for(t in 1:trials) {
    # get choice
    C <- predict(trueModel, newdata = simdat[simdat$trial == t, ] , type = "response")
    C <- ifelse(runif(length(C)) < C, 1, 0)
    simdat$C[simdat$trial == t] <- C
    # get reward
    reward <- ifelse(C == 0, simdat$reward1[simdat$trial == t], simdat$reward2[simdat$trial == t])
    
    # set the Kalman gain for the chosen option
    # sigma xi differs between options so need to index it
    kt <- matrix(0, ncol = no, nrow = nsubs*blocks)
    # indexing doesn't really work if not consistently same column so the _chosen thing is my akward workaround
    v_chosen <- ifelse(C == 0, v[simdat$trial == t, 1], v[simdat$trial == t, 2])
    sigma_xi_chosen <-  ifelse(C == 0, sigma_xi_sq[ , 1], sigma_xi_sq[ ,2])
    kt_chosen <- (v_chosen + sigma_xi_chosen)/(v_chosen + sigma_epsilon_sq + sigma_xi_chosen)
    kt[ ,1] <- ifelse(C == 0, kt_chosen, 0)
    kt[ ,2] <- ifelse(C == 1, kt_chosen, 0)
    # compute the posterior means
    m[simdat$trial == (t+1),] <- m[simdat$trial == t,] + kt*(reward - m[simdat$trial == t,])
    # compute the posterior variances
    v[simdat$trial == (t+1), ] <- (1-kt)*(v[simdat$trial == t,]) + sigma_xi_sq
    
    # compute V and RU
    simdat$V_old[simdat$trial == (t+1)] <- getV(m[simdat$trial == (t+1),1], m[simdat$trial == (t+1),2])
    simdat$VTU_old[simdat$trial == (t+1)] <- simdat$V_old[simdat$trial == (t+1)]/(sqrt(v[simdat$trial == (t+1),1] + v[simdat$trial == (t+1),2]))
    
  }
  
  
  simdat$VTU <- simdat$VTU_old
  simdat$C <- as.integer(simdat$C)
  
  simModel <- glm(C ~ VTU,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$VTU[simParams$ID == i] <- simModel$coefficients[2]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}

# how many converged?
table(simParams$converged)# all
table(trueParams$converged)# all but 7

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged) # 7 did not converge

trueParams$bothConverged <- simParams$bothConverged


cor(simParams$VTU[simParams$bothConverged], trueParams$VTU[simParams$bothConverged])# 0.969

########################### Softmax ######################

trueParams <- data.frame(ID = unique(data$sub),
                         V = rep(NA, length(unique(data$sub))),
                         converged = rep(NA, length(unique(data$sub))))

simParams <- data.frame(ID = unique(data$sub),
                        V = rep(NA, length(unique(data$sub))),
                        converged = rep(NA, length(unique(data$sub))))


for (i in unique(data$sub)){
  if (i %% 10 == 0) {print(i)}
  
  trueModel <- glm(C ~ V,
                   data = data[data$sub == i, ],
                   family = binomial(link = "probit"))
  # save coefficients
  trueParams$V[trueParams$ID == i] <- trueModel$coefficients[2]
  trueParams$converged[trueParams$ID == i] <- trueModel$converged
  
  # simulate data
  simdat <- subset(data, sub == i, -c(C, V, RU, VTU, est_m1, est_m2, est_s1, est_s2, reward))
  simdat$C <- predict(trueModel, type = "response")
  simdat$C <- ifelse(simdat$C < runif(nrow(simdat)), 0, 1)
  
  # calculate kalman filter stuff again
  simdat$KLM0 <- NA
  simdat$KLM1 <- NA
  simdat$KLV0 <- NA
  simdat$KLV1 <- NA
  
  for (k in unique(simdat$block)){
    dat <- subset(simdat, simdat$block == k)
    
    # stable or fluctuating arms? -> get innovation variance based on this
    # cond: experiment condition. 1:Fluctuating/Stable; 2:SF; 3:FF; 4:SS
    if (dat$cond[1] == 1) {xi <- c(4,0)
    } else if (dat$cond[1] == 2) {xi <- c(0,4)
    } else if (dat$cond[1] == 3) {xi <- c(4,4)
    } else if (dat$cond[1] == 4) {xi <- c(0,0)}
    
    # redo reward variable based on their choices
    
    dat$reward <- ifelse(dat$C == 0, dat$reward1, dat$reward2)
    
    posterior <- kalman_learning(dat, 2, xi, 1)
    
    simdat$KLM0[simdat$block == k] <- posterior$m_1[1:10]
    simdat$KLM1[simdat$block == k] <- posterior$m_2[1:10]
    simdat$KLV0[simdat$block == k] <- posterior$v_1[1:10]
    simdat$KLV1[simdat$block == k] <- posterior$v_2[1:10]
    
  }
  
  # re-calculate V, RU, VTU
  
 # simdat$RU <- simdat$KLV0 - simdat$KLV1
  simdat$V <- scale(simdat$KLM0 - simdat$KLM1)
 # simdat$VTU <- simdat$V/(sqrt(simdat$KLV0^2 + simdat$KLV1^2))
  
  simModel <- glm(C ~ V,
                  data = simdat,
                  family = binomial(link = "probit"))
  
  
  simParams$V[simParams$ID == i] <- simModel$coefficients[2]
  simParams$converged[simParams$ID == i] <- simModel$converged
  
}

# how many converged?
table(simParams$converged)# all
table(trueParams$converged)# all

simParams$bothConverged <- ifelse(simParams$converged & trueParams$converged, T, F)
table(simParams$bothConverged) 

trueParams$bothConverged <- simParams$bothConverged


cor(simParams$V, trueParams$V) # 0.589495

############### plot true and recovered parameter distributions Sam's model ###################
library(tidyr)

truePar <- pivot_longer(trueParams[trueParams$bothConverged, 1:4], 
                             cols = c("V", "VTU", "RU"), 
                             names_to = "parameter", 
                             values_to = "estimate")

truePar$source <- "true"

simPar <- pivot_longer(simParams[simParams$bothConverged, 1:4], 
                    cols = c("V", "VTU", "RU"), 
                    names_to = "parameter", 
                    values_to = "estimate")

simPar$source <- "recovered"

params <- rbind(truePar, simPar)

ggplot(params, aes(estimate, fill = source)) + geom_density(alpha = 0.3)+
  facet_wrap(facets = vars(parameter), scales = "free") # well you can't see shit here

ggplot(params, aes(estimate, fill = source)) + geom_boxplot(alpha = 0.3)+
  facet_grid(cols = vars(parameter), rows = vars(source), scales = "free") # but better but not much

ggplot(params[params$source == "true", ], aes(estimate, fill = source)) + geom_density(alpha = 0.3)+
  facet_grid(cols = vars(parameter), scales = "free")

############ for UCB


truePar <- pivot_longer(trueParams, 
                        cols = c("V", "RU"), 
                        names_to = "parameter", 
                        values_to = "estimate")

truePar$source <- "true"

simPar <- pivot_longer(simParams, 
                       cols = c("V", "RU"), 
                       names_to = "parameter", 
                       values_to = "estimate")

simPar$source <- "recovered"

params <- rbind(truePar, simPar)

ggplot(params, aes(estimate, fill = source)) + geom_density(alpha = 0.3)+
  facet_wrap(facets = vars(parameter), scales = "free") # well you can't see shit here

ggplot(params, aes(estimate, fill = source)) + geom_boxplot(alpha = 0.3)+
  facet_grid(cols = vars(parameter), rows = vars(source), scales = "free") # but better but not much

ggplot(params[params$source == "recovered", ], aes(estimate, fill = source)) + geom_density(alpha = 0.3)+
  facet_grid(cols = vars(parameter), scales = "free")
