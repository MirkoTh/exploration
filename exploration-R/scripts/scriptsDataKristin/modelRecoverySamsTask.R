############### model recovery Sam's task #################

################# model recovery horizon task ###############################

setwd("/Users/kwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/FanEtAl")
library(ggplot2)
theme_set(theme_classic(base_size = 15))
library(lme4)
library(plyr)
library(brms)
library(tidyr)

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

data <- read.csv("exp1_bandit_task_scale.csv")

set.seed(123)


################# Kalman filter learning model ##############

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





####################### fit the models & simulate data & get GoF #####################


## for improved comparability I will now also use a simple logistic regression separately for each sub
models = list(model = c("Sam", "UCB", "Thompson", "Softmax"),
              eq = c(as.formula("C ~ V_old + RU_old + VTU_old"),
                     as.formula("C ~ V_old + RU_old"),
                     as.formula("C ~ VTU_old"),
                     as.formula("C ~ V_old")))

AICs <- data.frame(ID = rep(unique(data$sub), length(models$model)^2),
                   AIC = rep(NA, length(unique(data$sub))*length(models$model)^2),
                   GenModel = rep(models$model, each = length(unique(data$sub))*length(models$model)),
                   FitModel = rep(rep(models$model, each = length(unique(data$sub))), length(models$model)),
                   converged = NA,
                   best = 0)

blocks <- max(data$block)
trials <- max(data$trial)
nsubs <- 1 # bc we do 1 subject at a time

data$VTU_old <- data$V_old/data$TU_old

for (i in unique(data$sub)){
  print(i)
  for(genModel in models$model){
    formula <- models$eq[models$model == genModel][[1]]
    trueModel <- glm(formula,
                     data = data[data$sub == i, ],
                     family = binomial(link = "probit"))
    
    
    # simulate data
    simdat <- subset(data, sub == i, -c(C, V, RU, VTU, est_m1, est_m2, est_s1, est_s2, reward))
    
    
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
    

    simdat$C <- as.integer(simdat$C)
    
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
    
    # indicate which one is best for that sub and that simulated dataset
    best = min(AICs$AIC[AICs$ID == i & AICs$GenModel == genModel])
    AICs$best[AICs$ID == i & AICs$GenModel == genModel & AICs$AIC == best] <- 1
    
    
  }
  
  
  
}


# how many converged?
table(AICs$converged) # 43 did not converge


# get proportion of subs for which a given model was best given which model the data was simulated from
bestModels <- ddply(AICs[AICs$converged, ], ~GenModel+FitModel, summarise, Pbest = mean(best))


# plot results

library(ggplot2)

ggplot(bestModels, aes(x = GenModel, y = FitModel, fill = Pbest)) + geom_raster() + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(Pbest, digits = 2)))

bestModels$Pbest[bestModels$GenModel == bestModels$FitModel]
