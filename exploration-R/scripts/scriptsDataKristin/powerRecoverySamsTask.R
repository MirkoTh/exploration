########################## power and recovery of UCB model in Sam's task with different trial numbers #########################


setwd("/Users/kwitte/Library/CloudStorage/OneDrive-Personal/CPI/ExplorationReview/PrelimReliabilities/FanEtAl")
library(ggplot2)
theme_set(theme_classic(base_size = 15))
library(lme4)
library(dplyr)
library(brms)
library(tidyr)
set.seed(123)

se<-function(x){sd(x, na.rm = T)/sqrt(length(na.omit(x)))}
meann <- function(x){mean(x, na.rm = T)}

data <- read.csv("exp1_bandit_task_scale.csv")

# the way the choice is coded is confusing and I'll just reverse it at the start to be 0 = left, 1  = right

data$C <- ifelse(data$C == 0, 1, 0)

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

################## settings ###################

nsubs <- 200
subsIncl <- sample(unique(data$sub), nsubs, replace = F)
dat <- data[is.element(data$sub, subsIncl), ]

ntrials <- c(8,10,12,14,16,18,20)
nblocks <- c(20,25,30,35,40,45)

################# recovery and power analysis #################

## create dfs

recovery <- data.frame(trueParam = rep(c("V", "RU"), 2*length(ntrials)*length(nblocks)),
                       simParam = rep(rep(c("V", "RU"), each = 2), length(ntrials) * length(nblocks)),
                       ntrials = rep(ntrials, each = 2*2*length(nblocks)),
                       nblocks = rep(rep(nblocks, each = 2*2),length(ntrials)),
                       cor = NA)


power <- data.frame(trueParam = rep(c("V", "RU"), length(ntrials)*length(nblocks)),
                    ntrials = rep(ntrials, each = 2*length(nblocks)),
                    nblocks = rep(rep(nblocks, each = 2),length(ntrials)),
                    sig = NA)


## get true parameters for each included subject to simulate from 
# Problem: using unscaled estimates bc otherwise prediction is weird BUT in simModel need to standardise otherwise it doesnt converge

trueModel <- glmer(C ~ V_old+ RU_old + (V_old + RU_old | sub),
                   data = dat,
                   family = binomial(link = "probit"))



############### save true params
ranefs <- as.data.frame(coef(trueModel)$sub)

trueParams <- data.frame(ID = unique(dat$sub),
                         V = ranefs$V,
                         RU = ranefs$RU)

############ initialise array for simulated parameter estimates
simParams <- data.frame(ID = unique(dat$sub),
                        V = rep(NA, nsubs),
                        RU = rep(NA, nsubs))



for (trials in ntrials){
  for (blocks in nblocks){
    print(sprintf("%f trials, %f blocks", trials, blocks))
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
        
        
        
      }
  
    
    
  
  
  }
  



############## plot it #############

ggplot(recovery, aes(x = trueParam, y = simParam, fill = cor)) + geom_raster() + scale_fill_gradient2(low = "red", mid = "white", high = "blue")+
  geom_text(aes(label = round(cor, digits = 2))) +
  facet_grid(rows = vars(ntrials), cols = vars(nblocks))

