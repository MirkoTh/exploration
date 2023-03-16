library(tidyverse)
library(rutils)
library(grid)
library(gridExtra)
library(mvtnorm)
library(zoo)
library(TTR)
library(future)
library(furrr)


home_grown <- c("exploration-R/utils/utils.R", "exploration-R/utils/plotting.R")
walk(home_grown, source)



# Generate Random Walk Data -----------------------------------------------


mu1 <- c(-60, -20, 20, 60)
nr_trials <- 400
sigma_xi_sq <- 16
sigma_epsilon_sq <- 16
lambda <- .9836

generate_restless_bandits <- function(sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials) {
  nr_bandits <- length(mu1)
  mus <- matrix(nrow = nr_trials, ncol = nr_bandits)
  mus[1, ] <- mu1
  for (t in 2:nr_trials) {
    mus[t, ] <- lambda * mus[t-1, ] + rnorm(nr_bandits, 0, sqrt(sigma_xi_sq))
  }
  noise <- matrix(
    rnorm(nr_trials * nr_bandits, 0, sqrt(sigma_epsilon_sq)),
    nrow = nr_trials, ncol = nr_bandits
  )
  as_tibble(as.data.frame(mus + noise)) %>% 
    mutate(trial_id = 1:nr_trials) %>%
    rename("Bandit 1" = V1, "Bandit 2" = V2, "Bandit 3" = V3, "Bandit 4" = V4)
}

tbl_bandits <- generate_restless_bandits(sigma_xi_sq, sigma_epsilon_sq, mu1, lambda, nr_trials)

ggplot(tbl_bandits %>% pivot_longer(-trial_id), aes(trial_id, value, group = name)) +
  geom_line(aes(color = name), size = .75) +
  scale_color_viridis_d(name = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Trial ID", y = "Reward")



# Generate Choices Softmax --------------------------------------


simulate_kalman_learning <- function(sigma_prior, mu_prior, sigma_xi_sq, sigma_epsilon_sq, gamma, tbl_rewards, seed = 4321) {
  #' 
  #' @description simulate choices from a Kalman filter with soft max choice model
  #' @param sigma_prior prior variance
  #' @param mu_prior prior mean
  #' @param sigma_xi_sq innovation variance
  #' @param sigma_epsilon_sq error variance
  #' @param gamma softmax inverse temperature
  #' @param tbl_rewards rewards to be selected on all trials
  #' @return a tbl with by-trial posterior means and variances for the chosen bandits
  
  set.seed(seed)
  nt <- nrow(tbl_rewards) # number of time points
  no <- ncol(tbl_rewards) # number of options
  m <- matrix(mu_prior, ncol = no, nrow = nt + 1) # to hold the posterior means
  v <- matrix(sigma_prior, ncol = no, nrow = nt + 1) # to hold the posterior variances
  choices <- rep(0, nt + 1) # to hold the choices of the RL agent
  rewards <- rep(0.0, nt + 1) # to hold the obtained rewards by the RL agent
  
  for(t in 1:nt) {
    p <- softmax_choice_prob(matrix(m[t, ], 1, ncol(m)), gamma)
    # choose an option according to these probabilities
    choices[t] <- sample(1:4, size = 1, prob = p)
    # get the reward of the choice
    rewards[t] <- tbl_rewards[t, choices[t]] %>% as_vector()
    kt <- rep(0, no)
    # set the Kalman gain for the chosen option
    kt[choices[t]] <- (v[t,choices[t]] + sigma_xi_sq)/(v[t,choices[t]] + sigma_epsilon_sq + sigma_xi_sq)
    # compute the posterior means
    m[t + 1, ] <- m[t, ] + kt * (tbl_rewards[t, ] - m[t, ]) %>% as_vector()
    # compute the posterior variances
    v[t + 1, ] <- (1 - kt) * (v[t, ])
  }
  
  tbl_m <- as.data.frame(m)
  # constrain v from becoming to small
  v <- t(apply(v, 1, function(x) pmax(x, .0001)))
  tbl_v <- as.data.frame(v)
  colnames(tbl_m) <- str_c("m_", 1:no)
  colnames(tbl_v) <- str_c("v_", 1:no)
  tbl_return <- tibble(cbind(tbl_m, tbl_v, choices, rewards))
  
  return(tbl_return)
}

softmax_choice_prob <- function(ms, gamma) {
  #' 
  #' @description soft max choice rule
  #' @param ms posterior means of the bandits
  #' @param gamma inverse temperature parameter
  #' @return a tbl with by-trial posterior means and variances for the chosen bandits
  prob <- exp(gamma * ms)
  prob <- prob / rowSums(prob)
  return(prob)
}


fit_kalman_softmax <- function(x, tbl_results, nr_options) {
  #' 
  #' @description kalman softmax fitting wrapper
  #' 
  sigma_xi_sq <- exp(x[[1]])
  sigma_epsilon_sq <- exp(x[[2]])
  gamma <- exp(x[[3]])
  tbl_learned <- kalman_learning(tbl_results, nr_options, sigma_xi_sq, sigma_epsilon_sq)
  p_choices <- softmax_choice_prob(
    tbl_learned[1:nrow(tbl_results), ] %>% select(starts_with("m_")), 
    gamma
  )
  lik <- pmap_dbl(tibble(cbind(p_choices, tbl_results$choices)), ~ c(..1, ..2, ..3, ..4)[..5])
  llik <- log(lik)
  sllik <- sum(llik)
  return(-sllik)
}



n_participants <- 100
sigma_xi_sq <- rnorm(n_participants, 16, 3)
sigma_epsilon_sq <- rnorm(n_participants, 16, 3)
s_gamma <- -1
while(s_gamma < 0){
  gamma <- rnorm(n_participants, 1, .25)
  s_gamma <- min(gamma)
}

tbl_params_softmax <- tibble(
  sigma_prior = rep(100, n_participants),
  mu_prior = rep(0, n_participants),
  sigma_xi_sq,
  sigma_epsilon_sq,
  gamma
)

plan(multisession, workers = availableCores() - 2)
l_choices_simulated <- future_pmap(
  tbl_params_softmax, simulate_kalman_learning, 
  tbl_rewards = tbl_rewards, .progress = TRUE, 
  .options = furrr_options(seed = NULL)
)


fit_softmax_wrapper <- function(tbl_results) {
  tbl_results <- tbl_results[1:(nrow(tbl_results) - 1), ]
  params_init <- c(log(13), log(10), log(.8))
  result_optim <- optim(params_init, fit_kalman_softmax, tbl_results = tbl_results, nr_options = 4)
  exp(result_optim$par)
}

l_softmax <- future_map(
  l_choices_simulated, 
  safely(fit_softmax_wrapper), .progress = TRUE, 
  .options = furrr_options(seed = NULL)
  )

tbl_results_softmax <- as.data.frame(reduce(map(l_softmax, "result"), rbind)) %>% as_tibble()
colnames(tbl_results_softmax) <- c("sigma_xi_sq_ml", "sigma_epsilon_sq_ml", "gamma_ml")
tbl_results_softmax <- as_tibble(cbind(tbl_params_softmax, tbl_results_softmax)) %>%
  mutate(participant_id = 1:nrow(tbl_results_softmax))

cor(tbl_results_softmax$sigma_xi_sq, tbl_results_softmax$sigma_xi_sq_ml)
# [1] 0.3901223 with 200 trials and 100 participants
ggplot(tbl_results_softmax, aes(sigma_xi_sq, sigma_xi_sq_ml)) +
  geom_point()

cor(tbl_results_softmax$sigma_epsilon_sq, tbl_results_softmax$sigma_epsilon_sq_ml)
ggplot(tbl_results_softmax, aes(sigma_epsilon_sq, sigma_epsilon_sq_ml)) +
  geom_point()

cor(tbl_results_softmax$gamma, tbl_results_softmax$gamma_ml)
ggplot(tbl_results_softmax, aes(gamma, gamma_ml)) +
  geom_point()


upper_and_lower_bounds <- function(par, lo, hi) {
  log(((par - lo) / (hi - lo)) / (1 - (par - lo) / (hi - lo)))
}

upper_and_lower_bounds_revert <- function(par, lo, hi) {
  lo + ((hi - lo) / (1 + exp(-par)))
}


