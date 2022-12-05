make_conditions_and_rewards <- function(
    opts, mn_spacing, vars, n_trials
) {
  #' create tbl with info about conditions and rewards per trial
  #' 
  #' @description creates a tbl with cols condition_id, n_options
  #' and list cols reward_stats (mns and sds) and rewards (by-trial rewards)
  #' @param opts vector with number of response options across conditions
  #' @param mn_spacing constant spacing between means
  #' @param vars variances of the data
  #' @param n_trials nr trials of the experiment
  #' @return the tbl with all information
  
  tbl_conditions <- crossing(opts, mn_spacing, vars)
  colnames(tbl_conditions) <- c("n_options", "mn_spacing", "var")
  tbl_conditions$reward_stats = pmap(
    tbl_conditions, ~ list(
      reward_mn = seq(..2, ..1*..2, by = ..2),
      reward_var = rep(..3, ..1)
    )
  )
  
  my_rnorm <- function(mn, sd, n) {
    rnorm(n, mn, sd)
  }
  
  l_out <- map(1:nrow(tbl_conditions), ~pmap(list(
    tbl_conditions$reward_stats[[.x]]$reward_mn,
    tbl_conditions$reward_stats[[.x]]$reward_var
  ), ~ my_rnorm(n = n_trials, mn = ..1, sd = sqrt(..2))
  ))
  
  # rewards per trial as matrix
  tbl_conditions$rewards <- l_out %>% 
    map(~ unlist(.x) %>% matrix(nrow = length(.x), byrow = TRUE)) %>% 
    map(~ t(.x))
  
  return(tbl_conditions)
}


update_kalman_filter <- function(var_prev, var_innov, var_error) {
  kg <- (var_prev + var_innov) / (var_prev + var_innov + var_error)
  var_new <- (1 - kg) * (var_prev + var_innov)
  l_params_updated <- list(kg = kg, var_new = var_new)
  return(l_params_updated)
}


rl_softmax_sim <- function(rewards, sigma_epsilon_sq, m0, params, choices = NULL) {
  nt <- nrow(rewards) # number of time points
  no <- ncol(rewards) # number of options
  m <- matrix(m0,ncol=no,nrow=nt+1) # to hold the posterior means
  v <- matrix(sigma_epsilon_sq,ncol=no,nrow=nt+1) # to hold the posterior variances
  choice <- rep(0,nt) # to hold the choices of the RL agent
  reward <- rep(0.0,nt) # to hold the obtained rewards by the RL agent
  # loop over all time points
  for(t in 1:nt) {
    if (is.null(choices)) {
      # use the prior means and compute the probability of choosing each option
      tb_exp <- params$gamma*m[t,]
      p <- exp(ifelse(tb_exp > 700, 700, tb_exp))
      p <- p/sum(p)
      # choose an option according to these probabilities
      choice[t] <- sample(1:no,size=1,prob=p)
      # get the reward of the choice
    } else {
      choice <- choices
    }
    
    reward[t] <- rewards[t,choice[t]]
    
    if (params$model == "Kalman") {
      # set the Kalman gain for not chosen options
      kt <- rep(0,no)
      # set the Kalman gain for the chosen option
      kt[choice[t]] <- (v[t,choice[t]])/(v[t,choice[t]] + sigma_epsilon_sq)
      # compute the posterior means
      m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
      # compute the posterior variances
      v[t+1,] <- (1-kt)*(v[t,])
      
    } else if (params$model == "Decay") {
      m[t+1, ] <- params$eta * m[t, ]
      m[t+1, choice[t]] <- m[t+1, choice[t]] + reward[t]
      v[t+1, choice[t]] <- 0
      
    }
    
  }
  # return everything of interest
  return(list(m=m,v=v,choice=choice,reward=reward))
}



total_rewards <- function(model, gamma, eta) {
  #' helper function to compute total rewards for given gamma
  #' @return the tbl with all information
  
  l_params <- list(model = model, gamma = gamma, eta = eta)
  l_l_m <- pmap(
    list(tbl_conditions$rewards, as.list(tbl_conditions$var)),
    rl_softmax_sim,
    m0 = 0, params = l_params
  )
  map_dbl(l_l_m, ~ sum(.x[["reward"]]))
  
}


iterate_once <- function(x) {
  #' iterate over gamma values for each condition
  #' 
  #' @description iterate once over a sequence of gamma values and return total_rewards
  #' once for each condition in tbl_conditions
  #' @param x dummy param to iterate over
  #' @return the results tbl
  
  tbl_params <- params_grid()
  tbl_conditions <- make_conditions_and_rewards(opts, mn_spacing, vars, n_trials)
  l_results <- pmap(tbl_params, total_rewards)
  tbl_results <- l_results %>% reduce(rbind) %>% as_tibble(.name_repair = "unique")
  colnames(tbl_results) <- interaction(tbl_conditions$n_options, tbl_conditions$var)
  tbl_results <- cbind(tbl_results, tbl_params)
  tbl_results <- tbl_results %>% 
    pivot_longer(cols = -colnames(tbl_params), names_to = "noxvar", values_to = "reward_tot") %>%
    mutate(
      n_options = str_extract(noxvar, "^([0-9]+)"),
      var = str_extract(noxvar, "([0-9]+)$")
    )
  return(tbl_results)
}


params_grid <- function() {
  #' create grid of parameter combinations to iterate over
  #' 
  #' @description crosses different different parameters for each model
  #' and combines them in one tbl
  #' @return tbl with parameter combinations
  
  gamma <- seq(.1, 5, by = .1) # temperature of softmax
  eta <- seq(.2, 1, by = .2) # decay rate in decay rule
  model <- "Decay"
  tbl_params_decay <- crossing(model, gamma, eta)
  eta <- 0
  model <- "Kalman"
  tbl_params_kalman <- crossing(model, gamma, eta)
  tbl_params <- rbind(tbl_params_decay, tbl_params_kalman)
  return(tbl_params)
}


make_condition_trials <- function(n_options, params_fixed, condition_distinct_ii) {
  #' make forced-choice trials for all conditions
  #' 
  #' @description makes a tbl for the forced-choice trials
  #' using desired parameter settings
  #' @param n_options number of response options
  #' @param params_fixed list with simulation parameters
  #' @param condition_distinct_ii vector with most distinctive response option 1
  #' @return tbl with separate row for each forced-choice trial
  #'
  my_rnorm <- function(m, sd) rnorm(params_fixed$n_trials, m, sd)
  l_rewards <- pmap(list(m = params_fixed$v_means, sd = params_fixed$v_sd), my_rnorm)
  tbl_prep <- tibble(
    trial_id = rep(
      seq(1, params_fixed$n_trials, by = 1), length(params_fixed$conditions)),
    condition = rep(params_fixed$conditions, each = params_fixed$n_trials),
    setsize = n_options,
    option_selected = factor(c(
      rep(1:n_options, each = params_fixed$n_trials/n_options),
      rep(1:n_options, params_fixed$n_trials/n_options),
      condition_distinct_ii
    )),
    value_sampled = NA,
    #value_sampled = rnorm(params_fixed$v_means[option_selected], params_fixed$v_sd),
    distance_t = abs(params_fixed$n_trials - trial_id) + params_fixed$ri,
    distance_t_log = log(distance_t)
  )
  extract_nested_sample <- function(idx_l, idx_pos) {
    l_rewards[[idx_l]][idx_pos]
  }
  tbl_prep$value_sampled <- pmap(
    tbl_prep[, c("option_selected", "trial_id")], ~ extract_nested_sample(.x, .y)
    ) %>% unlist()
  return(list(tbl_prep, l_rewards))
}

calc_discriminability <- function(tb, c, alpha) {
  #' SIMPLE discriminability
  #' 
  #' @description calculates discriminability of presented items
  #' given temporal structure using SIMPLE formulae
  #' @param tbl for one of the three conditions with by-trial info
  #' @param c SIMPLE parameter: scaling of exponential function
  #' @param alpha SIMPLE parameter: 1 = exponential, 2 = Gaussian
  #' @return list with all discriminabilities
  #' 
  distances_all <- tb$distance_t_log
  # map over all distances besides n
  all_positions <- seq(1, length(distances_all), by = 1)
  f_distinct <- function(n) 1/sum(map_dbl(distances_all, ~ exp(-c*abs(distances_all[n] - .x)^alpha)))
  map_dbl(all_positions, f_distinct)
}

kalman_forced_choice <- function(params_fixed, l_tbl_rewards){
  #' update Kalman filter using forced-choices
  #' 
  #' @description update Kalman filter over n trials using forced choices
  #' from tbl with by-trial info
  #' @param params_fixed list with simulation parameters
  #' @l_tbl_rewards nested list with reward matrix
  #' and tbl with by-trial info about choices and rewards
  #' @return nested list containing list with Kalman 
  #' results (i.e., ms, sds, choices, rewards)
  #' 

  # reward matrix
  rewards <- l_tbl_rewards[[2]]
  # tbl with by-trial info
  tbl_2 <- l_tbl_rewards[[1]]
  m_rewards_2 <- matrix(
    unlist(rewards)[1:(2*params_fixed$n_trials)], 
    nrow = params_fixed$n_trials, ncol = 2, byrow = FALSE
  )
  l_options_selected <- list(
    tbl_2$option_selected[seq(1, params_fixed$n_trials, by = 1)],
    tbl_2$option_selected[seq(params_fixed$n_trials + 1, 2 * params_fixed$n_trials, by = 1)],
    tbl_2$option_selected[seq(params_fixed$n_trials * 2 + 1, 3 * params_fixed$n_trials, by = 1)]
  )
  
  rl_softmax_sim_wrap <- function(choices, m_rew, sd, m, pars) {
    rl_softmax_sim(m_rew, sd, m, pars, choices)
  }
  
  map(
    l_options_selected, rl_softmax_sim_wrap,
    m_rewards_2, params_fixed$v_sd, 0, params_fixed
  )
}


# two helper functions to calculate density above zero after each choice
cum_density_above_zero <- function(i, l) {
 
  #' @description calculate density of difference of posterior distributions above zero
  #' 
  
  m <- l$m[i, 1] + l$m[i, 2]
  sd <- sqrt(l$v[i, 1] + l$v[i, 2])
  pnorm(0, m, sd)
}

wrap_all_densities <- function(l) {
  map_dbl(1:nrow(l$m), cum_density_above_zero, l = l)
}


agg_runs <-function(condition_idx, l_results, params_fixed) {
  #' aggregate simulation runs by condition and trial
  #' 
  #' @description calculate average proportion people should
  #' choose the option with the higher mean
  #' @param condition_idx index stating which condition should be aggregated
  #' @param l_results list with simulation results
  #' @param params_fixed list with simulation parameters
  #' @return tbl grouped by condition and trial
  
  map(l_results, condition_idx) %>% 
    unlist() %>%
    matrix(nrow = 13, ncol = n_runs, byrow = FALSE) %>%
    as.data.frame() %>%
    mutate(
      trial_id = seq(1, nrow(.)),
      condition = params_fixed$conditions[condition_idx],
    ) %>%
    pivot_longer(cols = starts_with("V")) %>%
    # group_by(condition, trial_id) %>%
    # summarize(
    #   val_mean = mean(value),
    #   val_sd = sd(value)
    # ) %>%
    ungroup()
} 



sample_y <- function(subj, mu_t1, mu_t2, var, n) {
  #' sample t1 and t2 values for one subject given means and error variance
  #' 
  #' @description sample t1 and t2 values from normal distributions
  #' for one subject given means and error variance; assemble everything in tibble
  #' @param subj subject id
  #' @param mu_t1 mean at t1
  #' @param mu_t2 mean at t2
  #' @param var error variance
  #' @param n number of samples
  #' @return the tbl with columns subject, val t1, and val t2

  y_t1 <- rnorm(n, mu_t1, sqrt(var))
  y_t2 <- rnorm(n, mu_t2, sqrt(var))
  tibble(
    subject = subj,
    t1 = y_t1,
    t2 = y_t2
  )
}

reliability_pipeline <- function(n_subjects, n_trials, reliability) {
  #' run reliability pipeline of Bayesian model once
  #' 
  #' @description create a data set with given number of subjects, trials, 
  #' and reliability and run Bayesian model on that data set
  #' @param n_subjects number of subjects in simulated data set
  #' @param n_trials number of subjects in simulated data set
  #' @param reliability reliability in simulated data set
  #' @return a summary tbl with means and ses of posterior distributionssz
  
  # set up data set ---------------------------------------------------------
  
  n_timepoints <- 2
  d_r <- 1.5
  a_r <- c(0 - d_r/2, 0 + d_r/2) # fixed effect over time
  
  sig_sq_0 <- 1 # error variance
  var_subj_t1 <- .5
  var_subj_t2 <- .5
  cov_t1_t2 <- reliability * sqrt(var_subj_t1) * sqrt(var_subj_t2)
  
  mu_subj <- c(0, 0)
  R_bold <- matrix(c(var_subj_t1, cov_t1_t2, cov_t1_t2, var_subj_t2), nrow = 2) # vcov matrix of subject level variability
  
  tau_s <- MASS::mvrnorm(n_subjects, mu_subj, R_bold)
  
  mu_rs <- matrix(rep(a_r, each = n_subjects), ncol = 2) + tau_s
  tbl_sample <- tibble(
    subj = 1:nrow(mu_rs),
    mu_t1 = mu_rs[, 1],
    mu_t2 = mu_rs[, 2]
  )
  
  tbl_sim <- pmap(tbl_sample, sample_y, var = sig_sq_0, n = n_trials) %>% reduce(rbind)
  tbl_sim_long <- pivot_longer(tbl_sim, c(t1, t2), names_to = "timepoint", values_to = "y")
  
  
  # fit Bayesian reliability model ------------------------------------------
  
  
  stan_normal_rel <- stan_normal_reliability()
  mod_normal_rel <- cmdstan_model(stan_normal_rel)
  
  x <- tbl_sim_long$timepoint %>% as.factor() %>% as.numeric()
  
  l_data <- list(
    n_data = nrow(tbl_sim_long),
    n_subj = length(unique(tbl_sim_long$subject)),
    subj = as.numeric(factor(
      tbl_sim_long$subject, 
      labels = 1:length(unique(tbl_sim_long$subject))
    )),
    x = x,
    response = tbl_sim_long$y
  )
  
  fit_normal_rel <- mod_normal_rel$sample(
    data = l_data, iter_sampling = 2000, iter_warmup = 1000, chains = 1
  )
  
  pars_interest <- c("mu_ic", "mu_time", "Sigma")
  tbl_draws <- fit_normal_rel$draws(variables = pars_interest, format = "df")
  tbl_summary <- fit_normal_rel$summary(variables = pars_interest)
  
  tbl_posterior <- tbl_draws %>% 
    dplyr::select(starts_with(c("mu", "Sigma[2,1]")), .chain) %>%
    rename(chain = .chain) %>%
    pivot_longer(starts_with(c("mu", "Sigma[2,1]")), names_to = "parameter", values_to = "value") %>%
    mutate(parameter = factor(parameter, labels = c("Intercept", "Time", "Reliability")))
  
  
  tbl_descriptive <- grouped_agg(tbl_posterior, parameter, value)
  
  return(tbl_descriptive)
}