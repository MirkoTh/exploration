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
      choice = choices
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
  #' @return list with all discriminabilities
  #' 
  distances_all <- tb$distance_t_log
  # map over all distances besides n
  all_positions <- seq(1, length(distances_all), by = 1)
  f_distinct <- function(n) 1/sum(map_dbl(distances_all, ~ exp(-c*abs(distances_all[n] - .x)^alpha)))
  map_dbl(all_positions, f_distinct)
}