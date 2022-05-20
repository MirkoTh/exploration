customGreen0 <- "#DeF7E9"
customGreen <- "#71CA97"
customRed <- "#ff7f7f"

format_task_tbl <- function(tbl_tasks) {
  #' format task tbl
  #' 
  #' @description format columns of task tble
  #' @param tbl_tasks \code{tibble} containing the tasks with their features
  #' @return the formatted tbl
  #' 
  tbl_tasks %>%
    mutate(
      nr_arms = fct_inorder(factor(nr_arms), ordered = TRUE),
      nr_samples = fct_inorder(factor(nr_samples), ordered = TRUE)
    )
}


plot_pca <- function(tbl_tasks) {
  #' run and plot pca on task characteristics
  #' 
  #' @description runs and visualizes pca results of tasks varying on several dimensions
  #' @param tbl_tasks \code{tibble} containing the tasks with their features
  #' 
  
  tbl_ivs_coded <- as_tibble(model.matrix(
    task_name ~ confound_rew_inf + nr_arms + add_gen + experience +
      moving_stats, tbl_tasks, 
    contrasts.arg = list(
      confound_rew_inf = "contr.treatment",
      nr_arms = "contr.treatment",
      add_gen = "contr.treatment",
      experience = "contr.treatment",
      moving_stats = "contr.treatment"
    )
  )) %>% select(-`(Intercept)`)
  row.names(tbl_ivs_coded) <- tbl_tasks$task_name
  
  pca_decomp <- prcomp(tbl_ivs_coded, scale. = FALSE)
  
  fviz_pca_biplot(pca_decomp, repel = TRUE,
                  col.var = "#2E9FDF", # Variables color
                  col.ind = "#696969"  # Individuals color
  )
}


plot_labels_task <- function(tbl_tasks) {
  #' plot task labels on capacity and information-reward confound variables
  #' 
  #' @description plot task labels on capacity and information-reward confound variables
  #' @param tbl_tasks \code{tibble} containing the tasks with their features
  #' 
  
  ggplot(tbl_tasks %>% mutate(
    confound_rew_inf = factor(confound_rew_inf, labels = c("No", "Yes"))
  ), aes(confound_rew_inf, capacity)) +
    geom_label_repel(aes(label = task_name, color = capacity)) +
    theme_bw() +
    scale_color_gradient(
      name = "Capacity", low = customGreen, high = customRed
    ) +
    labs(
      x = "Information-Reward Confound",
      y = "Capacity"
    )
}


plot_task_table <- function(tbl_tasks) {
  #' plot formatted task tbl
  #' 
  #' @description plot a formatted tbl of the task tbl
  #' @param tbl_tasks \code{tibble} containing the tasks with their features
  #' 
  
  names(tbl_tasks) <- c(
    "Task Name", "Nr. Arms", "Moving Stats", 
    "Generalization", "Experience", "Capacity", "Info Reward Confound",
    "Nr. Trials"
  )
  tbl_tasks <- tbl_tasks %>% arrange(`Info Reward Confound`, `Capacity`)
  tbl_tasks <- tbl_tasks %>% mutate_if(is.logical, function(x) as.character(x))
  tbl_tasks[tbl_tasks == "FALSE"] <- "No"
  tbl_tasks[tbl_tasks == "TRUE"] <- "Yes"
  tbl_tasks <- tbl_tasks %>% mutate_if(is.character, function(x) as.factor(x))
  
  
  
  formattable(
    tbl_tasks,
    align = c("l","c","c","c","c", "r", "r"), list(
      `Task Name` = formatter(
        "span", style = x ~ style(color = "gray", width = 100),
        x ~ icontext(ifelse(x %in% c("DFD", "DFE Sampling", "DFE"), "star", ""), x)
      ), 
      `Info Reward Confound`= color_tile(customGreen, customRed),
      `Nr. Arms`= color_tile(customGreen0, customGreen),
      `Moving Stats`= color_tile(customGreen0, customGreen),
      `Generalization`= color_tile(customGreen0, customGreen),
      `Experience`= color_tile(customGreen0, customGreen),
      #`Capacity`= color_bar(customRed)
      `Capacity` = formatter(
        "span", style = x ~ style(
          display = "inline-block",
          direction = "ltr",
          color = ifelse(x == 0, "black", "white"),
          "background-color" = csscolor(gradient(x, customGreen, customRed)),
          width = percent(x/3)
        )
      ),
      `Nr. Trials` = color_tile(customGreen0, customGreen)
    )
  )
}

plot_task_table2 <- function(tbl_tasks) {
  #' plot formatted task tbl
  #' 
  #' @description plot a formatted tbl of the task tbl
  #' @param tbl_tasks \code{tibble} containing the tasks with their features
  #' 
  
  names(tbl_tasks) <- c(
    "Task Name", "Bet vs. Total Trials", "Nr. Arms", "Moving Stats",
    "Capacity", "Info Reward Confound"
  )
  tbl_tasks <- tbl_tasks %>% arrange(`Info Reward Confound`, `Capacity`)
  tbl_tasks <- tbl_tasks %>% mutate_if(is.logical, function(x) as.character(x))
  tbl_tasks[tbl_tasks == "FALSE"] <- "no"
  tbl_tasks[tbl_tasks == "TRUE"] <- "yes"
  tbl_tasks <- tbl_tasks %>% mutate_if(is.character, function(x) as.factor(x))
  
  formattable(
    tbl_tasks,
    align = c("l","c","c","c","c", "r"), list(
      `Task Name` = formatter(
        "span", style = x ~ style(color = "gray", width = 100),
        x ~ icontext(ifelse(x %in% c("DFD", "DFE Sampling", "DFE"), "star", ""), x)
      ), 
      `Info Reward Confound`= color_tile(customGreen, customRed),
      `Nr. Arms`= color_tile(customGreen0, customGreen),
      `Moving Stats`= color_tile(customGreen0, customGreen),
      #`Capacity`= color_bar(customRed)
      `Capacity` = formatter(
        "span", style = x ~ style(
          display = "inline-block",
          direction = "ltr",
          color = ifelse(x == 0, "black", "white"),
          "background-color" = csscolor(gradient(x, customGreen, customRed)),
          width = percent(x/3)
        )
      ),
      `Nr. Trials` = color_tile(customGreen0, customGreen)
    )
  )
}

plot_cut_task_table2 <- function(tbl_tasks) {
  #' plot formatted task tbl
  #' 
  #' @description plot a formatted tbl of the task tbl
  #' @param tbl_tasks \code{tibble} containing the tasks with their features
  #' 
  
  names(tbl_tasks) <- c(
    "Task Name", "Bet vs. Total Trials", "Nr. Arms", "Moving Stats",
    "Capacity", "Info Reward Confound"
  )
  tbl_tasks <- tbl_tasks %>% arrange(`Info Reward Confound`, `Capacity`)
  tbl_tasks <- tbl_tasks %>% mutate_if(is.logical, function(x) as.character(x))
  tbl_tasks[tbl_tasks == "FALSE"] <- "no"
  tbl_tasks[tbl_tasks == "TRUE"] <- "yes"
  tbl_tasks <- tbl_tasks %>% mutate_if(is.character, function(x) as.factor(x))
  
  formattable(
    tbl_tasks %>% select(-c(Capacity, `Info Reward Confound`)),
    align = c("l","c","c","r"), list(
      `Task Name` = formatter(
        "span", style = x ~ style(color = "gray", width = 100),
        x ~ icontext(ifelse(x %in% c("DFD", "DFE Sampling", "DFE"), "star", ""), x)
      ), 
      `Nr. Arms`= color_tile(customGreen0, customGreen),
      `Moving Stats`= color_tile(customGreen0, customGreen),
      #`Capacity`= color_bar(customRed),
      `Nr. Trials` = color_tile(customGreen0, customGreen)
    )
  )
}


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


rl_softmax_sim <- function(rewards, sigma_epsilon_sq, m0, params) {
  nt <- nrow(rewards) # number of time points
  no <- ncol(rewards) # number of options
  m <- matrix(m0,ncol=no,nrow=nt+1) # to hold the posterior means
  v <- matrix(sigma_epsilon_sq,ncol=no,nrow=nt+1) # to hold the posterior variances
  choice <- rep(0,nt) # to hold the choices of the RL agent
  reward <- rep(0.0,nt) # to hold the obtained rewards by the RL agent
  # loop over all time points
  for(t in 1:nt) {
    # use the prior means and compute the probability of choosing each option
    tb_exp <- params$gamma*m[t,]
    p <- exp(ifelse(tb_exp > 700, 700, tb_exp))
    p <- p/sum(p)
    # choose an option according to these probabilities
    choice[t] <- sample(1:no,size=1,prob=p)
    # get the reward of the choice
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
