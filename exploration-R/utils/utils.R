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


conditions_and_rewards_fixed_means <- function(
    n_conditions, mn_spacing, sd_fixed, n_trials
) {
  #' create tbl with info about conditions and rewards per trial
  #' 
  #' @description creates a tbl with cols condition_id, n_options
  #' and list cols reward_stats (mns and sds) and rewards (by-trial rewards)
  #' @param n_conditions nr of different experimental conditions
  #' @param mn_spacing constant spacing between means
  #' @param sd_fixed sd for all conditions and options
  #' @param n_trials nr trials of the experiment
  #' @return the tbl with all information
  
  tbl_conditions <- tibble(
    condition_id = seq(1, n_conditions, by = 1),
    n_options = seq(2, 2*n_conditions, by = 2),
    reward_stats = map(
      n_options, ~ list(
        reward_mn = seq(mn_spacing, .x*mn_spacing, by = mn_spacing),
        reward_sd = rep(sd_fixed, .x)
      )
    )
  )
  
  my_rnorm <- function(mn, sd, n) {
    rnorm(n, mn, sd)
  }
  
  l_out <- map(1:nrow(tbl_conditions), ~pmap(list(
    tbl_conditions$reward_stats[[.x]]$reward_mn,
    tbl_conditions$reward_stats[[.x]]$reward_sd
  ), ~ my_rnorm(n = n_trials, mn = ..1, sd = ..2)
  ))
  
  tbl_conditions$rewards <- l_out
  return(tbl_conditions)
}

update_kalman_filter <- function(var_prev, var_innov, var_error) {
  kg <- (var_prev + var_innov) / (var_prev + var_innov + var_error)
  var_new <- (1 - kg) * (var_prev + var_innov)
  l_params_updated <- list(kg = kg, var_new = var_new)
  return(l_params_updated)
}


rl_softmax_sim <- function(rewards,m0,v0,sigma_xi_sq,sigma_epsilon_sq,gamma) {
  nt <- nrow(rewards) # number of time points
  no <- ncol(rewards) # number of options
  m <- matrix(m0,ncol=no,nrow=nt+1) # to hold the posterior means
  v <- matrix(v0,ncol=no,nrow=nt+1) # to hold the posterior variances
  choice <- rep(0,nt) # to hold the choices of the RL agent
  reward <- rep(0.0,nt) # to hold the obtained rewards by the RL agent
  # loop over all time points
  for(t in 1:nt) {
    # use the prior means and compute the probability of choosing each option
    p <- exp(gamma*m[t,])
    p <- p/sum(p)
    # choose an option according to these probabilities
    choice[t] <- sample(1:4,size=1,prob=p)
    # get the reward of the choice
    reward[t] <- rewards[t,choice[t]]
    # set the Kalman gain for unchosen options
    kt <- rep(0,4)
    # set the Kalman gain for the chosen option
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    # compute the posterior means
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    # compute the posterior variances
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
  }
  # return everything of interest
  return(list(m=m,v=v,choice=choice,reward=reward))
}
