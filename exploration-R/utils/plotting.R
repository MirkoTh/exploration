library(grid)

check_reward_distributions <- function(tbl_conditions) {
  #' violin plots of simulated rewards
  #'
  #' @description violin plots of simulated rewards faceted by condition
  #' @param tbl_conditions tibble with information and rewards
  #' @return nothing; just plots

  tbl_rewards <- pmap(
    list(
      tbl_conditions$rewards,
      as.list(tbl_conditions$var),
      as.list(tbl_conditions$n_options)
    ),
    ~ as.data.frame(..1) %>% mutate(var = ..2, n_arms = ..3)
  ) %>%
    map(~ pivot_longer(.x, -c(var, n_arms))) %>%
    reduce(rbind) %>%
    mutate(name = substr(name, 2, 5))
  tbl_rewards <- tbl_rewards %>%
    mutate(
      var = str_c("Var = ", var),
      n_arms = str_c("Nr. Options = ", n_arms)
    )

  ggplot(tbl_rewards, aes(name, value, group = name)) +
    geom_violin(draw_quantiles = .5, aes(fill = name)) +
    scale_fill_brewer(name = "Option", palette = "Set1") +
    facet_grid(var ~ n_arms) +
    coord_flip() +
    theme_bw() +
    labs(x = "Option", y = "Reward")
}

plot_mean_trajectories <- function(l_l_m, tbl_conditions) {
  #' plot estimated mean trajectories of response options
  #'
  #' @description using one walk through with the rl model
  #' @param l_l_m nested list with modeling results in inner lists
  #' @return nothing; just plots


  format_means <- function(l_m, var) {
    l_m$m %>%
      as.data.frame() %>%
      as_tibble() %>%
      rename_all(~ substr(.x, 2, 4)) %>%
      mutate(
        trial_id = 1:nrow(l_m$m),
        var = str_c("Var = ", var)
      ) %>%
      pivot_longer(cols = -c(trial_id, var)) %>%
      mutate(n_options = str_c("Nr. Options = ", length(unique(name))))
  }
  pmap(list(l_l_m, as.list(tbl_conditions$var)), format_means) %>%
    reduce(rbind) %>%
    ggplot(aes(trial_id, value, group = name)) +
    geom_line(aes(color = name)) +
    geom_point(color = "white", size = 3) +
    geom_point(aes(color = name), shape = 1) +
    facet_grid(var ~ n_options) +
    scale_color_brewer(name = "Option", palette = "Set1") +
    theme_bw() +
    labs(
      x = "Trial ID",
      y = "Estimated Mean"
    )
}


plot_total_rewards_kalman <- function(tbl_results_agg) {
  var_min <- min(as.numeric(tbl_results_agg$var))
  tbl_plot <- tbl_results_agg %>%
    filter(model == "Kalman") %>%
    mutate(var = factor(str_c("Var = ", var)))
  tbl_plot$var <- tbl_plot$var %>% relevel(str_c("Var = ", var_min))
  ggplot(tbl_plot, aes(gamma, mean_reward_tot, group = n_options)) +
    geom_errorbar(aes(
      ymin = mean_reward_tot - se_reward_tot * 1.96,
      ymax = mean_reward_tot + se_reward_tot * 1.96,
      color = n_options
    )) +
    geom_line(aes(color = n_options)) +
    geom_point(color = "white", size = 3) +
    geom_point(aes(color = n_options)) +
    facet_wrap(~var) +
    theme_bw() +
    scale_color_brewer(name = "Nr. Options", palette = "Set1") +
    labs(x = "Gamma", y = "Total Reward")
}

plot_total_rewards_decay <- function(tbl_results_agg, p_eta = "all") {
  var_min <- min(as.numeric(tbl_results_agg$var))
  tbl_plot <- tbl_results_agg %>%
    filter(model == "Decay") %>%
    group_by(n_options) %>%
    mutate(
      mean_reward_prop = mean_reward_tot / max(mean_reward_tot),
      var = factor(str_c("Var = ", var)),
      n_options = str_c("Nr. Options = ", n_options)
    ) %>%
    ungroup()
  tbl_plot$var <- tbl_plot$var %>% relevel(str_c("Var = ", var_min))

  if (p_eta == "all") {
    ggplot(tbl_plot, aes(gamma, eta)) +
      geom_raster(aes(fill = mean_reward_prop)) +
      # geom_text(aes(
      #   label = ifelse(mean_reward_prop >= .995, 1, substr(round(mean_reward_prop, 2), 2, 4)),
      #   color = mean_reward_prop
      #   )) +
      facet_wrap(n_options ~ var, ncol = 2) +
      theme_bw() +
      scale_fill_viridis_c(name = "Proportionate\nReward") +
      scale_color_gradient(low = "white", high = "black", guide = "none") +
      labs(x = "Gamma", y = "Eta")
  } else if (p_eta != "all") {
    tbl_plot <- tbl_plot %>% filter(near(eta, p_eta))
    ggplot(tbl_plot, aes(gamma, mean_reward_tot, group = n_options)) +
      geom_errorbar(aes(
        ymin = mean_reward_tot - se_reward_tot * 1.96,
        ymax = mean_reward_tot + se_reward_tot * 1.96,
        color = n_options
      )) +
      geom_line(aes(color = n_options)) +
      geom_point(color = "white", size = 3) +
      geom_point(aes(color = n_options)) +
      facet_wrap(~var) +
      theme_bw() +
      scale_color_brewer(name = "Nr. Options", palette = "Set1") +
      labs(x = "Gamma", y = "Total Reward", title = str_c("Eta = ", p_eta))
  }
}

plot_optimal_gammas <- function(tbl_results_max) {
  var_min <- min(as.numeric(tbl_results_agg$var))
  tbl_results_max <- tbl_results_max %>%
    mutate(
      modelxeta = interaction(model, eta, sep = ", Eta = "),
      modelxeta = case_when(
        model == "Kalman" ~ "Kalman", TRUE ~ str_c(as.character(modelxeta), "*")
      ),
      var = factor(str_c("Var = ", var))
    )
  tbl_results_max$var <- tbl_results_max$var %>% relevel(str_c("Var = ", var_min))

  dg <- position_dodge(width = .0)
  ggplot(tbl_results_max, aes(n_options, gamma, group = modelxeta)) +
    geom_line(aes(color = modelxeta, linetype = model), position = dg) +
    geom_point(size = 3, color = "white", position = dg) +
    geom_point(aes(color = modelxeta), position = dg) +
    facet_wrap(~var) +
    theme_bw() +
    scale_color_brewer(name = "Updating Rule / Model", palette = "Set1") +
    guides(linetype = "none") +
    labs(
      x = "Nr. Options",
      y = "Gamma",
      caption = "* Best-fitting Eta in Speekenbrink & Konstantinidis (2015) approx. 0.6"
    )
}

customGreen0 <- "#DeF7E9"
customGreen <- "#71CA97"
customRed <- "#ff7f7f"
customBlue <- "#4363d8"
customGrey <- "#a9a9a9"

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

  fviz_pca_biplot(pca_decomp,
    repel = TRUE,
    col.var = "#2E9FDF", # Variables color
    col.ind = "#696969" # Individuals color
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
    align = c("l", "c", "c", "c", "c", "r", "r"), list(
      `Task Name` = formatter(
        "span",
        style = x ~ style(color = "gray", width = 100),
        x ~ icontext(ifelse(x %in% c("DFD", "DFE Sampling", "DFE"), "star", ""), x)
      ),
      `Info Reward Confound` = color_tile(customGreen, customRed),
      `Nr. Arms` = color_tile(customGreen0, customGreen),
      `Moving Stats` = color_tile(customGreen0, customGreen),
      `Generalization` = color_tile(customGreen0, customGreen),
      `Experience` = color_tile(customGreen0, customGreen),
      # `Capacity`= color_bar(customRed)
      `Capacity` = formatter(
        "span",
        style = x ~ style(
          display = "inline-block",
          direction = "ltr",
          color = ifelse(x == 0, "black", "white"),
          "background-color" = csscolor(gradient(x, customGreen, customRed)),
          width = percent(x / 3)
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
    align = c("l", "c", "c", "c", "c", "r"), list(
      `Task Name` = formatter(
        "span",
        style = x ~ style(color = "gray", width = 100),
        x ~ icontext(ifelse(x %in% c("DFD", "DFE Sampling", "DFE"), "star", ""), x)
      ),
      `Info Reward Confound` = color_tile(customGreen, customRed),
      `Nr. Arms` = color_tile(customGreen0, customGreen),
      `Moving Stats` = color_tile(customGreen0, customGreen),
      # `Capacity`= color_bar(customRed)
      `Capacity` = formatter(
        "span",
        style = x ~ style(
          display = "inline-block",
          direction = "ltr",
          color = ifelse(x == 0, "black", "white"),
          "background-color" = csscolor(gradient(x, customGreen, customRed)),
          width = percent(x / 3)
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
    align = c("l", "c", "c", "r"), list(
      `Task Name` = formatter(
        "span",
        style = x ~ style(color = "gray", width = 100),
        x ~ icontext(ifelse(x %in% c("DFD", "DFE Sampling", "DFE"), "star", ""), x)
      ),
      `Nr. Arms` = color_tile(customGreen0, customGreen),
      `Moving Stats` = color_tile(customGreen0, customGreen),
      # `Capacity`= color_bar(customRed),
      `Nr. Trials` = color_tile(customGreen0, customGreen)
    )
  )
}




plot_study_table <- function(tbl_studies) {
  #' plot formatted task tbl
  #'
  #' @description plot a formatted tbl of the task tbl
  #' @param tbl_studies \code{tibble} containing the tasks with their features
  #'
  improvement_formatter <- formatter(
    "span",
    style = x ~ icontext(ifelse(x > 0, "arrow-up", "arrow-down"), x)
  )
  tbl_studies <- tbl_studies %>% relocate(ir_confound, .after = driftcondition)

  names(tbl_studies) <- c(
    "Authors", "Mean Drifting?",
    "Confound Inf.-Reward?", "Explore Directed", "Explore Value-Guided"
  )
  tbl_studies <- tbl_studies %>% arrange(Authors, `Mean Drifting?`)
  tbl_studies <- tbl_studies %>% mutate_if(is.logical, function(x) as.character(x))
  tbl_studies[tbl_studies == "FALSE"] <- "No"
  tbl_studies[tbl_studies == "TRUE"] <- "Yes"
  tbl_studies <- tbl_studies %>% mutate_if(is.character, function(x) as.factor(x))

  formattable(
    tbl_studies,
    align = c("l", "c", "c", "r"), list(
      `Mean Drifting?` = color_tile(customGrey, customBlue),
      `Confound Inf.-Reward?` = color_tile(customGreen, customRed),
      `Explore Directed` = color_tile(customRed, customGreen),
      `Explore Value-Guided` = color_tile(customRed, customGreen)
    )
  )
}


plot_some_subjects <- function(tbl_df) {
  #' plot y values of a subset of subjects
  #'
  #' @description plot y values of a subset of subjects for both time points;
  #' also display by-time point and by-subject means
  #' @param tbl_df tbl with trial-wise data
  #' @return the ggplot object
  ggplot(
    tbl_df %>%
      filter(subject %in% sample(1:n_subjects, 5, replace = FALSE)) %>%
      group_by(subject, timepoint) %>% mutate(y_mn = mean(y)) %>% arrange(y_mn) %>%
      ungroup() %>% mutate(subject = fct_inorder(as.character(subject))),
    aes(timepoint, y, group = subject)
  ) +
    geom_point(aes(color = subject), position = position_dodge(width = .2)) +
    geom_point(aes(y = y_mn), position = position_dodge(width = .2), size = 3, shape = 2) +
    scale_color_viridis_d(name = "Subject ID") +
    scale_x_discrete(labels = c("1", "2")) +
    theme_bw() +
    labs(x = "Timepoint", y = "y")
}

save_my_tiff <- function(pl, path_fl, w, h) {
  tiff(path_fl, w, h, "in", res = 300)
  grid.draw(pl)
  dev.off()
}

save_my_pdf <- function(pl, path_fl, w, h) {
  pdf(path_fl, w, h, paper = "special")
  grid.draw(pl)
  dev.off()
}


plot_my_heatmap_softmax <- function(tbl_x) {
  ggplot(
    tbl_x %>% 
      mutate(my_vars = colnames(tbl_x[1:3])) %>%
      pivot_longer(cols = c(sigma_xi_sq_ml, sigma_epsilon_sq_ml, gamma_ml)) %>%
      mutate(
        my_vars = factor(my_vars),
        my_vars = fct_recode(my_vars, "Sigma Xi" = "sigma_xi_sq_ml", "Sigma Epsilon" = "sigma_epsilon_sq_ml", "Gamma" = "gamma_ml"),
        name = factor(name),
        name = fct_recode(name, "Sigma Xi" = "sigma_xi_sq_ml", "Sigma Epsilon" = "sigma_epsilon_sq_ml", "Gamma" = "gamma_ml")
      ) , 
    aes(my_vars, name)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(name = "") +
    geom_label(aes(label = str_c("r = ", round(value, 2)))) +
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(title = str_c("Gamma = ", tbl_x[1, "gamma_mn"], ",\nSimulate Data by Participant = ", tbl_x[1, "simulate_data"], ",\nNr. Trials = ", tbl_x[1, "nr_trials"]))
}


plot_my_heatmap_thompson <- function(tbl_x) {
  ggplot(
    tbl_x %>% 
      mutate(my_vars = colnames(tbl_x[1:2])) %>%
      pivot_longer(cols = c(sigma_xi_sq_ml, sigma_epsilon_sq_ml)) %>%
      mutate(
        my_vars = factor(my_vars),
        my_vars = fct_recode(my_vars, "Sigma Xi" = "sigma_xi_sq_ml", "Sigma Epsilon" = "sigma_epsilon_sq_ml"),
        name = factor(name),
        name = fct_recode(name, "Sigma Xi" = "sigma_xi_sq_ml", "Sigma Epsilon" = "sigma_epsilon_sq_ml")
      ) , 
    aes(my_vars, name)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(name = "") +
    geom_label(aes(label = str_c("r = ", round(value, 2)))) +
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(title = str_c("Simulate Data by Participant = ", tbl_x[1, "simulate_data"], ",\nNr. Trials = ", tbl_x[1, "nr_trials"]))
}