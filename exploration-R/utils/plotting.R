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
    l_m$m %>% as.data.frame() %>% as_tibble() %>%
      rename_all(~ substr(.x, 2, 4)) %>%
      mutate(
        trial_id = 1:nrow(l_m$m),
        var = str_c("Var = ", var)
        ) %>%
      pivot_longer(cols = -c(trial_id, var)) %>%
      mutate(n_options = str_c("Nr. Options = ", length(unique(name))))
  }
  pmap(list(l_l_m, as.list(tbl_conditions$var)), format_means) %>% reduce(rbind) %>%
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
  tbl_plot <- tbl_results_agg %>% filter(model == "Kalman") %>%
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
    facet_wrap(~ var) +
    theme_bw() +
    scale_color_brewer(name = "Nr. Options", palette = "Set1") +
    labs(x = "Gamma", y = "Total Reward")
}

plot_total_rewards_decay <- function(tbl_results_agg, p_eta = "all") {
  var_min <- min(as.numeric(tbl_results_agg$var))
  tbl_plot <- tbl_results_agg %>% filter(model == "Decay") %>%
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
      facet_wrap(~ var) +
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
    facet_wrap(~ var) +
    theme_bw() +
    scale_color_brewer(name = "Updating Rule / Model", palette = "Set1") +
    guides(linetype = "none") +
    labs(
      x = "Nr. Options",
      y = "Gamma",
      caption = "* Best-fitting Eta in Speekenbrink & Konstantinidis (2015) approx. 0.6"
    )
}
