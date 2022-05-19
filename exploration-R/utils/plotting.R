check_reward_distributions <- function(tbl_conditions) {
  #' violin plots of simulated rewards
  #' 
  #' @description violin plots of simulated rewards faceted by condition
  #' @param tbl_conditions tibble with information and rewards
  #' @return nothing; just plots
  
  tbl_rewards <- map(tbl_conditions$rewards, ~ as.data.frame(.x)) %>%
    map(~ pivot_longer(.x, everything())) %>%
    map(~ mutate(.x, n_arms = length(unique(name)))) %>%
    reduce(rbind) %>% 
    mutate(name = substr(name, 2, 5))
  
  ggplot(tbl_rewards, aes(name, value, group = name)) +
    geom_violin(draw_quantiles = .5, aes(fill = name)) +
    scale_fill_brewer(name = "Option", palette = "Set1") +
    facet_wrap(~ n_arms) + 
    theme_bw() +
    labs(x = "Option", y = "Reward")
}

plot_mean_trajectories <- function(l_l_m) {
  #' plot estimated mean trajectories of response options
  #' 
  #' @description using one walk through with the rl model
  #' @param l_l_m nested list with modeling results in inner lists
  #' @return nothing; just plots
  
  
  format_means <- function(l_m) {
    l_m$m %>% as.data.frame() %>% as_tibble() %>%
      rename_all(~ substr(.x, 2, 4)) %>%
      mutate(trial_id = 1:nrow(l_m$m)) %>%
      pivot_longer(cols = -trial_id) %>%
      mutate(n_options = length(unique(name)))
  }
  map(l_l_m, format_means) %>% reduce(rbind) %>%
    ggplot(aes(trial_id, value, group = name)) +
    geom_line(aes(color = name)) +
    geom_point(color = "white", size = 3) +
    geom_point(aes(color = name), shape = 1) +
    facet_wrap(~ n_options) +
    scale_color_brewer(name = "Option", palette = "Set1") +
    theme_bw() +
    labs(
      x = "Trial ID",
      y = "Estimated Mean"
    )
}


plot_total_rewards_kalman <- function(tbl_results_agg) {
  tbl_plot <- tbl_results_agg %>% filter(model == "Kalman")
  ggplot(tbl_plot, aes(gamma, mean_reward_tot, group = n_options)) +
    geom_errorbar(aes(
      ymin = mean_reward_tot - se_reward_tot * 1.96,
      ymax = mean_reward_tot + se_reward_tot * 1.96,
      color = n_options
    )) +
    geom_line(aes(color = n_options)) +
    geom_point(color = "white", size = 3) +
    geom_point(aes(color = n_options)) +
    theme_bw() +
    scale_color_brewer(name = "Nr. Options", palette = "Set1") +
    labs(x = "Gamma", y = "Total Reward")
}

plot_total_rewards_decay <- function(tbl_results_agg, p_eta = "all") {
  tbl_plot <- tbl_results_agg %>% filter(model == "Decay") %>%
    group_by(n_options) %>%
    mutate(mean_reward_prop = mean_reward_tot / max(mean_reward_tot)) %>%
    ungroup()
  
  if (p_eta == "all") {
    ggplot(tbl_plot, aes(gamma, eta)) +
    geom_raster(aes(fill = mean_reward_prop)) +
    geom_text(aes(label = round(mean_reward_prop, 2), color = mean_reward_prop)) +
    facet_wrap(~ n_options, ncol = 1) +
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
      theme_bw() +
      scale_color_brewer(name = "Nr. Options", palette = "Set1") +
      labs(x = "Gamma", y = "Total Reward", title = str_c("Eta = ", eta))
  }
}

plot_optimal_gammas <- function(tbl_results_max, eta_speek_konst) {
  ggplot(tbl_results_max, aes(n_options, gamma, group = model)) +
    geom_line(aes(color = model)) +
    geom_point(size = 3, color = "white") +
    geom_point(aes(color = model)) +
    theme_bw() +
    scale_color_brewer(name = "Model", palette = "Set1") +
    labs(
      x = "Nr. Options",
      y = "Gamma",
      title = str_c("Decay Model: eta = ", eta_speek_konst, " *"),
      caption = "* Approx. value taken from Speekenbrink & Konstantinidis (2015)"
    )
}
