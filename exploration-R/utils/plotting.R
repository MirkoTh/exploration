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
    scale_color_brewer(name = "Option", palette = "Set1") +
    facet_wrap(~ n_arms) + 
    theme_bw() +
    labs(x = "Option", y = "Reward")
}