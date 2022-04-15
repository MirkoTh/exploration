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
    "Generalization", "Experience", "Capacity", "Info Reward Confound"
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
      )
    )
  )
}
