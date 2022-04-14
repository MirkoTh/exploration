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