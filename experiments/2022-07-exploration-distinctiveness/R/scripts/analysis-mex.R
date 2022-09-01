library(tidyverse)




path_data <- c("experiments/2022-07-exploration-distinctiveness/data/2022-09-01-mex1-pilot/")
returned_timeout <- c()

l_tbls_data <- map(path_data, load_data, participants_returned = returned_timeout)
l_tbl_data <- list(
  "memory" = reduce(map(l_tbls_data, "memory"), rbind),
  "choice" = reduce(map(l_tbls_data, "choice"), rbind)
)

tbl_choice <- l_tbl_data[["choice"]]
tbl_memory <- l_tbl_data[["memory"]]



