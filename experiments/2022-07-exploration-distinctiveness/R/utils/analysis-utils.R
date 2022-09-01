
load_data <- function(path_data, participants_returned) {
  #' load memory and choice data
  #' 
  #' @description loads data and declares factor and numeric columns in the two tibbles
  #' 
  #' @return a list with the two tibbles
  #' 

  # check for each participant which file has more data and select that one
  
  
  files_dir <- dir(path_data)
  fld_mem <- files_dir[startsWith(files_dir, "memory")]
  fld_choice <- files_dir[startsWith(files_dir, "choice")]
  paths_mem_individual <- str_c(path_data, fld_mem[!str_detect(fld_mem, "allinone")])
  paths_choice_individual <- str_c(path_data,  fld_choice[!str_detect(fld_choice, "allinone")])
  paths_choice_compound <- str_c(path_data,  fld_choice[str_detect(fld_choice, "allinone")])
  paths_mem_compound <- str_c(path_data, fld_mem[str_detect(fld_mem, "allinone")])

  l_paths <- list(
    `memory` = paths_mem_individual, 
    `choice` = paths_choice_individual, 
    `memory-allinone` = paths_mem_compound, 
    `choice-allinone` = paths_choice_compound
  )
  
  json_to_tibble <- function(path_file) {
    js_txt <- read_file(path_file)
    js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
    js_txt <- str_replace(js_txt, ",\n]", "]")
    tbl_df <- jsonlite::fromJSON(js_txt) %>% as_tibble()
    return(tbl_df)
  }
  
  inner_map <- safely(function(x) map(x, json_to_tibble))
  l_tbl_all <- map(l_paths, inner_map)
  l_tbl_all <- map(l_tbl_all, "result")
  l_mask <- map_lgl(l_tbl_all, ~!(is.null(.x)))
  l_tbl_all <- l_tbl_all[l_mask]
  inner_map <- function(a, b) map(
    a, function(x) c(participant_id = x$participant_id[1], ntrials = nrow(x))
  ) %>% reduce(rbind) %>% rbind() %>% as_tibble() %>% mutate(savemethod = b)
  tbl_ntrials <- map2(l_tbl_all, names(l_tbl_all), inner_map) %>% reduce(rbind)
  tbl_ntrials$task <- factor(str_detect(tbl_ntrials$savemethod, "memory"), labels = c("choice", "memory"))
  files_select <- tbl_ntrials %>% group_by(participant_id, task) %>%
    mutate(rwn_max = row_number(desc(ntrials))) %>% 
    filter(rwn_max == 1)
  l_files_select <- split(files_select, files_select$task)
  c_paths <- function(x) str_c(path_data, x$savemethod, "-participant-", x$participant_id, ".json")
  l_paths <- map(l_files_select, c_paths)
  
  tbl_mem <- reduce(map(l_paths[["memory"]], json_to_tibble), rbind) %>% filter(session %in% c(1, 2))
  tbl_choice <- reduce(map(l_paths[["choice"]], json_to_tibble), rbind)

  factors <- c("participant_id", "session", "item_id", "presentation", "horizon", "is_memtest")
  numerics <- c("trial_id", "true_mean", "n_correct", "n_redundant", "rt")
  tbl_mem <- fix_data_types(tbl_mem, c(factors, "loc_cued"), numerics)
  tbl_choice <- fix_data_types(tbl_choice, factors, numerics)
  
  tbl_mem <- tbl_mem %>% filter(!(participant_id %in% participants_returned))
  tbl_choice <- tbl_choice %>% filter(!(participant_id %in% participants_returned))
  
  l_data <- list("memory" = tbl_mem, "choice" = tbl_choice)
  return(l_data)
}


fix_data_types <- function(tbl, fs, ns) {
  #' fix data types of columns of tbl
  #' 
  #' @description factors as factors and numerics as numerics, leave characters
  #' 
  #' @param tbl the columns of this tibble are changed
  #' @param fs vector of factors
  #' @param ns vector of numerics
  #' @return the tibble with the changed data types
  #' 
  cols <- colnames(tbl)
  fs_available <- intersect(fs, cols)
  ns_available <- intersect(ns, cols)
  tbl[, fs_available] <- map(tbl[, fs_available], as.factor)
  tbl[, ns_available] <- map(tbl[, ns_available], as.numeric)
  return(tbl)
}

