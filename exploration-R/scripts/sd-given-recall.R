data <- rnorm(1000)

sample_n_data <- function(n, data) {
  data_trial <- sample(data, n)
  sd(data_trial)
}
ns <- seq(2, 20, by = 1)
n_iter <- 10000
l_results <- list()
for (i in 1:n_iter) {
  v_results <- map_dbl(ns, sample_n_data, data = data)
  l_results[[i]] <- v_results
}
plot(colMeans(l_results %>% reduce(rbind)))
