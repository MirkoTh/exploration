library(tidyverse)

my_two_seeds <- c(39737632, 8567389)
session_id <- 2

set.seed(my_two_seeds[session_id])

maxSetSize <- 8
minSetSize <- 4
n_reps <- 3
SetSizes <- seq(minSetSize, maxSetSize, by = 1)
nTrials <- length(SetSizes) * n_reps

sample_size_sufficient <- (maxSetSize + 1) * nTrials
possibleOperations <- c(" + ", " - ")

eqsCorrect <- sample(c("true", "false"), sample_size_sufficient, replace = TRUE)
eqsCorrect_demo <- sample(c("true", "false"), 100, replace = TRUE)
operations <- sample(possibleOperations, sample_size_sufficient, replace = TRUE)
ansDiffs <- sample(c(1, 2), sample_size_sufficient, replace = TRUE)
coinFlips <- sample(c("true", "false"), sample_size_sufficient, replace = TRUE)

num1plus <- floor(runif(sample_size_sufficient, min = 1, max = 11))
num2plus <- floor(runif(sample_size_sufficient, min = 1, max = 11))
num1minus <- floor(runif(sample_size_sufficient, min = 1, max = 11))
num2minus <- floor(runif(sample_size_sufficient, min = 1, max = num1minus))

eqsCorrect_str <- reduce(c("[", str_c(eqsCorrect, ", ")), str_c)
str_c(str_replace_all(eqsCorrect_str, "(e)(,) $", replacement="\\1"), "]")

eqsCorrect_demo_str <- reduce(c("[", str_c(eqsCorrect_demo, ", ")), str_c)
str_c(str_replace_all(eqsCorrect_demo_str, "(e)(,) $", replacement="\\1"), "]")

operations_str <- reduce(c("[", str_c("'", operations, "', ")), str_c)
str_c(str_replace_all(operations_str, "(,) $", replacement=""), "]")

ansDiffs_str <- reduce(c("[", str_c(ansDiffs, ", ")), str_c)
str_c(str_replace(ansDiffs_str, ", $", replacement=""), "]")

coinFlips_str <- reduce(c("[", str_c(coinFlips, ", ")), str_c)
str_c(str_replace_all(coinFlips_str, "(e)(,) $", replacement="\\1"), "]")

l_str <- map(
  list(num1plus, num1minus, num2plus, num2minus), 
  ~ reduce(c("[", str_c(.x, ", ")), str_c)
)
map(l_str, ~ str_c(str_replace(.x, ", $", replacement=""), "]"))



