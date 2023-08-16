library(tidyverse)
library(grid)
library(gridExtra)


x_vals <- seq(1, 100, by = 1)
y_vals <- c(rep(1, 9), rep(4, 17), rep(1, 54), rep(7, 5), rep(1, 15))

tbl_bimodal <- tibble(
  x = x_vals,
  y = y_vals
)

tbl_target <- unlist(pmap(tbl_bimodal, ~ rep(.x, .y))) %>%
  as_tibble() %>%
  rename(x = value)



tbl_responses <- sample(1:100, 1810, replace = TRUE) %>%
  as_tibble() %>%
  rename(x = value)


pl_learn_bimodal <- ggplot() +
  geom_histogram(data = tbl_target, aes(x), binwidth = 1, color = "black", fill = "#21918c") + 
  geom_histogram(data = tbl_responses, aes(x, y = after_stat(count) * .1), binwidth = 1, color = "black", fill = "white", alpha = .7) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(1, 7, by = 1)) +
  labs(x = "Value", y = "Frequency", title = "Mason et al. (2022)") +
  theme(strip.background = element_rect(fill = "white"), text = element_text(size = 20))



x_transition <- seq(-5, 5, by = 1)
y_transition <- c(seq(1/6, 1, by = 1/6), seq(5/6, 1/6, by = -1/6))/6
y_transition_dat <- c(2.5/66, 2/66, 3/66, 4.5/66, 17/66, 2.5/66, 20.5/66, 7/66, 3.5/66, 2/66, 1/66)
y_transition_c <- c(2/66, 2.5/66, 4.5/66, 8/66, 16/66, 2.5/66, 12.5/66, 8/66, 5/66, 3/66, 1.5/66)
tbl_transition <- tibble(
  x = c(x_transition, x_transition, x_transition),
  y = c(y_transition, y_transition_dat, y_transition_c),
  group = c(rep("Real Die", 11), rep("DAT", 11), rep("Healty Control", 11))
)
pl_die <- ggplot() +
  geom_line(data = tbl_transition, aes(x, y, group = group, color = group)) + 
  geom_point(data = tbl_transition, aes(x, y, group = group), color = "white", size = 4) +
  geom_point(data = tbl_transition, aes(x, y, group = group, shape = group, color = group)) +
  theme_bw() +
  scale_x_continuous(expand = c(.01, 0), breaks = seq(-5, 5, by = 1)) +
  scale_y_continuous(expand = c(.01, 0)) +
  scale_color_viridis_d(name = "Group") +
  scale_shape_discrete(guide = "none") +
  theme(legend.position = c(.85, .7), text = element_text(size = 20)) +
  labs(x = "Step n - 1 to n", y = "Probability Mass", title = "Brugger et al. (1996)") +
  theme(strip.background = element_rect(fill = "white"))
  

pl_both <- arrangeGrob(pl_learn_bimodal, pl_die, nrow = 2)
save_my_pdf_and_tiff(pl_both, "documents/ccn-2023/lit-review-4", 10, 8)

dev.off()













