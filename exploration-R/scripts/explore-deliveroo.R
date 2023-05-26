library(tidyverse)
library(rutils)


tbl_deliveroo <- read_csv("exploration-R/data/deliveroo.csv")
tbl_deliveroo <- tbl_deliveroo %>%
  group_by(USER_ID) %>%
  arrange(SUBMITTED_AT_LOCAL_TIME) %>%
  mutate(
    ORDER_NR = row_number(ORDER_ID),
    IS_REORDER = RESTAURANT_ID == lag(RESTAURANT_ID)
    ) %>%
  ungroup()


tbl_deliveroo_smpl <- tbl_deliveroo[tbl_deliveroo$USER_ID %in% sample(unique(tbl_deliveroo$USER_ID), 1000), ]
tbl_deliveroo_smpl <- tbl_deliveroo_smpl %>%
  group_by(USER_ID) %>%
  arrange(SUBMITTED_AT_LOCAL_TIME) %>%
  mutate(ORDER_NR_IN_PERIOD = row_number(ORDER_ID)) %>%
  ungroup()


# re-plotting Fig. 1A from the PNAS paper
grouped_agg(
  tbl_deliveroo %>% 
    rename(RATING = ORDER_RATING) %>%
    filter(!is.na(RATING)), 
  c(IS_NEW_TO_RESTAURANT, NB_ORDERS_USER), RATING
) %>%
  rename()  %>%
  filter(NB_ORDERS_USER <= 300) %>%
  ggplot(aes(NB_ORDERS_USER, mean_RATING, group = IS_NEW_TO_RESTAURANT)) +
  geom_errorbar(aes(
    ymin = mean_RATING - se_RATING, 
    ymax = mean_RATING + se_RATING,
    color = IS_NEW_TO_RESTAURANT
  )) +
  geom_line(aes(color = IS_NEW_TO_RESTAURANT)) +
  geom_point(size = 3, color = "white") +
  geom_point(aes(color = IS_NEW_TO_RESTAURANT)) + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_brewer(palette = "Set1", name = "New Restaurant?") +
  labs(x = "Order Nr.", y = "Avg. Rating") +
  theme(strip.background = element_rect(fill = "white")) +
  coord_cartesian(ylim = c(4.1, 4.8))

length(unique(tbl_deliveroo$USER_ID))


tbl_orders_restaurants <- tbl_deliveroo %>% 
  group_by(USER_ID, RESTAURANT_ID) %>%
  count() %>%
  group_by(USER_ID) %>%
  summarize(nr_orders = sum(n), n_restaurants = n()) %>%
  ungroup()


ggplot(tbl_orders_restaurants, aes(n_restaurants)) + 
  geom_histogram(fill = "white", color = "black", binwidth = 1) +
  scale_x_continuous(breaks = seq(1, 25, by = 2), expand = c(.03, 0)) + 
  theme_bw() +
  scale_y_continuous(expand = c(0, 100)) +
  labs(x = "Nr. Restaurants", y = "Nr. Customers") +
  theme(strip.background = element_rect(fill = "white")) +
  coord_cartesian(xlim = c(1, 25))


tbl_deliveroo %>%
  filter(!is.na(IS_REORDER)) %>%
  count(IS_REORDER) %>%
  mutate(p_reorder = n / sum(n))


tbl_deliveroo %>% count(ORDER_RATING) %>% mutate(prop = n / sum(n))

# look at sequences of same vs. different restaurant over time
tbl_deliveroo_smpl %>%
  filter(!is.na(IS_REORDER) & !is.na(ORDER_RATING)) %>%
  group_by(ORDER_NR, ORDER_RATING) %>%
  summarize(prop_reorder = mean(IS_REORDER), n_data = n()) %>%
  filter(n_data > 200) %>%
  ggplot(aes(ORDER_RATING, prop_reorder, group = ORDER_NR)) +
  geom_line(aes(color = ORDER_NR)) +
  geom_point(color = "white", size = 5) +
  geom_point(aes(size = n_data, color = ORDER_NR)) + 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Rating", y = "P(Reorder)") +
  theme(strip.background = element_rect(fill = "white"))
  
















