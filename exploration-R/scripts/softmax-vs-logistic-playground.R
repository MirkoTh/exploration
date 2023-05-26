
# Simulate random and directed exploration in a logistic model ------------




n_data <- 5000
x_valdiff <- rep(seq(-5, 5, length.out = n_data/2), 2)
x_horizon <- rep(c(-.5, .5), each = n_data/2)
x_info <- rep(c(0, 1), n_data/2)
intercept <- rnorm(n_data, 0, 1)
valdiff <- rnorm(n_data, 4, 1)
horizonxvaldiff <- rnorm(n_data, 3, 1)
infoxhorizon <- rnorm(n_data, 3, 1)
y_logit <- intercept + x_valdiff * valdiff - x_horizon*x_valdiff*horizonxvaldiff + x_info*x_horizon*infoxhorizon
prob_choice <- 1/(1 + exp(-y_logit))
choices <- rbernoulli(n_data, prob_choice)
tbl_data <- tibble(
  x_valdiff = x_valdiff,
  x_horizon = x_horizon,
  x_info = x_info,
  y_logit = y_logit,
  y = choices
)

tbl_data %>% mutate(x_valdiff_cut = cut(x_valdiff, 11, labels = FALSE)) %>%
  group_by(x_valdiff_cut, x_horizon, x_info) %>%
  summarize(logit_mean = mean(y)) %>%
  ggplot(aes(x_valdiff_cut, logit_mean, group = x_horizon)) +
  geom_line(aes(color = x_horizon)) +
  facet_wrap(~ x_info)


m <- glm(y ~ x_valdiff + x_valdiff:x_horizon + x_info:x_horizon, family = binomial, data = tbl_data)
summary(m)


# plot nr. of switches in data simulated from 4-armed RL bandit -----------

# this is just exploratory code using output from the file exploration-R/scripts/parameter-recovery.R
# nb, cannot be run on itself

ggplot(tmp2 %>% filter(gamma_ml < 2.9), aes(gamma, gamma_ml, group = gamma_mn)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~ gamma_mn, scales = "free_x") +
  geom_abline()
ggplot(tmp2, aes(sigma_xi_sq, sigma_xi_sq_ml, group = gamma_mn)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~ gamma_mn) +
  geom_abline()
ggplot(tmp2, aes(sigma_epsilon_sq, sigma_epsilon_sq_ml, group = gamma_mn)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~ gamma_mn) +
  geom_abline()

tmp2 %>% group_by(gamma_mn) %>%
  summarize(n_larger = sum(gamma_ml > gamma))



switches.08 <- map(map(l_choices_simulated, "tbl_return"), ~ .x %>% mutate(is_switch = choices != lag(choices)) %>% count(is_switch))
avg_switches.08 <- reduce(switches.08, rbind) %>% mutate(id = rep(1:200, each = 3)) %>% filter(is_switch) %>% summarize(mean(n), sd(n)) %>%
  mutate(gamma = .08)


switches.16 <- map(map(l_choices_simulated, "tbl_return"), ~ .x %>% mutate(is_switch = choices != lag(choices)) %>% count(is_switch))
avg_switches.16 <- reduce(switches.16, rbind) %>% mutate(id = rep(1:200, each = 3)) %>% filter(is_switch) %>% summarize(mean(n), sd(n)) %>%
  mutate(gamma = .16)


switches1 <- map(map(l_choices_simulated, "tbl_return"), ~ .x %>% mutate(is_switch = choices != lag(choices)) %>% count(is_switch))
avg_switches1 <- reduce(switches1, rbind) %>% mutate(id = rep(1:200, each = 3)) %>% filter(is_switch) %>% summarize(mean(n), sd(n)) %>%
  mutate(gamma = 1)



switches3 <- map(map(l_choices_simulated, "tbl_return"), ~ .x %>% mutate(is_switch = choices != lag(choices)) %>% count(is_switch))
avg_switches3 <- reduce(switches3, rbind) %>% mutate(id = rep(1:200, each = 3)) %>% filter(is_switch) %>% summarize(mean(n), sd(n)) %>%
  mutate(gamma = 3)


rbind(avg_switches.08, avg_switches.16, avg_switches1, avg_switches3) %>%
  relocate(gamma, .before = `mean(n)`) %>%
  rename(Gamma = gamma, `Mean Switches` = `mean(n)`, `Sd Switches` = `sd(n)`) %>%
  mutate(`Prop. Switches` = `Mean Switches`/ 400) %>%
  round(3) %>%
  DT::datatable()
