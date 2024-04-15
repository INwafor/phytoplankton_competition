#monod # 2
source("code/05_monod_1.R")

library(tidyverse)
library(cowplot)
library(broom)
library(readxl)
library(janitor)
library(plotrix)
library(here)
library(boot)
library(car)
library(rTPC)
library(nls.multstart)
library(patchwork)
library(minpack.lm)

#bootstrapping from rtpc 
d_fit <- nest(preds4, data = c(r_concentration, estimate)) %>%
  mutate(sharpeschoolhigh = map(data, ~nls_multstart(estimate~sharpeschoolhigh_1981(temp = r_concentration, r_tref,e,eh,th, tref = 15),
                                                     data = .x,
                                                     iter = c(3,3,3,3),
                                                     start_lower = get_start_vals(.x$r_concentration, .x$estimate, model_name = 'sharpeschoolhigh_1981') - 10,
                                                     start_upper = get_start_vals(.x$r_concentration, .x$estimate, model_name = 'sharpeschoolhigh_1981') + 10,
                                                     lower = get_lower_lims(.x$r_concentration, .x$estimate, model_name = 'sharpeschoolhigh_1981'),
                                                     upper = get_upper_lims(.x$r_concentration, .x$estimate, model_name = 'sharpeschoolhigh_1981'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(sharpeschoolhigh, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds <- select(d_fit, preds) %>%
  unnest(preds)

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Growth rate across temperatures')
d_fit <- nest(d, data = c(temp, rate)) %>%
  mutate(sharpeschoolhigh = map(data, ~nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                                                     data = .x,
                                                     iter = c(3,3,3,3),
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') - 10,
                                                     start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') + 10,
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(sharpeschoolhigh, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds <- select(d_fit, preds) %>%
  unnest(preds)

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Growth rate across temperatures')

fit_nlsLM <- minpack.lm::nlsLM(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                               data = d,
                               start = coef(d_fit$sharpeschoolhigh[[1]]),
                               lower = get_lower_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981'),
                               upper = get_upper_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981'),
                               weights = rep(1, times = nrow(d)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)
# create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(d$temp), max(d$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = sharpeschoolhigh_1981(temp, r_tref, e, eh, th, tref = 15))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p1 <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Growth rate across temperatures')

{
fit_model2 <- growth_sum_p2 %>%
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
  #rename(estimate = mu) %>% 
  group_by(file_name) %>% 
  nls(mu ~ umax * (r_concentration / (ks + r_concentration)),
      data = .,
      start = list(ks = 0.5, umax = 0.5),
      algorithm = "port",
      control = nls.control(maxiter = 500, minFactor = 1/204800000))



fit_model <- nls(mu ~ umax * (r_concentration / (ks + r_concentration)),
                 data = preds4,
                 start = list(ks = 0.5, umax = 0.5),
                 algorithm = "port",
                 control = nls.control(maxiter = 500, minFactor = 1/204800000))
class(fit_model)

library(tidymodels)
install.packages("tidymodels")

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

preds4 %>%
  mutate(r_concentration = as.numeric(r_concentration)) %>%  
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ umax * (x / (ks + x)),
              method.args = list(start = list(umax = 0.5, ks = 0.5)),
              se = FALSE) +
  facet_wrap(~ file_name, scales = "free") +
  geom_errorbar(aes(ymin=estimate-best_se, ymax=estimate + best_se), width=.2) + 
  geom_line(data = preds4, aes(x = r_concentration, y = fitted), color = "blue", size = 1) +
  ylab("Exponential growth rate (/day)") + 
  xlab("Phosphate concentration (uM)") 

## this would be the monod grave to use 
}