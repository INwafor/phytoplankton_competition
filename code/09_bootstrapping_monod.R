#monod # 2
source("code/05_monod_1.R")

##starter monod curve - this is what i have rn and am attempting to bootstrap 
preds4 %>%
  mutate(r_concentration = as.numeric(r_concentration)) %>%  
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ umax * (x / (ks + x)),
              method.args = list(start = list(umax = 0.5, ks = 0.5)),
              se = FALSE) +
  facet_wrap(~ file_name, scales = "free") +
  geom_errorbar(aes(ymin=estimate-best_se, ymax=estimate + best_se), width=.2) + 
  geom_line(data = preds4, aes(x = r_concentration, y = fitted), color = "#9e0142", size = 1) +
  ylab("Exponential growth rate (/day)") + 
  xlab("Phosphate concentration (uM)") 

## positive only - fist 8c, scen 21c
preds_subset <- preds4 %>%
  filter(file_name != "Fist_21C",
         file_name != "Fist_30C",
         file_name != "Scen_30C",
         file_name != "Scen_8C")

preds_subset %>%
  mutate(r_concentration = as.numeric(r_concentration)) %>%  
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ umax * (x / (ks + x)),
              method.args = list(start = list(umax = 0.5, ks = 0.5)),
              se = FALSE) +
  facet_wrap(~ file_name, scales = "free") +
  #geom_errorbar(aes(ymin=estimate-best_se, ymax=estimate + best_se), width=.2) + 
  geom_line(data = preds_subset, aes(x = r_concentration, y = fitted), color = "#3288bd", size = 1) +
  ylab("Exponential growth rate (/day)") + 
  xlab("Phosphate concentration (uM)") 

#set mortality rate and R*?

## bootstrapping from rtpc trial 3

# fit Sharpe-Schoolfield model - why would i fit to this model? 
d_fit <- nest(preds4, data = c(r_concentration, estimate)) %>%
  mutate(sharpeschoolhigh = map(data, ~nls_multstart(estimate~sharpeschoolhigh_1981(r_concentration = r_tref,e,eh,th, tref = 15),
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
