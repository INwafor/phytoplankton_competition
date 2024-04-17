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
  # geom_hline(yintercept = 0.1, linetype = "dashed", color = "black") +
  ylab("Exponential growth rate (/day)") + 
  xlab("Phosphate concentration (uM)") 

## bootstrapping from rtpc trial 3
install.packages("minpack.lm")
library(minpack.lm)

class(fit_model2) # this is a nls model 

bootstrap_nls <- function(data, n_bootstraps) {
  bootstrapped_coefs <- replicate(1000, {
    boot_data <- preds4[sample(nrow(preds4), replace = TRUE), ]
    fitted_model <- fit_nls_model(boot_data)
    coef(fitted_model)
  }, simplify = FALSE)
  return(bootstrapped_coefs)
}

boot_result <- bootstrap_nls(preds4, n_bootstraps = 1000)

boot_ci <- t(apply(boot_result, 2, quantile, c(0.025, 0.975)))  

#boot_ci <- boot.ci(boot_result, type = "basic") 
colnames(boot_ci) <- c("2.5%", "97.5%")
boot_ci



