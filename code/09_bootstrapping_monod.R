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
theme_set(theme_cowplot())

##starter monod curve - this is what i have rn and am attempting to bootstrap 
## keep as is with negative growth rates too - estimates from monod curve unreliable for scen 30C
preds4 %>%
  mutate(r_concentration = as.numeric(r_concentration)) %>%  
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ umax * (x / (ks + x)),
              method.args = list(start = list(umax = 0.5, ks = 0.5)),
              se = FALSE) +
  facet_wrap(~ file_name, scales = "free") +
  geom_errorbar(aes(ymin = estimate - best_se, ymax = estimate + best_se), width = .2) + 
  geom_line(data = preds4, aes(x = r_concentration, y = fitted), color = "#9e0142", size = 1) +
  ylab("Exponential growth rate (/day)") + 
  xlab("Phosphate concentration (uM)") +
  geom_ribbon(data = newdf, aes(x = r_concentration, ymin = CI.2.5., ymax = CI.97.5., fill = parameters), alpha = 0.3)

merged_data <- preds4 %>%
  left_join(newdf, by = "file_name")

# Plot the merged data
merged_data %>%
  mutate(r_concentration = as.numeric(r_concentration)) %>%  
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ umax * (x / (ks + x)),
              method.args = list(start = list(umax = 0.5, ks = 0.5)),
              se = FALSE) +
  facet_wrap(~ file_name, scales = "free") +
  geom_errorbar(aes(ymin = estimate - best_se, ymax = estimate + best_se), width = .2) + 
  geom_line(aes(x = r_concentration, y = fitted), color = "#9e0142", size = 1.5) +
  #geom_ribbon(aes(ymin = estimate - best_se, ymax = estimate + best_se), width = .2) +
  ylab("Exponential growth rate (/day)") + 
  xlab("Phosphate concentration (uM)")

## bootstrapping from rtpc trial 3
install.packages("minpack.lm")
install.packages('nlstools')
library(nlstools)
library(minpack.lm)

##file names all in one
fit_model2 <- growth_sum_p2 %>%
  mutate(r_concentration = as.numeric(r_concentration)) 
  
fit_model3 <- nls(mu ~ umax * (r_concentration / (ks + r_concentration)),
      data = fit_model2,
      start = list(ks = 0.5, umax = 0.5),
      algorithm = "port",
      control = nls.control(maxiter = 500, minFactor = 1/204800000))

class(fit_model3) # this is a nls model 
summary(fit_model3)

boot1 <- nlsBoot(fit_model3)


#mean / standard error for fist 8C
boot1$estiboot
#boot2 <- data_frame(boot1$estiboot, colnames(c("Estimate", "Std_error")))

#median and 2.5 - 97.5 confidence intervals 
boot1$bootCI 
#%>% ggplot(aes(x ))

##doing individually
f_21C <- growth_sum_p2 %>%
  filter(file_name == "Fist_21C") %>% 
  mutate(r_concentration = as.numeric(r_concentration)) 
f21C_mod <- nls(mu ~ umax * (r_concentration / (ks + r_concentration)),
                  data = f_21C,
                  start = list(ks = 0.5, umax = 0.5),
                  algorithm = "port",
                  control = nls.control(maxiter = 500, minFactor = 1/204800000))
b_f21C <- nlsBoot(f21C_mod)
b_f21C$bootCI

f_8C <- growth_sum_p2 %>%
  filter(file_name == "Fist_8C") %>% 
  mutate(r_concentration = as.numeric(r_concentration)) 
f8C_mod <- nls(mu ~ umax * (r_concentration / (ks + r_concentration)),
                data = f_8C,
                start = list(ks = 0.5, umax = 0.5),
                algorithm = "port",
                control = nls.control(maxiter = 500, minFactor = 1/204800000))
b_f8C <- nlsBoot(f8C_mod)
b_f8C$bootCI

f_30C <- growth_sum_p2 %>%
  filter(file_name == "Fist_30C") %>% 
  mutate(r_concentration = as.numeric(r_concentration)) 
f30C_mod <- nls(mu ~ umax * (r_concentration / (ks + r_concentration)),
                data = f_30C,
                start = list(ks = 0.5, umax = 0.5),
                algorithm = "port",
                control = nls.control(maxiter = 500, minFactor = 1/204800000))
b_f30C <- nlsBoot(f30C_mod)
b_f30C$bootCI

s_21C <- growth_sum_p2 %>%
  filter(file_name == "Scen_21C") %>% 
  mutate(r_concentration = as.numeric(r_concentration)) 
s21C_mod <- nls(mu ~ umax * (r_concentration / (ks + r_concentration)),
                data = s_21C,
                start = list(ks = 0.5, umax = 0.5),
                algorithm = "port",
                control = nls.control(maxiter = 500, minFactor = 1/204800000))
b_s21C <- nlsBoot(s21C_mod)
b_s21C$bootCI

s_8C <- growth_sum_p2 %>%
  filter(file_name == "Scen_8C") %>% 
  mutate(r_concentration = as.numeric(r_concentration)) 
s8C_mod <- nls(mu ~ umax * (r_concentration / (ks + r_concentration)),
                data = s_8C,
                start = list(ks = 0.5, umax = 0.5),
                algorithm = "port",
                control = nls.control(maxiter = 500, minFactor = 1/204800000))
b_s8C <- nlsBoot(s8C_mod)
b_s8C$bootCI

s_30C <- growth_sum_p2 %>%
  filter(file_name == "Scen_30C") %>% 
  mutate(r_concentration = as.numeric(r_concentration)) 
s30C_mod <- nls(mu ~ umax * (r_concentration / (ks + r_concentration)),
               data = s_30C,
               start = list(ks = 0.5, umax = 0.5),
               algorithm = "port",
               control = nls.control(maxiter = 500, minFactor = 1/204800000))
b_s30C <- nlsBoot(s30C_mod)
b_s30C$bootCI

## make the dataframe of them all together and then assign a file name?
f_21C_df <- data.frame(File_Name = "Fist_21C", CI = b_f21C$bootCI)
f_8C_df <- data.frame(File_Name = "Fist_8C", CI = b_f8C$bootCI)
f_30C_df <- data.frame(File_Name = "Fist_30C", CI = b_f30C$bootCI)
s_21C_df <- data.frame(File_Name = "Scen_21C", CI = b_s21C$bootCI)
s_8C_df <- data.frame(File_Name = "Scen_8C", CI = b_s8C$bootCI)
s_30C_df <- data.frame(File_Name = "Scen_30C", CI = b_s30C$bootCI)

# Combine dataframes into one
all_bootCI_df <- rbind(f_21C_df, f_8C_df, f_30C_df, s_21C_df, s_8C_df, s_30C_df)
## first column how do i remove?

#estimate of error in fit of curve - confidence intervals use to compare between treatments - plot them and if standard errors overlap or not - 
#indistingusihable if they do overlap  , different from one another if they dont overlap 
#two plots, one is ks and one is umax 

## how to separate that first row - add colours - coloumn that says parameters to separate by 
all_bootCI_df %>%
  ggplot(aes(x = File_Name, y = CI.Median)) +
  #ggplot(aes(x = File_Name, y = CI.Median, colour = parameters)) +
  geom_point(size = 4) +
  #facet_wrap(~parameters) +
  geom_errorbar(aes(ymin= CI.2.5., ymax= CI.97.5.), width=.2) +
  labs(x = 'treatment',
       y = 'estimate') +
  scale_color_manual(values = c(ks = "#9e0142", umax = "sienna1"))

parameters <- row.names(all_bootCI_df)

newdf <- cbind(parameters, all_bootCI_df) %>% 
  clean_names()

newdf[newdf == "ks1"| newdf == "ks2"| newdf == "ks3"| newdf == "ks4"| newdf == "ks5"] <- "ks"
newdf[newdf == "umax1"| newdf == "umax2"| newdf == "umax3"| newdf == "umax4"| newdf == "umax5"] <- "umax"

newdf %>%
  #filter(File_Name != "Fist_8C") %>%
  ggplot(aes(x = File_Name, y = CI.Median, colour = parameters)) +
  geom_point(size = 4) +
  facet_wrap(~parameters, scales = "free_y") +
  geom_errorbar(aes(ymin= CI.2.5., ymax= CI.97.5.), width=.2) +
  labs(x = 'treatment',
       y = 'estimate') +
  scale_color_manual(values = c(ks = "sienna1", umax = "#5e4fa2"))


# c("#9e0142","sienna1","#fbcf51","#4bc425","#3288bd","#5e4fa2")

## trying a different bootstrapping method --------
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



