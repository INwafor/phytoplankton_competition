source("code/02_data_clean.R")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) 
library(janitor)
library(lubridate)
library(broom)
library(viridis)
library(ggsci)
library(RColorBrewer)

theme_set(theme_cowplot())

#Graphing
#Fistulifera 21C
Fist_21C <- final_merge %>%
  filter(grepl("Fist_21C", spec_temp))

Fist_21C <- Fist_21C %>%
  mutate(r_concentration = factor(r_concentration))

Fist_21C %>% 
  filter(r_concentration != 0) %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = r_concentration)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y")+ scale_color_npg()+
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Fist_21C r*")
#ggsave("figures/Fist_21C_logged", width = 25, height = 20)

Fist_21C_growth <- Fist_21C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(rfu) ~ day, data = .))) 

Fist_21C_growth2 <- Fist_21C_growth %>% 
  left_join(plate_layout)

concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

Fist_21C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level") +
  ggtitle ("growth rate - Fist 21C")

##Fistulifera 30C
Fist_30C <- final_merge %>%
  filter(grepl("Fist_30C", spec_temp))

Fist_30C %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = r_concentration)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y")+ scale_color_viridis(option = "plasma") +
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Fist_30C r*")
#ggsave("figures/Fist_30C_logged", width = 25, height = 20)

Fist_30C_growth <- Fist_30C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(rfu) ~ day, data = .))) 

Fist_30C_growth2 <- Fist_30C_growth %>% 
  left_join(plate_layout)

Fist_30C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level") +
  ggtitle ("growth rate - Fist 30C")

##Fistulifera 8C
Fist_8C <- final_merge %>%
  filter(grepl("Fist_8C", spec_temp))

Fist_8C %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = r_concentration)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y")+ scale_color_viridis() +
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Fist_8C r*")
#ggsave("figures/Fist_8C_logged", width = 25, height = 20)

Fist_8C_growth <- Fist_8C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(rfu) ~ day, data = .))) 

Fist_8C_growth2 <- Fist_8C_growth %>% 
  left_join(plate_layout)

Fist_8C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level") +
  ggtitle ("growth rate - Fist 8C")

