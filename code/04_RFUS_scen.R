##graphing scenedesmus 

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
#Scenedesmus 21C
Scen_21C <- final_merge %>%
  filter(grepl("Scen_21C", spec_temp))

Scen_21C <- Scen_21C %>%
  mutate(r_concentration = factor(r_concentration))

Scen_21C %>% 
  filter(r_concentration != 0) %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = r_concentration)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y")+ scale_color_npg()+
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Scen_21C r*")
#ggsave("figures/scen_21C_logged", width = 15, height = 10)

Scen_21C_growth <- Scen_21C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(rfu) ~ day, data = .))) 

##filter out (intercept)
Scen_21C_growth2 <- Scen_21C_growth %>% 
  left_join(plate_layout) %>%
  filter(term == "day")

concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

Scen_21C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level") +
  ggtitle ("growth rate - Scen 21C")

##Scenedesmus 30C
Scen_30C <- final_merge %>%
  filter(grepl("Scen_30C", spec_temp))

Scen_30C %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = r_concentration)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y")+ scale_color_viridis(option = "plasma") +
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Scen_30C r*")
#ggsave("figures/scen_30C_logged", width = 15, height = 10)

Scen_30C_growth <- Scen_30C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(rfu) ~ day, data = .))) 

Scen_30C_growth2 <- Scen_30C_growth %>% 
  left_join(plate_layout)

Scen_30C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level") +
  ggtitle ("growth rate - Scen 30C")

##Scenedesmus 8C
Scen_8C <- final_merge %>%
  filter(grepl("Scen_8C", spec_temp))

Scen_8C %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = r_concentration)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y")+ scale_color_viridis() +
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Scen_8C r*")
#ggsave("figures/scen_8C_logged", width = 15, height = 10)

Scen_8C_growth <- Scen_8C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(rfu) ~ day, data = .))) 

Scen_8C_growth2 <- Scen_8C_growth %>% 
  left_join(plate_layout)

Scen_8C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level") +
  ggtitle ("growth rate - Scen 8C")

