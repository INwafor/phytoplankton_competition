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
theme_set(theme_cowplot())

#Graphing
#Scenedesmus 21C
Scen_21C <- final_merge %>%
  filter(grepl("Scen_21C", spec_temp))

Scen_21C %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = treatment)) + geom_line() +
  geom_point() +
  facet_wrap( ~ r_concentration) +
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Scen_21C r*")

Scen_21C_growth <- Scen_21C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(rfu) ~ day, data = .))) 

Scen_21C_growth2 <- Scen_21C_growth %>% 
  left_join(plate_layout)

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
  ggplot(aes(x = day, y = log(rfu), group = well, color = treatment)) + geom_line() +
  geom_point() +
  facet_wrap( ~ r_concentration) +
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Scen_30C r*")

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
  ggplot(aes(x = day, y = log(rfu), group = well, color = treatment)) + geom_line() +
  geom_point() +
  facet_wrap( ~ r_concentration) +
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Scen_8C r*")

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
