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

#Graphing - REDOING ON MARCH 25TH
#scenedesmus 21C
Scen_21C <- all_merged %>%
  filter(grepl("Scen_21C", file_name))

Scen_21C <- Scen_21C %>%
  mutate(r_concentration = factor(r_concentration))

Scen_21C <- Scen_21C %>%
  mutate(time_elapsed_units = as.numeric(as.character(time_elapsed_units))) %>%  # Convert to numeric
  mutate(time_elapsed_units = round(time_elapsed_units, 2)) %>%  # Round to 2 decimal places
  mutate(time_elapsed_units = as.factor(time_elapsed_units))

Scen_21C %>%
  filter(r_concentration != 0) %>%
  ggplot(aes(x = time_elapsed_units, y = log(RFU), group = well, color = factor(r_concentration))) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y") + 
  scale_color_npg() +
  ylab("Log(RFU)") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  #ggsave("figures/Scen_21C_logged.png", width = 15, height = 10)

Scen_21C_growth <- Scen_21C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(RFU) ~ day, data = .))) 

Scen_21C_growth2 <- Scen_21C_growth %>% 
  left_join(plate_layout) %>% 
  filter(term == "day")

concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

Scen_21C_growth2 %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("Growth rate (per day)") + xlab("Resource level")  
  #ggsave("figures/Scen_21C_growthrate.png", width = 15, height = 10)

##scenedemsus 8C
Scen_8C <- all_merged %>%
  filter(grepl("Scen_8C", file_name))

Scen_8C <- Scen_8C %>%
  mutate(r_concentration = factor(r_concentration))

Scen_8C <- Scen_8C %>%
  mutate(time_elapsed_units = as.numeric(as.character(time_elapsed_units))) %>%  # Convert to numeric
  mutate(time_elapsed_units = round(time_elapsed_units, 2)) %>%  # Round to 2 decimal places
  mutate(time_elapsed_units = as.factor(time_elapsed_units))

Scen_8C %>%
  filter(r_concentration != 0) %>%
  ggplot(aes(x = time_elapsed_units, y = log(RFU), group = well, color = factor(r_concentration))) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y") + 
  scale_color_npg() +
  ylab("Log(RFU)") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  #ggsave("figures/Scen_8C_logged.png", width = 15, height = 10)

Scen_8C_growth <- Scen_8C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(RFU) ~ day, data = .))) 

Scen_8C_growth2 <- Scen_8C_growth %>% 
  left_join(plate_layout) %>% 
  filter(term == "day")

Scen_8C_growth2 %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("Growth rate (per day)") + xlab("Resource level")  
  #ggsave("figures/Scen_8C_growthrate.png", width = 15, height = 10)

##Scenedesmus 30C
Scen_30C <- all_merged %>%
  filter(grepl("Scen_30C", file_name))

Scen_30C <- Scen_30C %>%
  mutate(r_concentration = factor(r_concentration))

Scen_30C <- Scen_30C %>%
  mutate(time_elapsed_units = as.numeric(as.character(time_elapsed_units))) %>%  # Convert to numeric
  mutate(time_elapsed_units = round(time_elapsed_units, 2)) %>%  # Round to 2 decimal places
  mutate(time_elapsed_units = as.factor(time_elapsed_units))

Scen_30C %>%
  filter(r_concentration != 0) %>%
  ggplot(aes(x = time_elapsed_units, y = log(RFU), group = well, color = factor(r_concentration))) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y") + 
  scale_color_npg() +
  ylab("Log(RFU)") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  #ggsave("figures/Scen_30C_logged.png", width = 15, height = 10)

Scen_30C_growth <- Scen_30C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(RFU) ~ day, data = .))) 

Scen_30C_growth2 <- Scen_30C_growth %>% 
  left_join(plate_layout) %>% 
  filter(term == "day")

Scen_30C_growth2 %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("Growth rate (per day)") + xlab("Resource level")  
  #ggsave("figures/Scen_30C_growthrate.png", width = 15, height = 10)
