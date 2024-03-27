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

#Graphing - this is the code updated on mar 24th - repeat for all species 
#Fistulifera 21C
Fist_21C <- all_merged %>%
  filter(grepl("Fist_21C", file_name))

Fist_21C <- Fist_21C %>%
  mutate(r_concentration = factor(r_concentration))

Fist_21C <- Fist_21C %>%
  mutate(time_elapsed_units = as.numeric(as.character(time_elapsed_units))) %>%  # Convert to numeric
  mutate(time_elapsed_units = round(time_elapsed_units, 2)) %>%  # Round to 2 decimal places
  mutate(time_elapsed_units = as.factor(time_elapsed_units))

Fist_21C %>%
  filter(r_concentration != 0) %>%
  ggplot(aes(x = time_elapsed_units, y = log(RFU), group = well, color = factor(r_concentration))) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y") + 
  scale_color_npg() +
  ylab("Log(RFU)") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave("figures/fist_21C_logged.png", width = 15, height = 10)

Fist_21C_growth <- Fist_21C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
    do(tidy(lm(log(RFU) ~ day, data = .))) 

Fist_21C_growth2 <- Fist_21C_growth %>% 
  left_join(plate_layout)

concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

Fist_21C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("Growth rate (per day)") + xlab("Resource level") 
  #ggsave("figures/fist_21C_growthrate.png", width = 15, height = 10)

##Fistulifera 8C
Fist_8C <- all_merged %>%
  filter(grepl("Fist_8C", file_name))

Fist_8C <- Fist_8C %>%
  mutate(r_concentration = factor(r_concentration))

Fist_8C <- Fist_8C %>%
  mutate(time_elapsed_units = as.numeric(as.character(time_elapsed_units))) %>%  # Convert to numeric
  mutate(time_elapsed_units = round(time_elapsed_units, 2)) %>%  # Round to 2 decimal places
  mutate(time_elapsed_units = as.factor(time_elapsed_units))

Fist_8C %>%
  filter(r_concentration != 0) %>%
  ggplot(aes(x = time_elapsed_units, y = log(RFU), group = well, color = factor(r_concentration))) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y") + 
  scale_color_npg() +
  ylab("Log(RFU)") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  #ggsave("figures/fist_8C_logged.png", width = 15, height = 10)

Fist_8C_growth <- Fist_8C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(RFU) ~ day, data = .))) 

Fist_8C_growth2 <- Fist_8C_growth %>% 
  left_join(plate_layout)

Fist_8C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("Growth rate (per day)") + xlab("Resource level") 
  #ggsave("figures/fist_8C_growthrate.png", width = 15, height = 10)

##Fistulifera 30C
Fist_30C <- all_merged %>%
  filter(grepl("Fist_30C", file_name))

Fist_30C <- Fist_30C %>%
  mutate(r_concentration = factor(r_concentration))

Fist_30C <- Fist_30C %>%
  mutate(time_elapsed_units = as.numeric(as.character(time_elapsed_units))) %>%  # Convert to numeric
  mutate(time_elapsed_units = round(time_elapsed_units, 2)) %>%  # Round to 2 decimal places
  mutate(time_elapsed_units = as.factor(time_elapsed_units))

Fist_30C %>%
  filter(r_concentration != 0) %>%
  ggplot(aes(x = time_elapsed_units, y = log(RFU), group = well, color = factor(r_concentration))) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y") + 
  scale_color_npg() +
  ylab("Log(RFU)") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  #ggsave("figures/fist_30C_logged.png", width = 15, height = 10)

Fist_30C_growth <- Fist_30C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(RFU) ~ day, data = .))) 

Fist_30C_growth2 <- Fist_30C_growth %>% 
  left_join(plate_layout)

Fist_30C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("Growth rate (per day)") + xlab("Resource level") 
  #ggsave("figures/fist_30C_growthrate.png", width = 15, height = 10)

