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
#Fistulifera 21C
Fist_21C <- final_merge %>%
  filter(grepl("Fist_21C", spec_temp))

Fist_21C %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = treatment)) + geom_line() +
  geom_point() +
  facet_wrap( ~ r_concentration) +
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged fist_21C r*")

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
  ggplot(aes(x = day, y = log(rfu), group = well, color = treatment)) + geom_line() +
  geom_point() +
  facet_wrap( ~ r_concentration) +
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged fist_30C r*")

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
  ggplot(aes(x = day, y = log(rfu), group = well, color = treatment)) + geom_line() +
  geom_point() +
  facet_wrap( ~ r_concentration) +
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged fist_8C r*")

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
