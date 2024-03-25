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
  ylab("log(RFU)") + 
  xlab("Time elapsed (units of days)") +
  ggtitle("Logged Fist_21C r*") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Fist_21C_growth <- Fist_21C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
    do(tidy(lm(log(RFU) ~ time_elapsed_units, data = .))) 

Fist_21C_growth2 <- Fist_21C_growth %>% 
  left_join(plate_layout)

concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

Fist_21C_growth2 %>% 
  filter(term == "time_elapsed_units") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level") +
  ggtitle ("growth rate - Fist 21C")

