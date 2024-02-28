##feb 13th 
##did a mixed plate trial at all three temperatures 
##killed off the 8C project a few days in - didn;t do all the days, only a fews
##trial grown at 6uM of P

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) 
library(janitor)
library(lubridate)
theme_set(theme_cowplot())


plate_layout_c <- read_excel("data/plate_template.xlsx", sheet = "competition_plate") %>%
  mutate(R_Concentration = as.numeric(R_Concentration))