#March 25th, 2024 - Ijeoma Nwafor thesis 
##looking at competition data - make a box plot 

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) 
library(janitor)
library(lubridate)

comp <- read_excel("data/comp_phycoprobe.xlsx") %>% 
  clean_names() %>%
  rename("green_algae_ug" = green_algae_4,
         "green_algae_ml" = green_algae_5,
         "diatom_ug" = diatoms_6,
         "diatom_ml" = diatoms_7,
         "tot_conc_ug" = total_conc,
         "tot_density_ml" = total_cell_count)

comp <- comp[!(row.names(comp) %in% c("1")),]

##average each replicate reads - so then we have 1 "read" per then for all the cold / hot

