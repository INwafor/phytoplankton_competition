##feb 21st 
## RFUS FOR FISTULIFERA

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) 
library(janitor)
library(lubridate)
theme_set(theme_cowplot())

##drop "R_Concentration column, remove first row 
plate_layout_r <- read_excel("data/plate_template.xlsx", sheet = "rstar_plates") %>%
  clean_names() %>%
  mutate(r_concentration = as.numeric(r_concentration))

plate_layout_r <- plate_layout_r[!(row.names(plate_layout_r) %in% c("1")),]
view(plate_layout)
