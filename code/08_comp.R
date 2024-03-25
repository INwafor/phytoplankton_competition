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
  clean_names() 

comp <- comp[!(row.names(comp) %in% c("1")),]
