##Feb13th 
##worrk on second experiment trial 
# Rstar for Phosphate

##plan: start by making each of the sheets into a data frame - join those all together - thats one file of code
## next file make a path from this code and organize/clean up the data frame by species, make times/dates correct
## path from that code to make a new one that plots the R* by temperature and use that to analyze 
## finally one last script to work on the monod curves and fitting the data to growth models

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) 
library(janitor)
library(lubridate)
theme_set(theme_cowplot())

##have the plate layout which includes the well, treatment and resource level - NO NEED TO LEFT JOIN?
plate_layout_r <- read_excel("data/plate_template.xlsx", sheet = "rstar_plates") %>%
  mutate(R_Concentration = as.numeric(R_Concentration))

plate_layout_c <- read_excel("data/plate_template.xlsx", sheet = "competition_plate") %>%
  mutate(R_Concentration = as.numeric(R_Concentration))


RFU_files <- c(list.files("data/rstar 2", full.names = TRUE))

RFU_files <- RFU_files[grepl(".xls", RFU_files)]

names(RFU_files) <- RFU_files %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")

## what does this do - row = X__1 does not exist
all_plates <- map_df(RFU_files, read_excel, range = "B16:M23", .id = "file_name") 

%>%
  rename(row = X__1) %>% 
  filter(!grepl("dilution", file_name)) %>% 
  mutate(file_name = str_replace(file_name, " ", ""))

