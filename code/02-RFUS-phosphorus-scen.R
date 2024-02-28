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
plate_layout_s <- read_excel("data/plate_template.xlsx", sheet = "rstar_plates") %>%
  clean_names() %>%
  mutate(r_concentration = as.numeric(r_concentration))

plate_layout_s <- plate_layout_s[!(row.names(plate_layout_s) %in% c("1")),]

RFU_files <- c(list.files("data/rstar_dataraw", full.names = TRUE))

RFU_files <- RFU_files[grepl(".xls", RFU_files)]

names(RFU_files) <- RFU_files %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")

##what is all plates - what is it showing/why 
all_plates <- map_df(RFU_files, read_excel, range = "B16:M23", .id = "file_name") %>%
  mutate(file_name = str_replace(file_name, " ", ""))
view(all_plates)

all_times <- map_df(RFU_files, read_excel, range = "A7:A8", .id = "file_name") %>% 
  clean_names()

#separate by day? how to remove time:
all_times <- all_times %>%
  rename("Day 1" = "date_2_14_2024",
         "Day 2" = "date_2_15_2024",
         "Day 3" = "date_2_16_2024",
         "Day 4" = "date_2_17_2024")

##want to fix and remove time: crap but not change the column 
all_times <- all_times %>% separate(file_name, into = c("data", "location", "loc_2", "file_name", "temp", "read"), remove = FALSE) %>%
  select(-data,
         -location,
         -loc_2)
view(all_times)
 
