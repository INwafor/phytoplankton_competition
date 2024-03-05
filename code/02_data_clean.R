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
plate_layout <- read_excel("data/plate_template.xlsx", sheet = "rstar_plates") %>%
  clean_names() %>%
  mutate(r_concentration = as.numeric(r_concentration)) %>%
  mutate(well = as.character(well))

plate_layout <- plate_layout[!(row.names(plate_layout) %in% c("1")),]

RFU_files <- c(list.files("data/rstar_dataraw", full.names = TRUE))

RFU_files <- RFU_files[grepl(".xls", RFU_files)]

names(RFU_files) <- RFU_files %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")

all_plates <- map_df(RFU_files, read_excel, range = "A15:M23", .id = "file_name")%>%
  rename(row = ...1) %>% 
  mutate(file_name = str_replace(file_name, " ", "")) %>%
  separate(file_name, into = c("data", "location", "loc_2", "file_name", "temp", "read"), remove = FALSE)%>%
  select(-data,
         -location,
         -loc_2)

all_plates <- unite(all_plates, file_name, c(file_name, temp, read))
view(all_plates)

all_times <- map_df(RFU_files, read_excel, range = "A7:A8", .id = "file_name") %>% 
  clean_names()
  
all_times <- all_times %>%
  rename("Day 1" = "date_2_14_2024",
         "Day 2" = "date_2_15_2024",
         "Day 3" = "date_2_16_2024",
         "Day 4" = "date_2_17_2024")

all_times <- all_times %>% separate(file_name, into = c("data", "location", "loc_2", "file_name", "temp", "read"), remove = FALSE) %>%
  select(-data,
         -location,
         -loc_2)

all_times$`Day 1` <- gsub("Time: ", "", as.character(all_times$`Day 1`))
all_times$`Day 2` <- gsub("Time: ", "", as.character(all_times$`Day 2`))
all_times$`Day 3` <- gsub("Time: ", "", as.character(all_times$`Day 3`))
all_times$`Day 4` <- gsub("Time: ", "", as.character(all_times$`Day 4`))

all_times <- unite(all_times, file_name, c(file_name, temp, read))

all_plates2 <- dplyr::left_join(all_plates, all_times, by = "file_name")

all_plates2 <- all_plates2 %>%
  mutate(row = as.character(row))

view(all_plates2)

## saying that the column isnt present ? where i get stuck trying to add in my well key layout
all_temp_RFU <- all_plates2 %>% 
  gather(key = row, value = RFU, 3:14) %>%
  mutate(row = as.character(row))

num_rows_all_plates2 <- nrow(all_plates2)
num_repeats <- ceiling(num_rows_all_plates2 / nrow(plate_layout))

# Repeat the rows in plate_layout accordingly
plate_layout_repeated <- plate_layout[rep(seq_len(nrow(plate_layout)), each = num_repeats), ]

all_temp_RFU <- all_plates2 %>% 
  gather(key = row, value = RFU, 3:14) %>%
  mutate(row = as.character(row)) 

letters_vec <- rep(c("A", "B", "C", "D", "E", "F", "G", "H"), length.out = nrow(all_temp_RFU))

# Add the letter column to your dataframe
all_temp_RFU$well <- letters_vec

  view(all_temp_RFU)
  
all_merged <- paste0(all_temp_RFU$well, all_temp_RFU$row)

##all merged says its 5760 
all_temp_RFU$well_key <- all_merged

repeated_well_key <- rep(well_key, length.out = nrow(all_temp_RFU))

# now we can drop the row and the well - then we have to add r_concentration - then you can graph
##removing row and well
all_temp_RFU <- subset(all_temp_RFU, select = -c(row, well))

colnames(all_temp_RFU)[colnames(all_temp_RFU) == "well_key"] <- "well"

##adding in R concentration - now we can left bind with plate layout
all_merged <- left_join(all_temp_RFU, plate_layout, by = "well")

## Joeys code - not using for this script anymore 
{
bind_cols(plate_layout_repeated[c("well", "treatment", "r_concentration")])
view(all_temp_RFU)

str(all_plates2)

str(plate_layout_repeated)

##if number 1 = a - h t

  unite(all_plates2, col = "well", remove = FALSE, sep = "") 
  mutate(column = formatC(column, width = 2, flag = 0)) %>% 
  mutate(column = str_replace(column, " ", "0")) %>% 
  unite(col = well, row, column, sep = "") %>% 
  filter(!is.na(RFU))



all_rfus_raw <- left_join(all_temp_RFU, plate_info, by = c("well"))



all_rfus2 <- all_rfus_raw %>%
  unite(col = date_time, Date, time, sep = " ") %>%
  mutate(date_time = ymd_hms(date_time)) %>% 
  mutate(population = ifelse(population == "cc1629", "COMBO", population))


all_rfus3 <- all_rfus2 %>% 
  group_by(n_level) %>% 
  mutate(start_time = min(date_time)) %>% 
  mutate(days = interval(start_time, date_time)/ddays(1)) %>% 
  unite(col = well_plate, well, plate, remove =  FALSE) %>% 
  separate(col = n_level, sep = 1, into = c("p", "phosphate_level")) %>% 
  mutate(phosphate_level = as.numeric(phosphate_level))
}
