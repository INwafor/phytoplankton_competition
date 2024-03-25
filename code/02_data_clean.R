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

all_times <- map_df(RFU_files, read_excel, range = "A7:A8", .id = "file_name") %>% 
  clean_names()

##fixing times
all_times <- all_times %>% separate(file_name, into = c("data", "location", "loc_2", "file_name", "temp", "read"), remove = FALSE)%>%
  select(-data,
         -location,
         -loc_2)

all_times <- all_times %>%
  rename("day_0" = "date_2_14_2024",
         "day_1" = "date_2_15_2024",
         "day_2" = "date_2_16_2024",
         "day_3" = "date_2_17_2024")

all_times$`day_0` <- gsub("Time: ", "", as.character(all_times$`day_0`))
all_times$`day_1` <- gsub("Time: ", "", as.character(all_times$`day_1`))
all_times$`day_2` <- gsub("Time: ", "", as.character(all_times$`day_2`))
all_times$`day_3` <- gsub("Time: ", "", as.character(all_times$`day_3`))


##LOOK AT IT BY READS INSTEAD OF BY DAY- time elapsed in units of days divide number of hours by 24 = unit of days - 
#then use that in the equation to calculate growth rates
##time elaspsed from first read add column 
#time_elapsed_units = the dividing by 24 

## this stuff didnt work 
library(purrr)

# set new date for everything
#date_0 <- as.POSIXct("2024-02-14", format = "%Y-%m-%d")
#date_1 <- as.POSIXct("2024-02-15", format = "%Y-%m-%d")
#date_2 <- as.POSIXct("2024-02-16", format = "%Y-%m-%d")
#date_3 <- as.POSIXct("2024-02-17", format = "%Y-%m-%d") 
  
#all_times$day_0_POSIXct <- as.POSIXct(paste(date_0, all_times$day_0), format = "%Y-%m-%d %H:%M:%S")
#all_times$day_1_POSIXct <- as.POSIXct(paste(date_1, all_times$day_1), format = "%Y-%m-%d %H:%M:%S")
#all_times$day_2_POSIXct <- as.POSIXct(paste(date_2, all_times$day_2), format = "%Y-%m-%d %H:%M:%S")
#all_times$day_3_POSIXct <- as.POSIXct(paste(date_3, all_times$day_3), format = "%Y-%m-%d %H:%M:%S")

times_dates <- all_times %>% 
  select(-day_0,
         -day_1,
         -day_2,
         -day_3)

#make a column with these numbers and assign them to reads , in hours/ minutes 
reads_times <- c(0, 18.40, 22.40, 26.10, 44.25, 47.55, 52.25, 69.10, 73, 76.20)

reads_times2 <- data.frame(reads_times = c(0, 18.40, 22.40, 26.10, 44.25, 47.55, 52.25, 69.10, 73, 76.20))

# Define the corresponding reads
reads_times2 <- reads_times2 %>%
  mutate(
    read = case_when(
      reads_times == 0 ~ "00",
      reads_times == 18.40 ~ "01",
      reads_times == 22.40 ~ "02",
      reads_times == 26.10 ~ "03",
      reads_times == 44.25 ~ "04",
      reads_times == 47.55 ~ "05",
      reads_times == 52.25 ~ "06",
      reads_times == 69.10 ~ "07",
      reads_times == 73 ~ "08",
      reads_times == 76.20 ~ "09",
      TRUE ~ NA_character_  # Default value if no condition is met
    )
  )

all_times2 <- dplyr::left_join(times_dates, reads_times2, by = "read")%>%
  select(-day_0_POSIXct,
         -day_1_POSIXct,
         -day_2_POSIXct,
         -day_3_POSIXct)

#remove the date columns and then join to all plates?
alltimes_2 <- all_times2 %>%
    mutate(
      day = case_when(
        read == "00" ~ 0,
        read == "01" ~ 1,
        read == "02" ~ 1,
        read == "03" ~ 1,
        read == "04" ~ 2,
        read == "05" ~ 2,
        read == "06" ~ 2,
        read == "07" ~ 3,
        read == "08" ~ 3,
        read == "09" ~ 3,
        TRUE ~ NA_integer_  # Default value if no condition is met
      )
    )
alltimes_3 <- alltimes_2 %>%
  mutate(
    time_elapsed_units = reads_times / 24
  )

alltimes_3 <- unite(alltimes_3, file_name, c(file_name, temp, read))

#POSITct code that didn't work - come back and try later
{
time_difference_1 <- difftime(times_dates$day_1_POSIXct, times_dates$day_0_POSIXct, units = "hours")
print(time_difference_1)

time_to_numeric <- function(time_string) {
    parts <- strsplit(time_string, ":")[[1]]
    hours <- as.numeric(parts[1])
    minutes <- as.numeric(parts[2])
    seconds <- as.numeric(parts[3])
    total_seconds <- hours * 3600 + minutes * 60 + seconds
    return(total_seconds)
  }
  
  
  # Apply the time_to_numeric function to convert time strings to numeric (total seconds)
  times_dates$day_0_numeric <- sapply(times_dates$day_0_POSIXct, time_to_numeric)
  all_times$day_1_numeric <- sapply(all_times$day_1, time_to_numeric)
  all_times$day_2_numeric <- sapply(all_times$day_2, time_to_numeric)
  all_times$day_3_numeric <- sapply(all_times$day_3, time_to_numeric)
  
  # Calculate elapsed time in hours
  all_times$elapsed_day_0 <- c(0, diff(all_times$day_0_numeric) / 3600)
  
  merged_time$day_1_numeric <- sapply(merged_time$day_1_formatted, time_to_numeric)
  
  view(merged_time)
  
  
  merged_time$day_1 <- format(merged_time$day_1, format = "%H:%M:%S %p")
  merged_time$day_2 <- format(merged_time$day_2, format = "%H:%M:%S %p")
  merged_time$day_3 <- format(merged_time$day_3, format = "%H:%M:%S %p")
  merged_time$day_4 <- format(merged_time$day_4, format = "%H:%M:%S %p")
  
  view(merged_time)
  
  str(merged_time)
  print(merged_time[, c("day_1", "day_2", "day_3", "day_4")])
  
  ##make time increasing - this didnt work 
  merged_hours <- merged_time %>%
    mutate(
      time_sequence = pmap_dbl(select(., starts_with("day")), ~ as.numeric(difftime(..1, ..[1], units = "hours")))
    )
  
  ## only changed day 1, added todays date 
  for (i in 2:5) {
    all_merged[[i]] <- as.POSIXct(all_merged[[i]], format = "%H:%M:%S %p")
    
    print(sapply(all_merged[2:5], class))
  } 
  view(all_merged)
  
  # View the resulting data frame
  print(data)
  
  all_times_merged <- all_merged %>%
    mutate(`Day 1` = as.numeric(`Day 1`))
  
  str(all_times_merged)
  
  
  unite(col = date_time, Date, time, sep = " ") %>%
    mutate(date_time = ymd_hms(date_time)) %>% 
    
    mutate(start_time = min(date_time)) %>% 
    mutate(days = interval(start_time, date_time)/ddays(1))
}

all_plates2 <- dplyr::left_join(all_plates, alltimes_3, by = "file_name")

all_plates2 <- all_plates2 %>%
  mutate(row = as.character(row))

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
