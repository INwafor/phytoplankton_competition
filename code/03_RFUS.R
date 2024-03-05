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

##graphing RFUS 
merged_time <- all_merged %>% 
  clean_names()
view(merged_time)

##using file_name - create a new df with times/ file 
# attach hour thing to file name ? maybe have to separate both dataframes 
# and then based off what read it is - assign the times to it in a new column?
file_hours <- all_times %>%
  clean_names() %>%
  separate(file_name, into = c("file_name", "temp", "read"), remove = FALSE)

view(file_hours)

file_hours_new <- file_hours %>%
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
      read == "1" ~ 1,
      read == "4" ~ 2,
      TRUE ~ NA_integer_  # Default value if no condition is met
    )
  )

## merge file_name all back together
all_times_cleaned <- unite(file_hours_new, file_name, c(file_name, temp, read))

#now add this into the all_merged by day_1
final_merge <- left_join(merged_time, all_times_cleaned, by = "file_name")

final_merge <- final_merge %>%
  select(-day_1.x,
         -day_2.x,
         -day_3.x,
         -day_4.x,
         -day_1.y,
         -day_2.y,
         -day_3.y,
         -day_4.y)

##bothering me that some file names are wrong - go in and change 
#the two save the commits and then push eveyrthing again

{
##change time format  can try this again if you need to change up the hours. 
  # mutate(days = Hour/24) %>% # not 
library(purrr)

merged_time$day_1_POSIXct <- as.POSIXct(merged_time$day_1, format = "%I:%M:%S %p")

# Format to "%H:%M:%S %p" and store in another column
merged_time$day_1_formatted <- format(merged_time$day_1_POSIXct, format = "%H:%M:%S")

time_to_numeric <- function(time_string) {
  parts <- strsplit(time_string, ":")[[1]]
  hours <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  total_seconds <- hours * 3600 + minutes * 60 + seconds
  return(total_seconds)
}

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