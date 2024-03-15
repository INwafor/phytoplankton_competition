##third experiment trial - March 11th 

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

RFU_files2 <- c(list.files("data/rstar2_dataraw", full.names = TRUE))

RFU_files2 <- RFU_files2[grepl(".xls", RFU_files2)]

names(RFU_files2) <- RFU_files2 %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")

all_plates <- map_df(RFU_files2, read_excel, range = "A15:M23", .id = "file_name")%>%
  rename(row = ...1) %>% 
  mutate(file_name = str_replace(file_name, " ", ""))%>%
  separate(file_name, into = c("data", "location", "loc_2", "file_name", "temp", "read"), remove = FALSE)%>%
  select(-data,
         -location,
         -loc_2)

all_plates <- unite(all_plates, file_name, c(file_name, temp, read))
view(all_plates)

all_times <- map_df(RFU_files2, read_excel, range = "A7:A8", .id = "file_name") %>% 
  clean_names()

all_times <- all_times %>%
  rename("Day 0" = "date_3_11_2024",
         "Day 1" = "date_3_12_2024",
         "Day 2" = "date_3_13_2024",
         "Day 3" = "date_3_14_2024")

all_times <- all_times %>% separate(file_name, into = c("data", "location", "loc_2", "file_name", "temp", "read"), remove = FALSE) %>%
  select(-data,
         -location,
         -loc_2)

all_times$`Day 0` <- gsub("Time: ", "", as.character(all_times$`Day 0`))
all_times$`Day 1` <- gsub("Time: ", "", as.character(all_times$`Day 1`))
all_times$`Day 2` <- gsub("Time: ", "", as.character(all_times$`Day 2`))
all_times$`Day 3` <- gsub("Time: ", "", as.character(all_times$`Day 3`))

all_times <- unite(all_times, file_name, c(file_name, temp, read))

all_plates2 <- dplyr::left_join(all_plates, all_times, by = "file_name")

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
march15_rfus <- left_join(all_temp_RFU, plate_layout, by = "well") %>%
  clean_names()

file_hours <- all_times %>%
  clean_names() %>%
  separate(file_name, into = c("spec", "temp", "read"), remove = FALSE)

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
      TRUE ~ NA_integer_  # Default value if no condition is met
    )
  )

## merge file_name all back together
all_times_cleaned <- unite(file_hours_new, spec_temp, c(spec, temp))

#now add this into the all_merged by day_1
march15_rfus_final <- left_join(march15_rfus, all_times_cleaned, by = "file_name")

march15_rfus_final <- march15_rfus_final %>%
  select(-day_1.x,
         -day_2.x,
         -day_3.x,
         -day_0.x,
         -day_1.y,
         -day_2.y,
         -day_3.y,
         -day_0.y,
         -file_name)

##plotting rfus for all three plates 
library(viridis)
library(ggsci)
library(RColorBrewer)
library(janitor)
library(lubridate)
library(broom)
#Fist_21C
Fist_21C <- march15_rfus_final %>%
  filter(grepl("Fist_21C", spec_temp))

Fist_21C <- Fist_21C %>%
  mutate(r_concentration = factor(r_concentration))

Fist_21C %>% 
  filter(r_concentration != 0) %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = r_concentration)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y")+ scale_color_npg()+
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Fist_21C r*")
#ggsave("figures/fist_21C_logged", width = 15, height = 10)

Fist_21C_growth <- Fist_21C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(rfu) ~ day, data = .))) 

##filter out (intercept)
Fist_21C_growth2 <- Fist_21C_growth %>% 
  left_join(plate_layout) %>%
  filter(term == "day")

concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

Fist_21C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level") +
  ggtitle ("growth rate - Fist 21C")

#Fist_30C
Fist_30C <- march15_rfus_final %>%
  filter(grepl("Fist_30C", spec_temp))

Fist_30C <- Fist_30C %>%
  mutate(r_concentration = factor(r_concentration))

Fist_30C %>% 
  filter(r_concentration != 0) %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = r_concentration)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y")+ scale_color_npg()+
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Fist_30C r*")
#ggsave("figures/fist_30C_logged", width = 15, height = 10)

Fist_30C_growth <- Fist_30C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(rfu) ~ day, data = .))) 

##filter out (intercept)
Fist_30C_growth2 <- Fist_30C_growth %>% 
  left_join(plate_layout) %>%
  filter(term == "day")

concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

Fist_30C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level") +
  ggtitle ("growth rate - Fist 30C")

#Scen_8C
Scen_8C <- march15_rfus_final %>%
  filter(grepl("Scen_8C", spec_temp))

Scen_8C <- Scen_8C %>%
  mutate(r_concentration = factor(r_concentration))

Scen_8C %>% 
  filter(r_concentration != 0) %>% 
  ggplot(aes(x = day, y = log(rfu), group = well, color = r_concentration)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y")+ scale_color_npg()+
  ylab("log(rfu)") + xlab("day") +
  ggtitle("logged Scen_8C r*")
#ggsave("figures/Scen_8C_logged", width = 15, height = 10)

Scen_8C_growth <- Scen_8C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(rfu) ~ day, data = .))) 

##filter out (intercept)
Scen_8C_growth2 <- Scen_8C_growth %>% 
  left_join(plate_layout) %>%
  filter(term == "day")

concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

Scen_8C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level") +
  ggtitle ("growth rate - Scen_8C")
