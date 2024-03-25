##third experiment trial - March 11th 

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) 
library(janitor)
library(viridis)
library(ggsci)
library(RColorBrewer)
library(lubridate)
library(broom)
theme_set(theme_cowplot())

##march 25th - redoing for mar25
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


all_times <- map_df(RFU_files2, read_excel, range = "A7:A8", .id = "file_name") %>% 
  clean_names()

##fixing times
all_times <- all_times %>% separate(file_name, into = c("data", "location", "loc_2", "file_name", "temp", "read"), remove = FALSE)%>%
  select(-data,
         -location,
         -loc_2)

all_times <- all_times %>%
  rename("day_0" = "date_3_11_2024",
         "day_1" = "date_3_12_2024",
         "day_2" = "date_3_13_2024",
         "day_3" = "date_3_14_2024")

all_times$`day_0` <- gsub("Time: ", "", as.character(all_times$`day_0`))
all_times$`day_1` <- gsub("Time: ", "", as.character(all_times$`day_1`))
all_times$`day_2` <- gsub("Time: ", "", as.character(all_times$`day_2`))
all_times$`day_3` <- gsub("Time: ", "", as.character(all_times$`day_3`))

##up 2 date till now 

reads_times <- c(0,18.3,22.5,26.3,42.2,46.2,50.4,66.2,69.25,73)

reads_times2 <- data.frame(reads_times = c(0,18.3,22.5,26.3,42.2,46.2,50.4,66.2,69.25,73))

# Define the corresponding reads
reads_times2 <- reads_times2 %>%
  mutate(
    read = case_when(
      reads_times == 0 ~ "00",
      reads_times == 18.3 ~ "01",
      reads_times == 22.5 ~ "02",
      reads_times == 26.3 ~ "03",
      reads_times == 42.2 ~ "04",
      reads_times == 46.2 ~ "05",
      reads_times == 50.4 ~ "06",
      reads_times == 66.2 ~ "07",
      reads_times == 69.25 ~ "08",
      reads_times == 73 ~ "09",
      TRUE ~ NA_character_  # Default value if no condition is met
    )
  )

all_times2 <- dplyr::left_join(all_times, reads_times2, by = "read")

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
all_merged2 <- left_join(all_temp_RFU, plate_layout, by = "well")


##plotting rfus for all three plates 
#Fist_21C
Fist_21C <- all_merged2 %>%
  filter(grepl("Fist_21C", file_name))

Fist_21C <- Fist_21C %>%
  mutate(r_concentration = factor(r_concentration))

Fist_21C <- Fist_21C %>%
  mutate(time_elapsed_units = as.numeric(as.character(time_elapsed_units))) %>%  # Convert to numeric
  mutate(time_elapsed_units = round(time_elapsed_units, 2)) %>%  # Round to 2 decimal places
  mutate(time_elapsed_units = as.factor(time_elapsed_units))

Fist_21C %>%
  filter(r_concentration != 0) %>%
  ggplot(aes(x = time_elapsed_units, y = log(RFU), group = well, color = factor(r_concentration))) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y") + 
  scale_color_npg() +
  ylab("Log(RFU)") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave("figures/Fist_21C_logged2.png", width = 15, height = 10)

Fist_21C_growth <- Fist_21C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(RFU) ~ day, data = .))) 

Fist_21C_growth2 <- Fist_21C_growth %>% 
  left_join(plate_layout)

concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

Fist_21C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("Growth rate (per day)") + xlab("Resource level") + 
  ggsave("figures/Fist_21C_growthrate2.png", width = 15, height = 10)

##Scen 8C
Scen_8C <- all_merged2 %>%
  filter(grepl("Scen_8C", file_name))

Scen_8C <- Scen_8C %>%
  mutate(r_concentration = factor(r_concentration))

Scen_8C <- Scen_8C %>%
  mutate(time_elapsed_units = as.numeric(as.character(time_elapsed_units))) %>%  # Convert to numeric
  mutate(time_elapsed_units = round(time_elapsed_units, 2)) %>%  # Round to 2 decimal places
  mutate(time_elapsed_units = as.factor(time_elapsed_units))

Scen_8C %>%
  filter(r_concentration != 0) %>%
  ggplot(aes(x = time_elapsed_units, y = log(RFU), group = well, color = factor(r_concentration))) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y") + 
  scale_color_npg() +
  ylab("Log(RFU)") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggsave("figures/Scen_8C_logged2.png", width = 15, height = 10)

Scen_8C_growth <- Scen_8C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(RFU) ~ day, data = .))) 

Scen_8C_growth2 <- Scen_8C_growth %>% 
  left_join(plate_layout)

Scen_8C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("Growth rate (per day)") + xlab("Resource level") + 
  ggsave("figures/Scen_8C_growthrate2.png", width = 15, height = 10)

##Fistulifera 30C
Fist_30C <- all_merged2 %>%
  filter(grepl("Fist_30C", file_name))

Fist_30C <- Fist_30C %>%
  mutate(r_concentration = factor(r_concentration))

Fist_30C <- Fist_30C %>%
  mutate(time_elapsed_units = as.numeric(as.character(time_elapsed_units))) %>%  # Convert to numeric
  mutate(time_elapsed_units = round(time_elapsed_units, 2)) %>%  # Round to 2 decimal places
  mutate(time_elapsed_units = as.factor(time_elapsed_units))

Fist_30C %>%
  filter(r_concentration != 0) %>%
  ggplot(aes(x = time_elapsed_units, y = log(RFU), group = well, color = factor(r_concentration))) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~r_concentration, scales = "free_y") + 
  scale_color_npg() +
  ylab("Log(RFU)") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave("figures/Fist_30C_logged2.png", width = 15, height = 10)

Fist_30C_growth <- Fist_30C %>% 
  filter(treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(RFU) ~ day, data = .))) 

Fist_30C_growth2 <- Fist_30C_growth %>% 
  left_join(plate_layout)

Fist_30C_growth2 %>% 
  filter(term == "day") %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  ylab("Growth rate (per day)") + xlab("Resource level") + 
  ggsave("figures/Fist_30C_growthrate2.png", width = 15, height = 10)
