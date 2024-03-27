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

comp <- mutate_at(comp, vars(replicate, read, green_algae_ug, green_algae_ml, diatom_ug, diatom_ml, tot_conc_ug, tot_density_ml), as.numeric)


##how to make this work for all the reps
##average each replicate reads - so then we have 1 "read" per then for all the cold / hot
#make separate tibbles for each 
#COLD
C1 <- subset(comp, temp == 'C' & replicate == 1)
C2 <- subset(comp, temp == 'C' & replicate == 2)
C3 <- subset(comp, temp == 'C' & replicate == 3)
C4 <- subset(comp, temp == 'C' & replicate == 4)
C5 <- subset(comp, temp == 'C' & replicate == 5)
C6 <- subset(comp, temp == 'C' & replicate == 6)
C7 <- subset(comp, temp == 'C' & replicate == 7)
C8 <- subset(comp, temp == 'C' & replicate == 8)


#this finds the average for reach read - repeat for all ?  
C1_avg <- mean(C1$green_algae_ug[1:3], na.rm = TRUE)
C2_avg <- mean(C2$green_algae_ug[1:3], na.rm = TRUE)
C3_avg <- mean(C3$green_algae_ug[1:3], na.rm = TRUE)
C4_avg <- mean(C4$green_algae_ug[1:3], na.rm = TRUE)
C5_avg <- mean(C5$green_algae_ug[1:3], na.rm = TRUE)
C6_avg <- mean(C6$green_algae_ug[1:3], na.rm = TRUE)
C7_avg <- mean(C7$green_algae_ug[1:3], na.rm = TRUE)
C8_avg <- mean(C8$green_algae_ug[1:3], na.rm = TRUE)


C1_avg_g <- mean(C1$green_algae_ml[1:3], na.rm = TRUE)
C2_avg_g <- mean(C2$green_algae_ml[1:3], na.rm = TRUE)
C3_avg_g <- mean(C3$green_algae_ml[1:3], na.rm = TRUE)
C4_avg_g <- mean(C4$green_algae_ml[1:3], na.rm = TRUE)
C5_avg_g <- mean(C5$green_algae_ml[1:3], na.rm = TRUE)
C6_avg_g <- mean(C6$green_algae_ml[1:3], na.rm = TRUE)
C7_avg_g <- mean(C7$green_algae_ml[1:3], na.rm = TRUE)
C8_avg_g <- mean(C8$green_algae_ml[1:3], na.rm = TRUE)
#- then how do i add them - make a tibble with temp, replciate, and average for green/diatom etc 



#from chat
# group by temperature and replicate, then calculate the mean for each combination
averages <- comp %>%
  group_by(temp, replicate) %>%
  summarise(
    avg_green_algae_ug = mean(green_algae_ug[1:3], na.rm = TRUE),
    avg_green_algae_ml = mean(green_algae_ml[1:3], na.rm = TRUE),
    avg_diatom_ug = mean(diatom_ug[1:3], na.rm = TRUE),
    avg_diatom_ml = mean(diatom_ml[1:3], na.rm = TRUE),
    avg_tot_conc_ug = mean(tot_conc_ug[1:3], na.rm = TRUE),
    avg_tot_density_ml = mean(tot_density_ml[1:3], na.rm = TRUE)
  )
