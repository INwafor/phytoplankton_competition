#March 25th, 2024 - Ijeoma Nwafor thesis 
##looking at competition data - make a box plot 

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) 
library(janitor)
library(lubridate)

##this is the averages already cleaned up 
comp <- read_excel("data/comp_phycoprobe.xlsx", sheet = "clean_avg") %>% 
  clean_names() %>%
  rename("green_algae_ug" = green_algae_avg_3,
         "green_algae_ml" = green_algae_avg_4,
         "diatom_ug" = diatoms_avg_5,
         "diatom_ml" = diatoms_avg_6,
         "tot_conc_ug" = avg_total_conc,
         "tot_density_ml" = avg_tot_cell_count)

comp <- comp[!(row.names(comp) %in% c("1")),]

comp <- mutate_at(comp, vars(replicate, green_algae_ug, green_algae_ml, diatom_ug, diatom_ml, tot_conc_ug, tot_density_ml), as.numeric)

##now lets plot using a box plot!



{
##how to make this work for all the reps
##average each replicate reads - so then we have 1 "read" per then for all the cold / hot
#make separate tibbles for each 
#COLD
C1 <- subset(comp, temp == 'C' & replicate == 1)



#this finds the average for reach read - repeat for all ?  
C1_avg <- mean(C1$green_algae_ug[1:3], na.rm = TRUE)


C1_avg_g <- mean(C1$green_algae_ml[1:3], na.rm = TRUE)

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
}