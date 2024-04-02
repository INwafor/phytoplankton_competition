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

##now lets plot using a box plot
##plot byt tempertaures to look at all the replciates and then will average those and put them togther to look at everything at once


# Reshape data for plotting
df_c_long <- reshape2::melt(comp, id.vars = c('temp', 'replicate'))
df_ug <- subset(df_c_long, variable %in% c("green_algae_ug", "diatom_ug", "tot_conc_ug"))

# Create box plot for green_algae_ug, diatom_ug, and tot_conc_ug
ggplot(df_ug, aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap(~ temp, scales = 'free_y') +
  labs(x = '', y = 'Algae Concentration (ug)')

# Subset the data for green_algae_ml, diatom_ml, and tot_conc_ml
df_ml <- subset(df_c_long, variable %in% c("green_algae_ml", "diatom_ml", "tot_density_ml"))

# Create box plot for green_algae_ml, diatom_ml, and tot_conc_ml
ggplot(df_ml, aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap(~ temp, scales = 'free_y') +
  labs(x = '', y = 'Algae Concentration (mL)') 

