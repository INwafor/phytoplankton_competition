##Ijeoma Nwafor 
#Jan 27th 
# R* experiment from Jan 26th - Jan 29th; growth rates - then will fnid r*

#loading in packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)
library(cowplot)

## this is allowing me to see how they are declining/ some are growing - but i dont like this plot it produces
## average the wells and take that? remember to account for resource concentration!

#scenedesmus
jan29_scen <- read_excel("data/Rstar_experiment.xlsx", sheet = "scenedesmus_21C")%>%
  mutate(unique_well = paste(Well, hour,`Resource Concentration`, sep = "_"))

jan29_scen <- jan29_scen[!(row.names(jan29_scen) %in% c("1")),]

jan29_scen %>% 
  ggplot(aes( x = hour, y = RFU, colour = `Resource Concentration`, group = unique_well))+
  theme_minimal() +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(aes(x = hour, y = RFU, group = Well)) +
  ggtitle('Scenedesmus 24 hours')

#fistulifera
jan27_fist <- read_excel("data/Rstar_experiment.xlsx", sheet = "fistulifera_21C")%>% 
  mutate(unique_well = paste(Well, hour, `Resource Concentration (uM)`, sep = "_"))

jan27_fist <- jan27_fist[!(row.names(jan27_fist) %in% c("1")),]

jan27_fist %>% 
  ggplot(aes( x = hour, y = RFU, colour = `Resource Concentration (uM)`, group = unique_well))+
  theme_minimal() +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(aes(x = hour, y = RFU, group = Well)) +
  ggtitle('Fistulifera 24 hours')

## January 31st R* Analysis 
fistulifera <- read_excel("data/Rstar_experiment_final.xlsx", sheet = "fistulifera_21C") %>%
  mutate(unique_well = paste(Well, Hour, R_Concentration, sep = "_"))
fistulifera <- fistulifera[!(row.names(fistulifera) %in% c("1")),]

scenedesmus <- read_excel("data/Rstar_experiment_final.xlsx", sheet = "scenedesmus_21C") %>%
  mutate(unique_well = paste(Well, Hour, R_Concentration, sep = "_"))
scenedesmus <- scenedesmus[!(row.names(scenedesmus) %in% c("1")),]

## need to fix scale, remove NA, change colour and line thickness
scenedesmus %>%
  ggplot(aes(x = Hour, y = RFU, colour = Treatment, group = unique_wel))+
  theme_minimal() +
  facet_wrap(~R_Concentration, ncol = 4) +
  geom_line(aes(x = Hour, y = RFU, group = Well)) +
  ggtitle('Scenedesmus Phosphate Experiment 1')

