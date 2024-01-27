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
jan27_scen <- read_excel("data/Rstar_experiment.xlsx", sheet = "scenedesmus_21C")%>%
  mutate(unique_well = paste(Well, hour, Treatment, sep = "_"))

jan27_scen <- jan27_scen[!(row.names(jan27_scen) %in% c("1")),]

jan27_scen %>% 
  ggplot(aes( x = hour, y = RFU, colour = Treatment, group = unique_well))+
  geom_point(aes(shape = Treatment), size = 1.5) +
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
