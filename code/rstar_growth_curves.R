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
fulldataset <- read_excel("data/Rstar_experiment_final.xlsx") %>%
  mutate(unique_well = paste(Well, Hour, R_Concentration, sep = "_"))

fistulifera <- read_excel("data/Rstar_experiment_final.xlsx", sheet = "fistulifera_21C") %>%
  mutate(unique_well = paste(Well, Hour, R_Concentration, sep = "_"))
fistulifera <- fistulifera[!(row.names(fistulifera) %in% c("1")),]

scenedesmus <- read_excel("data/Rstar_experiment_final.xlsx", sheet = "scenedesmus_21C") %>%
  mutate(unique_well = paste(Well, Hour, R_Concentration, sep = "_"))
scenedesmus <- scenedesmus[!(row.names(scenedesmus) %in% c("1")),]

scen <- scenedesmus %>% 
  mutate(RFU = as.numeric(RFU)) %>% 
  mutate(Hour = as.numeric(Hour)) %>% 
  mutate(R_Concentration = as.numeric(R_Concentration))
str(scen)

## need to fix scale, remove NA, change colour and line thickness, give grid 
scen %>%
  ggplot(aes(x = Hour, y = RFU, colour = Treatment, group = unique_wel))+
  theme_light() +
  facet_wrap(~R_Concentration, ncol = 4) +
  geom_point(aes(x = Hour, y = RFU, group = Well)) +
  ggtitle('Scenedesmus Phosphate Experiment 1')

##fit to exponential growth / models - potential issue with media?   

fistulifera %>%
  ggplot(aes(x = Hour, y = RFU, colour = Treatment, group = unique_wel))+
  theme_light() +
  facet_wrap(~R_Concentration, ncol = 4) +
  geom_line(aes(x = Hour, y = RFU, group = Well)) +
  ggtitle('Fistulifera Phosphate Experiment 1')

#write.csv(fulldataset, "C:\\Users\\Ijeoma\\Desktop\\r_star_experiment.csv", row.names=FALSE)


##joeys script - edit
library(tidyverse)
library(cowplot)
library(broom)
theme_set(theme_cowplot())


sdata_raw_names <-  read_excel("data/Rstar_experiment_final.xlsx") %>% 
  colnames()

sdata <- read_excel("data/Rstar_experiment_final.xlsx", skip = 2, col_names = sdata_raw_names) %>% 
  mutate(resource_level = as.factor(R_Concentration)) %>% 
  filter(!is.na(resource_level))

sdata2 <- sdata %>% 
  separate_wider_delim(unique_well, delim = "_", names = c("well", "time", "resource")) %>% 
  mutate(days = Hour/24)

well_key <- sdata %>% 
  select(Well, resource_level) %>% 
  distinct() %>% 
  rename(well = Well)


sdata2 %>% 
  ggplot(aes(x = days, y = log(RFU), group = well, color = Treatment)) + geom_line() +
  geom_point() +
  facet_wrap( ~ resource_level)

growth <- sdata2 %>% 
  filter(Treatment != "Blank") %>% 
  group_by(well) %>% 
  do(tidy(lm(log(RFU) ~ days, data = .))) 


growth2 <- growth %>% 
  left_join(well_key)


growth2 %>% 
  filter(term == "days") %>% 
  ggplot(aes(x = resource_level, y = estimate)) + geom_point() +
  ylab("growth rate (per day)") + xlab("resource level")
