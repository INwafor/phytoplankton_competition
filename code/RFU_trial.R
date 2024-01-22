##Ijeoma Nwafor 
#Thesis : Investigating Resource-Limited Competition in Freshwater Phytoplankton 
#Jan. 20th 

##finding the lowest dilution to work with
library(devtools)
devtools::install_github("PhilPalmer/AutoPlate")
install.packages("ggplate")
library(ggplate)
library(autoplate)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot)

###scendesmus
scen <- read_excel("data/fluorescence_standard_data.xlsx", sheet = "scenedesmus") %>%
  mutate(Date = "Jan 17") %>%
  select(-c("Algal Concentration - hemocytomer?")) %>%
  mutate(Row = substr(Well, 1, 1)) %>%
  view()

### ok this is good but... how do we seperate it by well
scen %>% 
  ggplot(aes( x = Well, y = RFU, colour = Treatment)) +
  geom_point(aes(shape = Treatment), size = 2) +
  theme_minimal() +
  geom_line(aes(x = Well, y = RFU, group = `Dilution Factor`)) +
  ggtitle("Scenedesmus")

## hate this
scen %>%
  ggplot(aes(x = Well, y = RFU, colour = Treatment)) +
  geom_point(aes(shape = Treatment), size = 2) +
  theme_minimal() +
  geom_line(aes(x = Well, y = RFU, group = `Dilution Factor`)) +
  ggtitle("Scenedesmus") +
  facet_wrap(~Row, scales = "free_y")

##Jan 21st - trying ggplate / autoplate
str(scen)

plate_plot(
  data = scen,
  position = Well,
  value = RFU,
  label = RFU,
  plate_size = 96,
  colour = c(
  "#51127CFF",
  "#B63679FF",
  "#FB8861FF",
  "#FCFDBFFF"),
  plate_type = "round"
)
