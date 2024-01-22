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
scen_test <- read_excel("data/fluorescence_standard_data.xlsx", sheet = "scenedesmus") %>%
  mutate(Date = "Jan 17") %>%
  select(-c("Algal Concentration - hemocytomer?")) %>%
  mutate(Row = substr(Well, 1, 1)) %>%
  view()

### ok this is good but... how do we seperate it by well
scen_test %>% 
  ggplot(aes( x = Well, y = RFU, colour = Treatment)) +
  geom_point(aes(shape = Treatment), size = 2) +
  theme_minimal() +
  geom_line(aes(x = Well, y = RFU, group = `Dilution Factor`)) +
  ggtitle("Scenedesmus")

## hate this
scen_test %>%
  ggplot(aes(x = Well, y = RFU, colour = Treatment)) +
  geom_point(aes(shape = Treatment), size = 2) +
  theme_minimal() +
  geom_line(aes(x = Well, y = RFU, group = `Dilution Factor`)) +
  ggtitle("Scenedesmus") +
  facet_wrap(~Row, scales = "free_y")

##Jan 21st - trying ggplate / autoplate
str(scen_test)

plate_plot(
  data = scen_test,
  position = Well,
  value = `Dilution Factor`,
  plate_size = 96,
  colour = c(
  "#51127CFF",
  "#B63679FF",
  "#FB8861FF",
  "#FCFDBFFF"),
  plate_type = "round"
)


scen_19 <- read_excel("data/fluorescence_standard_data_sep.xlsx", sheet = "scen_jan19") %>%
  mutate(Row = substr(Well, 1, 1))

plate_plot(
  data = scen_19,
  position = Well,
  value = `Dilution Factor`,
  label = RFU,
  label_size = 2.5,
  plate_size = 96,
  plate_type = "round",
  scale = 2.3,
  title = "Scenedesmus Dilution Test (from 1:10 dilution)",
  title_size = 12)

## REAL WORK HERE - gonna save them without label and with (two slides)

scen <- read_excel("data/fluorescence_standard_data_final.xlsx", sheet = "scen_jan17") %>%
  mutate(Row = substr(Well, 1, 1))

plate_plot(
  data = scen,
  position = Well,
  value = `Dilution Factor`,
  label_size = 2.5,
  plate_size = 96,
  plate_type = "round",
  scale = 2.7,
  title = "Scenedesmus Serial Dilution Test",
  title_size = 12,
  colour = c("white","#345832", "#477744", "#599656", "#73ad70", "#92bf8f", "#b0d1ae"))

scen2 <- read_excel("data/fluorescence_standard_data_final.xlsx", sheet = "scen_jan19") %>%
  mutate(Row = substr(Well, 1, 1))

plate_plot(
  data = scen2,
  position = Well,
  value = `Dilution Factor`,
  label_size = 2.5,
  label = RFU,
  plate_size = 96,
  plate_type = "round",
  scale = 2.7,
  title = "Scenedesmus Dilution Test (from 1:10 dilution)",
  title_size = 12,
  colour = c("white", "#477744", "#73ad70", "#92bf8f","#599656", "#345832"))

fist <- read_excel("data/fluorescence_standard_data_final.xlsx", sheet = "fist_jan17") %>%
  mutate(Row = substr(Well, 1, 1))

#par(bg = "#EFEFEF") use to change background.. not working?

plate_plot(
  data = fist,
  position = Well,
  value = `Dilution Factor`,
  label_size = 2.5,
  label = RFU,
  plate_size = 96,
  plate_type = "round",
  scale = 2.7,
  title = "Fistulifera Serial Dilution Test",
  title_size = 12,
  colour = c("white", "#332211", "#6C4623", "#7E5C3A", "#987A5C", "#B19272", "beige"))

fist2 <- read_excel("data/fluorescence_standard_data_final.xlsx", sheet = "fist_jan19") %>%
  mutate(Row = substr(Well, 1, 1))

plate_plot(
  data = fist2,
  position = Well,
  value = `Dilution Factor`,
  label = RFU,
  label_size = 2.5,
  plate_size = 96,
  plate_type = "round",
  scale = 2.7,
  title = "Fistulifera Dilution Test (from 1:10 dilution)",
  title_size = 12,
  colour = c("white","#7E5C3A","#6C4623", "#B19272", "#987A5C", "#332211"))

