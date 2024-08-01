#IEN - august 1st 
#

library(readxl)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(janitor)
library(lubridate)
library(cellranger)
library(purrr)
library(fs)

#amandas plate map input - for TPC 3
tpc_platemap<- read_excel("data/plate_map.xlsx", col_names = T)
platemaplong<- tpc_platemap %>% gather(2:13, key="Column", value = "ID")
platemapwithcellid<- platemaplong %>%  unite (1:2,sep= "", col= "position" )