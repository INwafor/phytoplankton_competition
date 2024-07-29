#IEN - july 29-2024
##plotting some of the mini tests i did over the week - checking out svetas/amandas code

library(readxl)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(janitor)
library(lubridate)
library(cellranger)
library(purrr)
library(fs)

##starting with static vs shaking data 
#import folder and files 

platemap<- read_excel("Input_RFU/Acclimation_Trial_8d_Plates/platemap/plate_map.xlsx", col_names = T)
platemaplong<- platemap %>% gather(2:13, key="Column", value = "ID")
platemapwithcellid<- platemaplong %>%  unite (1:2,sep= "", col= "position" )

raw<- list.files(path = "Input_RFU/Acclimation_Trial_8d_Plates", pattern = "xlsx")

RFU_files2 <- c(list.files("data/rstar2_dataraw", full.names = TRUE))

RFU_files2 <- RFU_files2[grepl(".xls", RFU_files2)]

names(RFU_files2) <- RFU_files2 %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")
