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
#amandas plate map input 
platemap<- read_excel("data/plate_map.xlsx", col_names = T)
platemaplong<- platemap %>% gather(2:13, key="Column", value = "ID")
platemapwithcellid<- platemaplong %>%  unite (1:2,sep= "", col= "position" )

#static data
s_raw<- list.files(path = "data-raw/static-shaking", pattern = "xlsx")




all_plates <- map_df(s_raw, read_excel, range = "A1:M9", .id = "file_name")

%>%
  rename(row = ...1) %>% 
  mutate(file_name = str_replace(file_name, " ", "")) %>%
  separate(file_name, into = c("data", "location", "loc_2", "file_name", "temp", "read"), remove = FALSE)%>%
  select(-data,
         -location,
         -loc_2)

all_plates <- unite(all_plates, file_name, c(file_name, temp, read))

all_times <- map_df(RFU_files, read_excel, range = "A7:A8", .id = "file_name") %>% 
  clean_names()

#tpc test
tpc_raw <- c(list.files("data-raw/tpc-test1", full.names = TRUE))

tpc_raw <- tpc_raw[grepl(".xls", tpc_raw)]

names(tpc_raw) <- tpc_raw %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")
