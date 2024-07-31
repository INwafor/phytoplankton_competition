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
tpc_platemap<- read_excel("data/plate_map.xlsx", col_names = T)
platemaplong<- tpc_platemap %>% gather(2:13, key="Column", value = "ID")
platemapwithcellid<- platemaplong %>%  unite (1:2,sep= "", col= "position" )

#static data
s_raw<- list.files(path = "data-raw/static-shaking", pattern = "xlsx")

# need to do time manually - next time redo and download the way Amanda does + without template on protocol
ss_template <- read_excel("data/plate_template.xlsx", sheet = "shake-static")
ss_template <- ss_template[!(row.names(ss_template) %in% c("1")),]

s_files <- c(list.files("data-raw/static-shaking", full.names = TRUE))

s_files <- s_files[grepl(".xls", s_files)]

names(s_files) <- s_files %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")

all_plates <- map_df(s_files, read_excel, .id = "file_name")%>%
  rename(row = ...1) %>% 
  mutate(file_name = str_replace(file_name, " ", "")) %>% 
  separate(file_name, into = c("location", "loc_2","s1", "s2", "file_name", "read"), remove = FALSE)%>%
  select(-location,
         -loc_2, 
         -s1,
         -s2,
         -...14)

#all_merged2 <- left_join(all_temp_RFU, plate_layout, by = "well")


all_plates <- unite(all_plates, file_name, c(file_name, temp, read))

all_times <- map_df(RFU_files, read_excel, range = "A7:A8", .id = "file_name") %>% 
  clean_names()

#tpc test
tpc_raw <- c(list.files("data-raw/tpc-test1", full.names = TRUE))

tpc_raw <- tpc_raw[grepl(".xls", tpc_raw)]

names(tpc_raw) <- tpc_raw %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")
