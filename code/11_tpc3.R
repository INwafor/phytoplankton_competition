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
#change name to well and treatment

tpc3_raw <- c(list.files("data-raw/tpc-july29", full.names = TRUE))

tpc3_raw <- tpc3_raw[grepl(".xls", tpc3_raw)]

names(tpc3_raw) <- tpc3_raw %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")

##
tpc3_plates <- map_df(tpc3_raw, read_excel, .id = "file_name")%>%
  rename(row = ...1) %>% 
  mutate(file_name = str_replace(file_name, " ", "")) %>% 
  separate(file_name, into = c("location","one","two","s1","file_name", "read"), remove = FALSE)%>%
  select(-location,
         -one,
         -two,
         -s1,
         -...14) %>%
  unite(file_name, c(file_name, read))


allp <- all_plates %>% 
  gather(key = well, value = rfu, 3:14) %>%
  mutate(row = as.character(row))%>%
  unite(row, c(row, well))%>% 
  rename(Well = row)

allp$Well <- gsub("_", "", allp$Well)

## now we add in the ss_template
ss_merged<- left_join(allp, ss_template, by = "Well")%>%
  separate(file_name, into = c("name", "read"), remove = FALSE)

##now add in the timing by read - separate file name into name_read and add in time

sstime <- read_excel("data/tpc3-time .xlsx", sheet = "shaker") %>%
  select(-time)

ss_mergedt <- left_join(ss_merged, sstime, by = "read")%>%
  select(-file_name) %>%
  clean_names()

#Now can plot out by shaker vs. static 
shaking <- ss_mergedt %>%
  filter(grepl("shaking", name))

shaking %>%
  ggplot(aes(x = time_passed, y = rfu, group = well)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~treatment, scales = "free_y")

