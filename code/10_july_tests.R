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

all_plates <- unite(all_plates, file_name, c(file_name, read)) %>%
  filter(!grepl("H", row))

## not exactly
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

##need to assign algae to clammy scen and fist 
# if well = E and treatment = alage then change treatment to = clammy 

#tpc test
tpc_raw <- c(list.files("data-raw/tpc-test1", full.names = TRUE))

tpc_raw <- tpc_raw[grepl(".xls", tpc_raw)]

names(tpc_raw) <- tpc_raw %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")


s_raw<- list.files(path = "data-raw/static-shaking", pattern = "xlsx")

# need to do time manually - next time redo and download the way Amanda does + without template on protocol
ss_template <- read_excel("data/plate_template.xlsx", sheet = "shake-static")
ss_template <- ss_template[!(row.names(ss_template) %in% c("1")),]

s_files <- c(list.files("data-raw/static-shaking", full.names = TRUE))

s_files <- s_files[grepl(".xls", s_files)]

names(s_files) <- s_files %>% 
  gsub(pattern = ".xlsx$", replacement = "") %>% 
  gsub(pattern = ".xls$", replacement = "")

