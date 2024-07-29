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