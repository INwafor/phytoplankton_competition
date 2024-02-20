##Feb13th 
##worrk on second experiment trial 
# Rstar for Phosphate

##plan: start by making each of the sheets into a data frame - join those all together - thats one file of code
## next file make a path from this code and organize/clean up the data frame by species, make times/dates correct
## path from that code to make a new one that plots the R* by temperature and use that to analyze 
## finally one last script to work on the monod curves and fitting the data to growth models

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) 
theme_set(theme_cowplot())


scen_8C_00 <- read_excel("data-raw/Scen_8C_00.xlsx", range = "A40:CL137")

### this is one way of assigning treatments to wells. Alternatively, you could list out all the wells in an csv or xls file with their treatments and then do a left join. This would be my preferred appraoach
## how to do the left join
scen_8C_00 %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "Blank",
                               str_detect(well, "B") ~ "ftz1",
                               str_detect(well, "C") ~ "ftz2",
                               str_detect(well, "D") ~ "ftz3",
                               str_detect(well, "E") ~ "casp1",
                               str_detect(well, "F") ~ "casp2",
                               str_detect(well, "G") ~ "casp3",
                               str_detect(well, "H") ~ "Blank")) 