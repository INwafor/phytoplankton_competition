## going to work on monod curves 
source("code/03_RFUS_fist.R")
source("code/04_RFUS_scen.R")

library(tidyverse)
library(cowplot)
library(broom)
library(readxl)
library(janitor)
install.packages("here")
install.packages("plotrix")
library(plotrix)
library(here)

##February Monod curves - ALL MERGED?
str(all_merged)

##sTOOPPPID
all_merged %>% 
  # filter(days < 1.4) %>% 
  ggplot(aes(x = time_elapsed_units, y = RFU, color = factor(r_concentration), group = file_name)) + geom_point() +
  geom_line() + 
  facet_wrap( ~ file_name, scales = "free_y") + 
  scale_color_viridis_d(name = "Phosphate level")


phosphate_exp <- all_merged %>% 
  mutate(exponential = case_when(day >= 1 ~ "yes",
                                 TRUE ~ "no")) %>% 
  filter(exponential == "yes") %>% 
  group_by(r_concentration) %>% 
  mutate(N0 = RFU[[1]]) 
## what is NO rfu

##estimate growth rates first then fit the monod curve OR direct method (estimate parameters directly)
#plot estimate as function of r_concentration
##how to lay out all the concentration levels on the plot?

##FEBRUARY DATA 
#Fist 21C
Fist_21C_growth2 %>% 
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6)
##something concentration order - values chr 

mod1 <- nls(estimate ~ umax* (r_concentration / (ks+ r_concentration)),
              data= Scen_21C_growth2,  start=list(ks = 1, umax = 1), algorithm="port",
              control = nls.control(maxiter=500, minFactor=1/204800000))
summary(mod1)
#ks = concentration at which growth rate is at its half of the maximum (units of phosphate)
#umax = maximum estimated growth rate 

##then preds_model-- plot the predictions
preds <- augment(nls(estimate ~ umax* (r_concentration/ (ks+ r_concentration)),
                 data= Scen_21C_growth2,  start=list(ks = 1, umax = 1), algorithm="port", lower=list(c=0.01, d=0),
                 control = nls.control(maxiter=500, minFactor=1/204800000)))

preds <- augment(mod1)

## the preds model puts it too high up - the geom line - how can we fix - also show concentration values
Fist_21C_growth2 %>% 
  mutate(r_concentration = as.numeric(r_concentration)) %>%
  ggplot(aes(x= r_concentration, y= estimate)) + geom_point() +
  # geom_errorbar(aes(ymin=estimate-best.se, ymax=estimate + best.se), width=.2) + 
  geom_line(data=preds, aes(x= r_concentration, y=.fitted), color = "purple", size = 1) +
  # facet_grid(treatment ~ ancestor_id) +
  ylab("Exponential growth rate (/day)") + xlab("Phosphate concentration (uM)")


growth_rates <- phosphate_exp %>%
  group_by(r_concentration) %>%
  do(tidy(nls(RFU ~ N0 * exp(r*day),
              data= .,  start=list(r=0.01),
              control = nls.control(maxiter=100, minFactor=1/204800000)))) %>% 
  ungroup() 

#growth2 <- left_join(growth_rates, treatments, by = "population") %>% 
 # mutate(treatment = ifelse(is.na(treatment), "none", treatment))

#the concentrations with growth rates over 0 - just need to tidy
growth_rates %>% 
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  #facet_grid(treatment ~ ancestor_id) + 
  geom_hline(yintercept = 0) + ylab("Exponential growth rate (/day)") +
  xlab("Nitrate concentration (uM)") 


# growth rates with growthTools -------------------------------------------
library(growthTools)

#will i not get proper growth rates? 

##Fist_21C only 
str(all_merged)

b2 <- all_merged %>% 
  filter(r_concentration == 0.5) %>% 
  filter(file_name == "Fist_21C") %>% 
  mutate(ln.fluor = log(RFU)) 

##one again points are just displayed on top of eachother (line connection is jagged)
all_merged %>%
  # filter(spec_temp == "Fist_21C") %>%
  ggplot(aes(x = time_elapsed_units, y = RFU, group = r_concentration, color = treatment)) + geom_point() + geom_line()


res <- get.growth.rate(b2$day, b2$ln.fluor, plot.best.Q =T, id = "Fist_21C")

#literally what am i looking at 
growth_rates_p <- all_merged %>%
  filter(file_name != "Fist_21C") %>% 
  mutate(ln.fluor = log(RFU)) %>% 
  filter(r_concentration > 0.5) %>% 
  do(grs=get.growth.rate(x=.$day, y=.$ln.fluor,id=.$file_name,plot.best.Q=T))


growth_sum_p <- growth_rates_p %>%
  summarise(file_name,mu=grs$best.slope,
            best.model=grs$best.model,best.se=grs$best.se)

all_growth_p <- left_join(growth_sum_p, treatments5, by = c("plate_layout"))

#write_csv(all_growth_n, "data-processed/exponential_growth_nitrate.csv")
#all_growth_n <- read_csv("data-processed/exponential_growth_nitrate.csv")

all_growth_p %>% 
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
  ggplot(aes(x = r_concentration, y = mu)) + geom_point() +
  #facet_grid(treatment ~ ancestor_id) + 
  geom_hline(yintercept = 0) + ylab("Exponential growth rate (/day)") +
  xlab("Phosphate concentration (uM)") 

