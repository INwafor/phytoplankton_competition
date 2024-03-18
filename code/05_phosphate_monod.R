## going to work on monod curves 
source("code/03_RFUS_fist.R")
source("code/04_RFUS_scen.R")
source("code/07_rstar_redo.R")

library(tidyverse)
library(cowplot)
library(broom)
library(readxl)
library(janitor)
install.packages("here")
install.packages("plotrix")
library(plotrix)
library(here)

str(march15_rfus_final)

##LOOK AT IT BY READS INSTEAD OF BY DAY- time elapsed in units of days divide number of hours by 24 = unit of days - 
#then use that in the equation to calculate growth rates
##time elaspsed from first read add column = fix
march15_rfus_final %>% 
  # filter(days < 1.4) %>% 
  ggplot(aes(x = day, y = rfu, color = factor(r_concentration), group = r_concentration)) + geom_point() +
  geom_line() + 
  facet_wrap( ~ spec_temp, scales = "free_y") + 
  scale_color_viridis_d(name = "Phosphate level")


phosphate_exp <- final_merge %>% 
  mutate(exponential = case_when(day >= 1 ~ "yes",
                                 TRUE ~ "no")) %>% 
  filter(exponential == "yes") %>% 
  group_by(r_concentration) %>% 
  mutate(N0 = rfu[[1]]) 


##estimate growth rates first then fit the monod curve OR direct method (estimate parameters directly)
#plot edtimate as function of r_concentration
Scen_21C_growth2 %>% 
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6)

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

##stuff down here isnt working too good 
{
# this graph is a weirdo! how to make it better?
Scen_21C_growth2 %>% 
  mutate(r_concentration = as.numeric(r_concentration)) %>%
  ggplot(aes(x= r_concentration, y= estimate)) + geom_point() +
  # geom_errorbar(aes(ymin=estimate-best.se, ymax=estimate + best.se), width=.2) + 
  geom_line(data=preds, aes(x= r_concentration, y=.fitted), color = "purple", size = 1) +
  # facet_grid(treatment ~ ancestor_id) +
  ylab("Exponential growth rate (/day)") + xlab("Phosphate concentration (uM)")


growth_rates <- phosphate_exp %>%
  group_by(r_concentration) %>%
  do(tidy(nls(rfu ~ N0 * exp(r*day),
              data= .,  start=list(r=0.01),
              control = nls.control(maxiter=100, minFactor=1/204800000)))) %>% 
  ungroup() 


growth2 <- left_join(growth_rates, treatments, by = "population") %>% 
  mutate(treatment = ifelse(is.na(treatment), "none", treatment))


growth2 %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>% 
  ggplot(aes(x = nitrate_concentration, y = estimate)) + geom_point() +
  facet_grid(treatment ~ ancestor_id) + geom_hline(yintercept = 0) + ylab("Exponential growth rate (/day)") +
  xlab("Nitrate concentration (uM)") 
}

# growth rates with growthTools -------------------------------------------
library(growthTools)

#will i not get proper growth rates? 

##Fist_21C only 
str(final_merge)

b2 <- final_merge %>% 
  filter(r_concentration == 0.5) %>% 
  filter(spec_temp == "Fist_21C") %>% 
  mutate(ln.fluor = log(rfu)) 

##one again points are just displayed on top of eachother (line connection is jagged)
final_merge %>%
  # filter(spec_temp == "Fist_21C") %>%
  ggplot(aes(x = day, y = rfu, group = r_concentration, color = treatment)) + geom_point() + geom_line()


res <- get.growth.rate(b2$day, b2$ln.fluor, plot.best.Q =T, id = "Fist_21C")

#literally what am i looking at 
growth_rates_n <- final_merge %>%
  filter(spec_temp != "Fist_21C") %>% 
  mutate(ln.fluor = log(rfu)) %>% 
  filter(r_concentration > 0.5) %>% 
  do(grs=get.growth.rate(x=.$day, y=.$ln.fluor,id=.$spec_temp,plot.best.Q=T))


growth_sum_n <- growth_rates_n %>%
  summarise(spec_temp,mu=grs$best.slope,
            best.model=grs$best.model,best.se=grs$best.se)

all_growth_n <- left_join(growth_sum_n, treatments5, by = c("well_plate")) %>% 
  filter(!is.na(ancestor_id))
write_csv(all_growth_n, "data-processed/exponential_growth_nitrate.csv")

all_growth_n <- read_csv("data-processed/exponential_growth_nitrate.csv")
all_growth_n %>% 
  filter(!is.na(ancestor_id)) %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>% 
  ggplot(aes(x = nitrate_concentration, y = mu)) + geom_point() +
  facet_grid(treatment ~ ancestor_id) + geom_hline(yintercept = 0) + ylab("Exponential growth rate (/day)") +
  xlab("Nitrate concentration (uM)") 

