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

##estimate growth rates first then fit the monod curve OR direct method (estimate parameters directly)
#plot edtimate as function of r_concentration
Scen_21C_growth2 %>% 
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6)

mod1 <- nls(estimate ~ umax* (r_concentration / (ks+ r_concentration)),
              data= Scen_21C_growth2,  start=list(ks = 1, umax = 1), algorithm="port",
              control = nls.control(maxiter=500, minFactor=1/204800000))
#ks = concentration at which growth rate is at its half of the maximum (units of phosphate)
#umax = maximum estimated growth rate 

##then preds_model-- plot the predictions
preds <- augment(nls(estimate ~ umax* (r_concentration/ (ks+ r_concentration)),
                 data= Scen_21C_growth2,  start=list(ks = 1, umax = 1), algorithm="port", lower=list(c=0.01, d=0),
                 control = nls.control(maxiter=500, minFactor=1/204800000)))


### add the bewlow stuff 

growth2 %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>%
  ggplot(aes(x= nitrate_concentration, y= estimate)) + geom_point() +
  # geom_errorbar(aes(ymin=estimate-best.se, ymax=estimate + best.se), width=.2) + 
  geom_line(data=preds, aes(x=nitrate_concentration, y=.fitted), color = "purple", size = 1) +
  facet_grid(treatment ~ ancestor_id) +
  ylab("Exponential growth rate (/day)") + xlab("Nitrate concentration (uM)")



growth_rates <- nitrate_exp %>%
  filter(population != "COMBO") %>% 
  group_by(nitrate_concentration, population, well_plate) %>%
  do(tidy(nls(RFU ~ N0 * exp(r*days),
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

# growth rates with growthTools -------------------------------------------
library(growthTools)

view(final_merge)


b2 <- final_merge %>% 
  filter(nitrate_level == 1) %>% 
  filter(well_plate == "B06_1") %>% 
  mutate(ln.fluor = log(RFU)) 

nitrate %>%
  filter(plate == 1) %>%
  ggplot(aes(x = days, y = RFU, group = well_plate, color = population)) + geom_point() + geom_line()


res <- get.growth.rate(b2$days, b2$ln.fluor, plot.best.Q =T, id = "B02")

growth_rates_n <- nitrate %>%
  filter(population != "COMBO") %>% 
  mutate(ln.fluor = log(RFU)) %>% 
  # filter(nitrate_level > 1) %>% 
  group_by(well_plate) %>% 
  do(grs=get.growth.rate(x=.$days, y=.$ln.fluor,id=.$well_plate,plot.best.Q=T,fpath="/Users/joeybernhardt/Documents/Narwani/ChlamEE-R-star/figures/growth_curve_fits/nitrate/"))

growth_sum_n <- growth_rates_n %>%
  summarise(well_plate,mu=grs$best.slope,
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

