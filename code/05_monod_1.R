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

all_merged <- all_merged %>% separate(file_name, into = c("file_name", "temp", "read"), remove = FALSE) %>%
  unite(file_name, c(file_name, temp))

all_merged <- all_merged %>%
  mutate(time_elapsed_units = round(time_elapsed_units, 2))

custom_order <- c(0.00, 0.73, 0.93, 1.09, 1.84, 1.98, 2.18, 2.88, 3.04, 3.18)

##logged rfu but cant view 
all_merged %>% 
  filter(day > 0) %>% 
  ggplot(aes(x = time_elapsed_units, y = RFU, color = factor(r_concentration), group = file_name)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(file_name ~ r_concentration, scales = "free_y") + 
  scale_color_viridis_d(name = "Phosphate level") +
  geom_smooth(method = "loess", se = FALSE) + 
  ylab("RFU") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


phosphate_exp <- all_merged %>% 
  mutate(exponential = case_when(day >= 1 ~ "yes",
                                 TRUE ~ "no")) %>% 
  filter(exponential == "yes") %>% 
  group_by(r_concentration) 
#%>% mutate(N0 = RFU[[1]]) 
## what is NO rfu

##estimate growth rates first then fit the monod curve OR direct method (estimate parameters directly)
#plot estimate as function of r_concentration
##how to lay out all the concentration levels on the plot?

##FEBRUARY DATA 
#Fist 21C
concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

Fist_21C_growth2 %>% 
  ggplot(aes(x = factor(r_concentration, levels = concentration_order), y = estimate)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) +
  xlab("Phosphate concentration (uM)")

Fist_30C_growth2 %>% 
  ggplot(aes(x = factor(r_concentration, levels = concentration_order), y = estimate)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) +
  xlab("Phosphate concentration (uM)")

Fist_30C_growth2 %>% 
  ggplot(aes(x = factor(r_concentration, levels = concentration_order), y = estimate)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) +
  xlab("Phosphate concentration (uM)")

Scen_21C_growth2 %>% 
  ggplot(aes(x = factor(r_concentration, levels = concentration_order), y = estimate)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) +
  xlab("Phosphate concentration (uM)")

Scen_30C_growth2 %>% 
  ggplot(aes(x = factor(r_concentration, levels = concentration_order), y = estimate)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) +
  xlab("Phosphate concentration (uM)")

Scen_8C_growth2 %>% 
  ggplot(aes(x = factor(r_concentration, levels = concentration_order), y = estimate)) + 
  geom_point(col = "black", shape = 1, alpha = 0.6) +
  xlab("Phosphate concentration (uM)")


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

## the preds model puts it too high up - the geom line - how can we fix
##concentration values are smudged on one side ?? 
Fist_21C_growth2 %>% 
  mutate(r_concentration = as.numeric(r_concentration)) %>%
  ggplot(aes(x = factor(r_concentration, levels = concentration_order), y = estimate)) + geom_point() +
  #geom_errorbar(aes(ymin=estimate-best.se, ymax=estimate + best.se), width=.2) + 
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

#the concentrations with growth rates over 0 
## remove the outlier and make concentration show for each point
growth_rates %>% 
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  #facet_grid(treatment ~ ancestor_id) + 
  geom_hline(yintercept = 0) + ylab("Exponential growth rate (/day)") +
  xlab("Phosphate concentration (uM)") 

# april 3rd left off {
write_csv(growth2, "data-processed/nitrate-r-star-growth-rates.csv")
growth2 <- read_csv("data-processed/nitrate-r-star-growth-rates.csv")

monod_fits <- growth2 %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>% 
  group_by(population) %>% 
  do(tidy(nls(estimate ~ umax* (nitrate_concentration / (ks+ nitrate_concentration)),
              data= .,  start=list(ks = 1, umax = 1), algorithm="port", lower=list(c=0.01, d=0),
              control = nls.control(maxiter=500, minFactor=1/204800000))))


preds <- growth2 %>%
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>%
  group_by(treatment, ancestor_id) %>% 
  do(augment(nls(estimate ~ umax* (nitrate_concentration/ (ks+ nitrate_concentration)),
                 data= .,  start=list(ks = 1, umax = 1), algorithm="port", lower=list(c=0.01, d=0),
                 control = nls.control(maxiter=500, minFactor=1/204800000))))


growth2 %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>%
  ggplot(aes(x= nitrate_concentration, y= estimate)) + geom_point() +
  # geom_errorbar(aes(ymin=estimate-best.se, ymax=estimate + best.se), width=.2) + 
  geom_line(data=preds, aes(x=nitrate_concentration, y=.fitted), color = "purple", size = 1) +
  facet_grid(treatment ~ ancestor_id) +
  ylab("Exponential growth rate (/day)") + xlab("Nitrate concentration (uM)")


prediction_function <- function(df) {
  
  
  monodcurve<-function(x){
    growth_rate<- (df$umax[[1]] * (x / (df$ks[[1]] +x)))
    growth_rate}
  
  pred <- function(x) {
    y <- monodcurve(x)
  }
  
  x <- seq(0, 1000, by = 1)
  
  preds <- sapply(x, pred)
  preds <- data.frame(x, preds) %>% 
    rename(nitrate_concentration.x = x, 
           growth_rate = preds)
}


bs_split <- monod_fits %>% 
  select(population, term, estimate) %>% 
  dplyr::ungroup() %>% 
  spread(key = term, value = estimate) %>%
  split(.$population)


all_preds_n <- bs_split %>% 
  map_df(prediction_function, .id = "population")


all_predsn_2 <- left_join(all_preds_n, treatments, by = c("population"))
growth2 %>% 
  # all_growth_n %>% 
  # mutate(estimate = mu) %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>% 
  ggplot(aes(x= nitrate_concentration, y= estimate)) + geom_point() +
  # geom_errorbar(aes(ymin=estimate-best.se, ymax=estimate + best.se), width=.2) + 
  geom_line(data=all_predsn_2, aes(x=nitrate_concentration.x, y=growth_rate, color = treatment), size = 1) +
  facet_grid(treatment ~ ancestor_id) +
  ylab("Exponential growth rate (/day)") + xlab("Nitrate concentration (uM)")
ggsave("figures/nitrate_monod.pdf", width = 15, height = 10)

library(plotrix)

m2 <- left_join(monod_fits, treatments, by = "population")

m2 %>% 
  group_by(treatment, term) %>% 
  summarise_each(funs(mean, std.error), estimate) %>%
  ggplot(aes(x = treatment, y = mean)) + geom_point() + 
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
  facet_wrap( ~ term, scales = "free")
ggsave("figures/nitrate_monod_params.pdf", width = 8, height = 6)
}

# growth rates with growthTools -------------------------------------------
library(growthTools)

##Fist_21C start  
str(all_merged)

b2 <- all_merged %>% 
  filter(r_concentration == 0.5) %>% 
  filter(file_name == "Fist_21C") %>% 
  mutate(ln.fluor = log(RFU)) 

##one again points are just displayed on top of eachother
## look at all merged at top - face wrap by file name  / resource concentration? 
all_merged %>%
  # filter(spec_temp == "Fist_21C") %>%
  ggplot(aes(x = time_elapsed_units, y = RFU, group = r_concentration, color = treatment)) + geom_point() + geom_line()


res <- get.growth.rate(b2$day, b2$ln.fluor, plot.best.Q =T, id = "Fist_21C")

#this makes only one colmumn how to fix so it still is joined ot everyhting 
growth_rates_p <- all_merged %>%
  filter(file_name != "Fist_21C") %>% 
  mutate(ln.fluor = log(RFU)) %>% 
  filter(r_concentration > 0.5) %>% 
  do(grs=get.growth.rate(x=.$time_elapsed_units, y=.$ln.fluor,id=.$file_name,plot.best.Q=T))


growth_sum_p <- growth_rates_p %>%
  summarise(file_name,mu=grs$best.slope,
            best.model=grs$best.model,best.se=grs$best.se)


all_growth_p <- growth_sum_p %>% 
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
  ggplot(aes(x = r_concentration, y = mu)) + geom_point() +
  #facet_grid(treatment ~ ancestor_id) + 
  geom_hline(yintercept = 0) + ylab("Exponential growth rate (/day)") +
  xlab("Phosphate concentration (uM)") 


## second half 
{
write_csv(growth2, "data-processed/nitrate-r-star-growth-rates.csv")
growth2 <- read_csv("data-processed/nitrate-r-star-growth-rates.csv")

monod_fits <- growth2 %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>% 
  group_by(population) %>% 
  do(tidy(nls(estimate ~ umax* (nitrate_concentration / (ks+ nitrate_concentration)),
              data= .,  start=list(ks = 1, umax = 1), algorithm="port", lower=list(c=0.01, d=0),
              control = nls.control(maxiter=500, minFactor=1/204800000))))


preds <- growth2 %>%
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>%
  group_by(treatment, ancestor_id) %>% 
  do(augment(nls(estimate ~ umax* (nitrate_concentration/ (ks+ nitrate_concentration)),
                 data= .,  start=list(ks = 1, umax = 1), algorithm="port", lower=list(c=0.01, d=0),
                 control = nls.control(maxiter=500, minFactor=1/204800000))))


growth2 %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>%
  ggplot(aes(x= nitrate_concentration, y= estimate)) + geom_point() +
  # geom_errorbar(aes(ymin=estimate-best.se, ymax=estimate + best.se), width=.2) + 
  geom_line(data=preds, aes(x=nitrate_concentration, y=.fitted), color = "purple", size = 1) +
  facet_grid(treatment ~ ancestor_id) +
  ylab("Exponential growth rate (/day)") + xlab("Nitrate concentration (uM)")


prediction_function <- function(df) {
  
  
  monodcurve<-function(x){
    growth_rate<- (df$umax[[1]] * (x / (df$ks[[1]] +x)))
    growth_rate}
  
  pred <- function(x) {
    y <- monodcurve(x)
  }
  
  x <- seq(0, 1000, by = 1)
  
  preds <- sapply(x, pred)
  preds <- data.frame(x, preds) %>% 
    rename(nitrate_concentration.x = x, 
           growth_rate = preds)
}


bs_split <- monod_fits %>% 
  select(population, term, estimate) %>% 
  dplyr::ungroup() %>% 
  spread(key = term, value = estimate) %>%
  split(.$population)


all_preds_n <- bs_split %>% 
  map_df(prediction_function, .id = "population")


all_predsn_2 <- left_join(all_preds_n, treatments, by = c("population"))
growth2 %>% 
  # all_growth_n %>% 
  # mutate(estimate = mu) %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>% 
  ggplot(aes(x= nitrate_concentration, y= estimate)) + geom_point() +
  # geom_errorbar(aes(ymin=estimate-best.se, ymax=estimate + best.se), width=.2) + 
  geom_line(data=all_predsn_2, aes(x=nitrate_concentration.x, y=growth_rate, color = treatment), size = 1) +
  facet_grid(treatment ~ ancestor_id) +
  ylab("Exponential growth rate (/day)") + xlab("Nitrate concentration (uM)")
ggsave("figures/nitrate_monod.pdf", width = 15, height = 10)

library(plotrix)

m2 <- left_join(monod_fits, treatments, by = "population")

m2 %>% 
  group_by(treatment, term) %>% 
  summarise_each(funs(mean, std.error), estimate) %>%
  ggplot(aes(x = treatment, y = mean)) + geom_point() + 
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
  facet_wrap( ~ term, scales = "free")
ggsave("figures/nitrate_monod_params.pdf", width = 8, height = 6)
  }


# Monod fits with the growthTools estimates -------------------------------

monod_fits <- all_growth_n %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>% 
  rename(estimate = mu) %>% 
  group_by(population) %>% 
  do(tidy(nls(estimate ~ umax* (nitrate_concentration / (ks+ nitrate_concentration)),
              data= .,  start=list(ks = 1, umax = 1), algorithm="port", lower=list(c=0.01, d=0),
              control = nls.control(maxiter=500, minFactor=1/204800000))))

preds <- all_growth_n %>%
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>%
  rename(estimate = mu) %>% 
  group_by(treatment, ancestor_id) %>% 
  do(augment(nls(estimate ~ umax* (nitrate_concentration/ (ks+ nitrate_concentration)),
                 data= .,  start=list(ks = 1, umax = 1), algorithm="port", lower=list(c=0.01, d=0),
                 control = nls.control(maxiter=500, minFactor=1/204800000))))


bs_split <- monod_fits %>% 
  select(population, term, estimate) %>% 
  dplyr::ungroup() %>% 
  spread(key = term, value = estimate) %>%
  split(.$population)


all_preds_n <- bs_split %>% 
  map_df(prediction_function, .id = "population")


all_predsn_2 <- left_join(all_preds_n, treatments5, by = c("population")) %>% 
  filter(!is.na(ancestor_id))


all_predsn_2 %>% 
  # filter(treatment == "none", ancestor_id == "anc5") %>% 
  filter(population == 11) %>% View

treatments2 %>% 
  filter(population == 11) %>% View

names(all_predsn_2)

all_predsn_3 <- all_predsn_2 %>% 
  distinct(population, nitrate_concentration.x, nitrate_level, well_plate, .keep_all = TRUE)


all_growth_n %>% 
  mutate(estimate = mu) %>% 
  mutate(nitrate_concentration = as.numeric(nitrate_concentration)) %>% 
  # filter(treatment == "none", ancestor_id == "anc5") %>% 
  ggplot(aes(x= nitrate_concentration, y= estimate)) + geom_point() +
  # geom_errorbar(aes(ymin=estimate-best.se, ymax=estimate + best.se), width=.2) + 
  geom_line(data= all_predsn_3, aes(x=nitrate_concentration.x, y=growth_rate, color = treatment, group = population), size = 1) +
  facet_grid(treatment ~ ancestor_id) +
  # facet_wrap(~population) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  ylab("Exponential growth rate (/day)") + xlab("Nitrate concentration (uM)") + xlim(-15, 1000) + ylim(-1, 2.5)
ggsave("figures/nitrate_monod_growth_tools2.pdf", width = 15, height = 10)

m2 <- left_join(monod_fits, treatments5, by = "population")

m2 %>% 
  filter(term == "ks") %>% View

m2 %>% 
  group_by(treatment, term) %>% 
  summarise_each(funs(mean, std.error), estimate) %>%
  ggplot(aes(x = treatment, y = mean)) + geom_point() + 
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
  facet_wrap( ~ term, scales = "free")
ggsave("figures/nitrate_monod_params.pdf", width = 8, height = 6)



# find R* -----------------------------------------------------------------

#two sample - sampled paired 
#-2 one sample t-test and compare } treat set of differences as one sample 
## test R star at different temperatures / species - estimate amount of error
## family of different curves fit to the same dataset - bootstrapping (mean +- se) - dot and line

library(rootSolve)	

m <- 0.1 ## mortality rate



monod_wide <-  monod_fits %>% 
  select(population, term, estimate) %>% 
  spread(key = term, value = estimate)


### define the Monod curve, with a mortality rate of 0.1
monod_curve_mortality <- function(nitrate_concentration, umax, ks){
  res <- (umax* (nitrate_concentration / (ks+ nitrate_concentration))) - 0.1
  res
}	

m <- 0.1 ## set mortality rate, which we use in the rstar_solve

rstars <- monod_wide %>% 
  # mutate(rstar = uniroot.all(function(x) monod_curve_mortality(x, umax, ks), c(0.0, 50))) %>% ## numerical
  mutate(rstar_solve = ks*m/(umax-m)) ## analytical

write_csv(monod_wide, "data-processed/nitrate_monod_parameters.csv")

monod_wide <- read_csv("data-processed/nitrate_monod_parameters.csv")
monod_wide <- monod_fits %>% 
  select(population, term, estimate) %>% 
  spread(key = term, value = estimate)


find_rstar <- function(m) {
  rstar <- monod_wide %>% 
    mutate(rstar_solve = ks*m/(umax-m)) %>% 
    mutate(mortality_rate = m)
  return(rstar)
}

ms <- seq(0.00, 0.3, by = 0.01)

all_rstars <- ms %>% 
  map_df(find_rstar, .id = "ms")


rstars2 <- left_join(all_rstars, treatments, by = "population")
rstars2 %>% 
  filter(!is.na(rstar_solve)) %>% 
  # filter(population != 3) %>% 
  group_by(treatment, mortality_rate) %>% 
  summarise_each(funs(mean, std.error), rstar_solve) %>% 
  ggplot(aes(x = reorder(treatment, mean), y = mean, color = mortality_rate)) + geom_point() + 
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error, color = mortality_rate), width = 0.2) +
  ylab("R* (uM N)") + xlab("Selection treatment") + scale_color_viridis_c() +
  facet_wrap( ~ mortality_rate, scales = "free") +
  geom_point(shape = 1, color = "black")

ggsave("figures/nitrate-r-star-mortality-rates.pdf", width = 20, height = 10)


rstars2 %>% 
  # filter(!is.na(rstar_solve)) %>% 
  # filter(treatment == "B") %>% 
  group_by(treatment, mortality_rate) %>% 
  summarise_each(funs(mean, std.error), rstar_solve) %>% 
  ggplot(aes(x = mortality_rate, y = mean, color = mortality_rate)) + geom_point() + 
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error, color = mortality_rate), width = 0.01) +
  ylab("R* (uM N)") + xlab("Mortality rate") + scale_color_viridis_c() +
  facet_wrap( ~ treatment, scales = "free") +
  geom_point(shape = 1, color = "black") + geom_smooth(method = "lm")
ggsave("figures/nitrate-r-star-mortality-rate-effect.pdf", width = 8, height = 6)




monod_wide <- read_csv("data-processed/nitrate_monod_parameters.csv")

rstars_a <- monod_wide %>% 
  # mutate(rstar = uniroot.all(function(x) monod_curve_mortality(x, umax, ks), c(0.0, 50))) %>% ## numerical
  mutate(rstar_solve = ks*m/(umax-m)) ## analytical


treatments_old <- read_excel(here("data-general", "ChlamEE_Treatments_JB.xlsx")) %>%
  clean_names() %>%
  mutate(treatment = ifelse(is.na(treatment), "none", treatment)) %>%
  filter(population != "cc1629") %>% 
  mutate(population_old = population) %>% 
  mutate(treatment_old = treatment) 
# filter(population != "cc1690") %>% 

treatments_new <- read_csv(here("data-processed", "nitrate-treatments-processed.csv")) %>% 
  distinct(population, ancestor_id, treatment) %>%
  # filter(population != "cc1690") %>% 
  mutate(population_new = population) %>% 
  mutate(treatment_new = treatment)

left_join(treatments_new, treatments_old) %>% 
  select(contains("treatment"), everything()) %>% View

treatments7 <- treatments6 %>% 
  mutate(population_new = population)
left_join(treatments7, treatments_old) %>% 
  select(contains("treatment"), everything()) %>% View


rstars3 <- left_join(rstars_a, treatments_new, by = "population") %>% 
  distinct(population, ks, umax, .keep_all = TRUE)



rstars3 %>% 
  group_by(treatment) %>% 
  summarise_each(funs(mean, std.error), rstar_solve) %>% 
  ggplot(aes(x = reorder(treatment, mean), y = mean)) + geom_point() +
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error),width = 0.1) +
  ylab("R* (umol N)") + xlab("Selection treatment") + geom_point(aes(x = reorder(treatment, rstar_solve), y = rstar_solve, color = ancestor_id), size = 2, data = rstars3, alpha = 0.5) +
  scale_color_discrete(name = "Ancestor") + geom_point()
ggsave("figures/nitrate-r-star-means.pdf", width = 6, height = 4)
ggsave("figures/nitrate-r-star-means.png", width = 6, height = 4)

