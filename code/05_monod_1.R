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
theme_set(theme_cowplot())

##this has stopped working ?? need to fix this first bc will affect everything after for those temps
str(all_merged)
str(all_merged2)

all_merged <- all_merged %>% separate(file_name, into = c("file_name", "temp", "read"), remove = FALSE) %>%  unite(file_name, c(file_name, temp))

all_merged2 <- all_merged2 %>% separate(file_name, into = c("file_name", "temp", "read"), remove = FALSE) %>%
  unite(file_name, c(file_name, temp))

all_merged <- all_merged %>%
  mutate(time_elapsed_units = round(time_elapsed_units, 2))
all_merged2 <- all_merged2 %>%
  mutate(time_elapsed_units = round(time_elapsed_units, 2))

filtered_all_merged <- all_merged %>%
  filter(file_name != "Fist_21C" & file_name != "Fist_30C" & file_name != "Scen_8C")

rfu_df <- bind_rows(filtered_all_merged, all_merged2)

custom_order <- c(0.00, 0.73, 0.93, 1.09, 1.84, 1.98, 2.18, 2.88, 3.04, 3.18)

library(dplyr)

plot <- rfu_df %>% 
  filter(day > 0) %>% 
  ggplot(aes(x = time_elapsed_units, y = log(RFU), color = factor(r_concentration), group = file_name)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(file_name ~ r_concentration, scales = "free_y") + 
  scale_color_viridis_d(name = "Phosphate level") +
  ylab("logged RFU") + 
  xlab("Time elapsed (units of days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ggsave("plot.png", plot, width = 20, height = 15)

phosphate_exp <- rfu_df %>% 
  select(-read) %>%
  mutate(exponential = case_when(day >= 1 ~ "yes",
                                 TRUE ~ "no")) %>% 
  filter(exponential == "yes") %>% 
  group_by(r_concentration) %>% 
  mutate(N0 = RFU[[1]]) 
## what is NO rfu

##estimate growth rates first then fit the monod curve OR direct method (estimate parameters directly)
#plot estimate as function of r_concentration
##how to lay out all the concentration levels on the plot?


concentration_order <- c("0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")

all_merged_growth <- rfu_df %>% 
  filter(treatment != "Blank") %>% 
  group_by(well, file_name) %>% 
  do(tidy(lm(log(RFU) ~ day, data = .))) 

## now add it to something that adds file name
all_merged_growth2 <- all_merged_growth %>% 
  left_join(plate_layout) %>%
  filter(term == "day")

all_merged_growth2 %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  facet_wrap(~ file_name) +
  ylab("Growth rate (per day)") + xlab("Resource level") 

## individual growth plots{
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
}
fit_model <- function(all_merged_growth2) {
  nls(estimate ~ umax * (r_concentration / (ks + r_concentration)),
      data = all_merged_growth2,
      start = list(ks = 0.5, umax = 0.5),
      algorithm = "port",
      control = nls.control(maxiter = 500, minFactor = 1/204800000))
}

# Fit separate models for each group
models <- all_merged_growth2 %>%
  group_by(file_name) %>%
  do(model = fit_model(.))

coefficients <- models %>%
  summarise(file_name = first(file_name),
            ks = coef(model)["ks"],
            umax = coef(model)["umax"])
summary(coefficients)

## mod code from joey 
{
  mod1 <- nls(estimate ~ umax* (r_concentration / (ks+ r_concentration)),
              data= all_merged_growth2,  start=list(ks = 1, umax = 1), algorithm="port",
              control = nls.control(maxiter=500, minFactor=1/204800000))
summary(mod1)

mod2 <- nls(estimate ~ umax* (r_concentration / (ks+ r_concentration)),
            data= Scen_30C_growth2,  start=list(ks = 1, umax = 1), algorithm="port",
            control = nls.control(maxiter=500, minFactor=1/204800000))
summary(mod2)


#ks = concentration at which growth rate is at its half of the maximum (units of phosphate)
#umax = maximum estimated growth rate 
}

##then preds_model-- plot the predictions
preds <- all_merged_growth2 %>%
  group_by(file_name) %>%
  do({
    fit <- nls(estimate ~ umax * (r_concentration / (ks + r_concentration)),
               data = .,
               start = list(ks = 0.5, umax = 0.5),  # Provide reasonable initial guesses
               algorithm = "port",
               control = nls.control(maxiter = 500, minFactor = 1/204800000))
    augment(fit) %>% mutate(file_name = unique(.$file_name))
  })

fit <- nls(estimate ~ umax * (r_concentration / (ks + r_concentration)),
           data = all_merged_growth2,
           start = list(umax = 1, ks = 1)) 

# the x axis is not spread across the whole graph and is all clumped overlapping eachother (the ticks) on one side, fix this so it is evenly distributed across the graphs
all_merged_growth2 %>%
  mutate(r_concentration = as.character(r_concentration)) %>%  # Convert to character to match order
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point() +
#  geom_smooth(method = "nls", formula = y ~ umax * (x / (ks + x)),
              method.args = list(start = list(umax = 1, ks = 1),
              se = FALSE, color = "red") +
  facet_wrap(~ file_name) +
  #geom_errorbar(aes(ymin=estimate-best.se, ymax=estimate + best.se), width=.2) + 
  #geom_line(data = preds, aes(x = r_concentration, y = .fitted), color = "red", size = 1) +
  ylab("Exponential growth rate (/day)") + 
  xlab("Phosphate concentration (uM)") +
  scale_x_discrete(limits = concentration_order)


growth_rates <- phosphate_exp %>%
  group_by(file_name, r_concentration) %>%
  do(tidy(nls(RFU ~ N0 * exp(r*day),
              data= .,  start=list(r=0.01),
              control = nls.control(maxiter=100, minFactor=1/204800000)))) %>% 
  ungroup() 

# growth rate curves testing  -------------------------------
## make the line red and fix background to be nicer colour, put more units on the y-axis 
growth_rates %>% 
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  geom_smooth(method = "loess", se = FALSE) + 
  facet_wrap(~ file_name) + 
  geom_hline(yintercept = 0) + ylab("Exponential growth rate (/day)") +
  xlab("Phosphate concentration (uM)") 


growth_rates %>%
  mutate(r_concentration = as.numeric(r_concentration)) %>%
  ggplot(aes(x = r_concentration, y = estimate)) +
  geom_point(col = "black", size = 2.5) + 
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.5) +  # Change line color to red
  facet_wrap(~ file_name) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add horizontal line at y = 0
  ylab("Exponential growth rate (/day)") +
  xlab("Phosphate concentration (uM)") + 
  theme_cowplot() +
  theme(panel.grid.major = element_line(color = "darkgrey", linetype = "solid"),
        panel.grid.minor = element_blank())

# growth rates with growthTools - complete -------------------------------------------
library(growthTools)

str(rfu_df)

b2 <- rfu_df %>% 
  select(-read) %>%
  filter(r_concentration == 0.5) %>% 
  mutate(ln.fluor = log(RFU)) 

### just for 0.5
rfu_df %>%
  ggplot(aes(x = time_elapsed_units, y = RFU, group = r_concentration, color = treatment)) + 
  geom_point() + geom_line() +
  facet_wrap(file_name)

split_data <- split(b2, b2$file_name)

res <- lapply(split_data, function(rfu_df) {
  get.growth.rate(rfu_df$day, rfu_df$ln.fluor, plot.best.Q = TRUE, id = unique(rfu_df$file_name))
})
## makes a separate plot per file name 

#this makes only one colmumn how to fix so it still is joined ot everyhting 
growth_rates_p <- rfu_df %>%
  mutate(ln.fluor = log(RFU)) %>% 
  group_by(file_name, r_concentration) %>%
  filter(r_concentration >= 0.5) %>% 
  do(grs=get.growth.rate(x=.$time_elapsed_units, y=.$ln.fluor,id=.$file_name,plot.best.Q=T))

growth_sum_p <- growth_rates_p %>%
  summarise(file_name,mu=grs$best.slope,
            best.model=grs$best.model,best.se=grs$best.se)

growth_sum_p2 <- bind_cols(growth_sum_p, growth_rates_p) %>%
  clean_names()%>%
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
  select(-grs,
         -file_name_5) %>%
  rename("file_name" = file_name_1)

#gives one growth rate estimate per each temperature + file_name
growth_sum_p2 %>% 
  ggplot(aes(x = r_concentration, y = mu)) + geom_point() +
  facet_wrap(file_name ~ r_concentration) +
  geom_hline(yintercept = 0) + ylab("Exponential growth rate (/day)") +
  xlab("Phosphate concentration (uM)") 


# Monod fits with the growthTools estimates -------------------------------
fit_model2 <- growth_sum_p2
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
    rename(estimate = mu) %>% 
    group_by(file_name) %>% 
  nls(estimate ~ umax * (r_concentration / (ks + r_concentration)),
      data = data,
      start = list(ks = 0.5, umax = 0.5),
      algorithm = "port",
      control = nls.control(maxiter = 500, minFactor = 1/204800000))

preds2 <- growth_sum_p2 %>%
  mutate(r_concentration = as.numeric(r_concentration)) %>%
  rename(estimate = mu) %>% 
  group_by(file_name) %>% 
  do(augment(nls(estimate ~ umax * (r_concentration / (ks + r_concentration)),
                 data = .,  
                 start = list(ks = 0.5, umax = 0.5),  # Adjust starting values
                 algorithm = "default",  # Try a different optimization algorithm
                 lower = list(c = 0.01, d = 0),
                 control = nls.control(maxiter = 1000, minFactor = 1/204800000))))

preds4 <- bind_cols(growth_sum_p2, preds2)%>%
  clean_names() %>%
  select(-file_name_6,
         -r_concentration_5) %>%
  rename("file_name" = file_name_1,
         "r_concentration" = r_concentration_7)


bs_split <- fit_model2 %>% 
  select(file_name, r_concentration, mu) %>% 
  dplyr::ungroup() %>% 
  spread(key = r_concentration, value = mu) %>%
  split(.$file_name)

prediction_function <- function(df) {
  
  monodcurve <- function(x, umax, ks) {
    growth_rate <- umax * (x / (ks + x))
    return(growth_rate)
  }
  
  pred <- function(x, umax, ks) {
    y <- monodcurve(x, umax, ks)
    return(y)
  }
  
  x <- seq(0, 50, by = 1)
  
  preds <- pred(x, coefficients$umax[[1]], coefficients$ks[[1]])
  
  preds_df <- data.frame(r_concentration.x = x, growth_rate = preds)
  return(preds_df)
}

# Check the structure of bs_split
print(str(bs_split))

# Apply the prediction_function to bs_split
all_preds_p <- bs_split %>% 
  map_df(prediction_function, .id = "file_name")

##pred lines is wrong too low and exactly thr same for each
growth_sum_p2 %>% 
  mutate(estimate = mu) %>% 
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
  ggplot(aes(x= r_concentration, y= estimate)) + geom_point() +
  geom_errorbar(aes(ymin=estimate-best_se, ymax=estimate + best_se), width=.2) + 
  geom_line(data= all_preds_p, aes(x=r_concentration.x, y=growth_rate, color = r_concentration.x, group = file_name), size = 1) +
  facet_wrap(~file_name) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  ylab("Exponential growth rate (/day)") + xlab("Phosphate concentration (uM)") + xlim(0.5, 50) + ylim(-0.5, 1.5)

#ggsave("figures/nitrate_monod_growth_tools2.pdf", width = 15, height = 10)

m2< - all_preds_p %>% 
  filter(term == "ks") 

m2 %>% 
  group_by(treatment, term) %>% 
  summarise_each(funs(mean, std.error), estimate) %>%
  ggplot(aes(x = treatment, y = mean)) + geom_point() + 
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
  facet_wrap( ~ term, scales = "free")
#ggsave("figures/nitrate_monod_params.pdf", width = 8, height = 6)


# find R* -----------------------------------------------------------------

#two sample - sampled paired 
#-2 one sample t-test and compare } treat set of differences as one sample 
## test R star at different temperatures / species - estimate amount of error
## family of different curves fit to the same dataset - bootstrapping (mean +- se) - dot and line

library(rootSolve)	

m <- 0.1 ## mortality rate


monod_wide <-  fit_model2 %>% 
  select(file_name, r_concentration, mu) %>% 
  spread(key = r_concentration, value = mu)


### define the Monod curve, with a mortality rate of 0.1
monod_curve_mortality <- function(r_concentration, umax, ks){
  res <- (umax* (r_concentration / (ks+ r_concentration))) - 0.1
  res
}	


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
  mutate(rstar_solve = ks*m/(umax-m)) %>%  ## analytical 
  distinct(population, ks, umax, .keep_all = TRUE)


rstars3 %>% 
  group_by(treatment) %>% 
  summarise_each(funs(mean, std.error), rstar_solve) %>% 
  ggplot(aes(x = reorder(treatment, mean), y = mean)) + geom_point() +
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error),width = 0.1) +
  ylab("R* (umol N)") + xlab("Selection treatment") + geom_point(aes(x = reorder(treatment, rstar_solve), y = rstar_solve, color = ancestor_id), size = 2, data = rstars3, alpha = 0.5) +
  scale_color_discrete(name = "Ancestor") + geom_point()
ggsave("figures/nitrate-r-star-means.png", width = 6, height = 4)

