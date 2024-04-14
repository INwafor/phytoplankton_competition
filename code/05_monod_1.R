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
library(RColorBrewer)

rfu_df %>% 
  filter(day > 0) %>% 
  ggplot(aes(x = time_elapsed_units, y = log(RFU), color = factor(r_concentration), group = file_name)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(file_name ~ r_concentration, scales = "free_y") + 
  scale_color_manual(
    values = c("hotpink4", "#9e0142","#d53e4f","sienna1","#fbcf51","#4bc425","#66c2a5","#3288bd","#5e4fa2","hotpink")) +
  ylab("") + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 8),         
        panel.spacing = unit(1, "lines"),
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        legend.position = "none") #+ ggsave("rfu_final4.png", width = 15, height = 10, dpi = 300)


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

##final curve monod estimate
sep_growth2 <- all_merged_growth2 %>%
  separate(file_name, into = c("species", "temperature"), sep = "_")

##set a line at 0 and only display points that lie with 0.5 in both directions of that line 
## then the other points should just be a smooth line 
sep_growth2 %>% 
  mutate(r_concentration = factor(r_concentration, levels = concentration_order)) %>%
  arrange(r_concentration) %>% 
  ggplot(aes(x = r_concentration, y = estimate, color = species)) + geom_point() +
  geom_smooth(method = "loess", span = 0.5, se = FALSE) + 
  facet_wrap(~ temperature) +
  scale_color_manual(
    values = c("#9e0142","#3288bd")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ylab("Growth rate (per day)") + xlab("Resource level") 
## fix and separate by file_name? add line - Figure 2


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


all_merged_growth2 <- all_merged_growth2 %>%
  mutate(r_concentration = factor(r_concentration, levels = concentration_order))
# attempting to bootstrap this data and use it for my figures --------------
library(boot)
library(car)
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(patchwork)
library(minpack.lm)

all_merged_growth2 %>%
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ umax * (x / (ks + x)),
              method.args = list(start = list(umax = 0.5, ks = 0.5)),
              se = FALSE) +
  geom_line() +
  geom_smooth(method = "nls", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0.1, linetype = "solid", color = "red") +  # Add horizontal line at y = 0.5
  facet_wrap(~ file_name) +
  ylab("Exponential growth rate (/day)") + 
  xlab("Phosphate concentration (uM)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  expand_limits(x = 0.5) +
  theme_gray()

linear_model <- lm(estimate ~ r_concentration, data = all_merged_growth2)

growth_rates <- phosphate_exp %>%
  group_by(file_name, r_concentration) %>%
  do(tidy(nls(RFU ~ N0 * exp(r*day),
              data= .,  start=list(r=0.01),
              control = nls.control(maxiter=100, minFactor=1/204800000)))) %>% 
  ungroup() 

# growth rate curves testing -------------------------------
## work on this in the morning to get the curve you wnated from before
# set a line with mortality rate at 0.2

growth_rates %>% 
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
  ggplot(aes(x = r_concentration, y = estimate)) + geom_point() +
  geom_smooth(method = "loess", se = FALSE) + 
  facet_wrap(~ file_name) + 
  geom_hline(yintercept = 0.1) + ylab("Exponential growth rate (/day)") +
  xlab("Phosphate concentration (uM)") 

growth_rates <- growth_rates %>%
  mutate(r_concentration = as.numeric(r_concentration))

scen_growth_rates <- growth_rates %>%
  filter(file_name %in% c("Scen_21C", "Scen_30C"))

# Plot the subsetted data - !!! graph used 
scen_growth_rates %>%
  ggplot(aes(x = r_concentration, y = estimate)) +
  geom_point() +
  stat_summary(geom = "line", fun = mean, color = "lightblue", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0.2, linetype = "solid", color = "red") +
  facet_wrap(~ file_name) +
  ylab("Exponential growth rate (/day)") +
  xlab("Phosphate concentration (uM)")


# growth rates with growthTools  -------------------------------------------
library(growthTools)

str(rfu_df)

rfu_df <- rfu_df %>%
  filter(treatment != "Blank")

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
fit_model2 <- growth_sum_p2 %>%
  mutate(r_concentration = as.numeric(r_concentration)) %>% 
    #rename(estimate = mu) %>% 
    group_by(file_name) %>% 
  nls(mu ~ umax * (r_concentration / (ks + r_concentration)),
      data = .,
      start = list(ks = 0.5, umax = 0.5),
      algorithm = "port",
      control = nls.control(maxiter = 500, minFactor = 1/204800000))

str(fit_model2)
class(fit_model2) ##USE THIS FOR BOOTSTRAPPING

##expecting a model of class nLS - should give you a model of class nls
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

#DOUBLE CHECK - bootstrap to all the variables 
#
preds4 %>%
  #mutate(r_concentration = as.character(r_concentration)) %>%  # Convert to character to match order
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ umax * (x / (ks + x)),
              method.args = list(start = list(umax = 0.5, ks = 0.5)),
              se = FALSE) +
  facet_wrap(~ file_name, scales = "free") +
  geom_errorbar(aes(ymin=estimate-best_se, ymax=estimate + best_se), width=.2) + 
  geom_line(data = preds4, aes(x = r_concentration, y = fitted), color = "red", size = 1) +
  ylab("Exponential growth rate (/day)") + 
  xlab("Phosphate concentration (uM)") 

## this would be the monod grave to use 


##remove 0 from Scen_30C from the dataset and see how the model fits - for fun not justifiable

bs_split <- fit_model2 %>% 
  select(file_name, r_concentration, mu) %>% 
  dplyr::ungroup() %>% 
  spread(key = r_concentration, value = mu) %>%
  split(.$file_name)

prediction_function <- function(df) {
  monodcurve <- function(x, umax, ks) {
    growth_rate <- umax * (as.numeric(x) / (as.numeric(ks) + as.numeric(x)))
    return(growth_rate)
  }
  
  pred <- function(x, umax, ks) {
    y <- monodcurve(x, umax, ks)
    return(y)
  }
  
  x <- c("0", "0.5", "1", "2", "4", "6", "8", "10", "20", "35", "50")
  
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
  mutate(estimate = mu,
         r_concentration = as.numeric(r_concentration)) %>% 
  ggplot(aes(x = r_concentration, y = estimate)) + 
  geom_point() +
  geom_errorbar(aes(ymin = estimate - best_se, ymax = estimate + best_se), width = 0.2) + 
  geom_line(data = all_preds_p %>% mutate(r_concentration.x = as.numeric(r_concentration.x)), 
            aes(x = r_concentration.x, y = growth_rate, color = r_concentration.x, group = file_name), 
            size = 1) +
  facet_wrap(~ file_name) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  ylab("Exponential growth rate (/day)") +
  xlab("Phosphate concentration (uM)") + xlim(0.5, 50) + ylim(-0.5, 1.5)

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


#add coefficients somehow so th
monod_wide <-  fit_model2 %>% 
  select(file_name, r_concentration, mu) %>% 
  spread(key = r_concentration, value = mu)

monod2 <- left_join(coefficients, monod_wide, by = "file_name")
monod2 <- monod2 %>%
  filter(!between(row_number(), 7, 12))%>%
  select_if(~!any(is.na(.))) %>%
  rename("mu_0_5" = `0.5.y`,
         "mu_1" = `1.y`,
         "mu_2" = `2.y`,
         "mu_4" = `4.y`,
         "mu_6" = `6.y`,
         "mu_8" = `8.y`,
         "mu_10" = `10.y`,
         "mu_20" = `20.y`,
         "mu_35" = `35.y`,
         "mu_50" = `50.y`)

### define the Monod curve, with a mortality rate of 0.1
monod_curve_mortality <- function(r_concentration, umax, ks){
  res <- (umax* (r_concentration / (ks+ r_concentration))) - 0.1
  res
}	

rstars <- monod2 %>% 
  # mutate(rstar = uniroot.all(function(x) monod_curve_mortality(x, umax, ks), c(0.0, 50))) %>% ## numerical
  mutate(rstar_solve = ks*m/(umax-m))## analytical

m_values <- tibble(m = 0.1)

# Select the columns to keep in rstars_subset
rstars_subset <- rstars %>%
  select(-mu_0_5, -mu_1, -mu_2, -mu_4, -mu_6, -mu_8, -mu_10, -mu_20, -mu_35, -mu_50)

# Add the 'm' column to rstars_subset
rstars_subset <- bind_cols(rstars_subset, m_values)

find_rstar <- function(m) {
  rstar <- rstars_subset %>% 
    mutate(rstar_solve = ks*m/(umax-m)) %>% 
    mutate(mortality_rate = m)
  return(rstar)
}

# what is ms.. 
ms <- seq(0.00, 0.3, by = 0.01)

# Apply the function to all rows of rstars_subset
all_rstars <- rstars_subset %>% 
  group_by(file_name) %>%
  do(find_rstar(.)) %>%
  ungroup() %>%
  filter(!between(row_number(), 7, 36))

rstars2 <- all_rstars %>% 
  filter(!is.na(rstar_solve)) %>% 
  # filter(population != 3) %>% 
  group_by(file_name, m) %>% 
  summarise_each(funs(mean, std.error), rstar_solve) %>% 
  ggplot(aes(x = reorder(file_name, mean), y = mean, color = m)) + geom_point() + 
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error, color = m), width = 0.2) +
  ylab("R* (uM N)") + xlab("Selection treatment") + scale_color_viridis_c() +
  facet_wrap( ~ m, scales = "free") +
  geom_point(shape = 1, color = "black")


rstars_a <- monod2 %>% 
  # mutate(rstar = uniroot.all(function(x) monod_curve_mortality(x, umax, ks), c(0.0, 50))) %>% ## numerical
  mutate(rstar_solve = ks*m/(umax-m)) %>%  ## analytical 
  distinct(file_name, ks, umax, .keep_all = TRUE)

rstars_a <- rstars_a %>%
  filter(file_name != "Fist_8C",
         file_name != "Scen_30C")

rstars_a %>%
  group_by(file_name) %>%
  summarise_each(funs(mean, std.error), rstar_solve) %>%
  ggplot(aes(x = reorder(file_name, mean), y = mean)) +
  geom_point(size = 4, aes(color = file_name)) +  # Increase point size and match colors to file_name
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.1) +
  geom_text(aes(label = round(mean, 2)), vjust = 2, size = 3.5, color = "black") + 
  scale_color_manual(
    values = c("#9e0142","#fbcf51","#4bc425","#3288bd","#d7a4dd")) +
  labs(y = "R* (umol P)", x = "Treatment", color = "Treatment") +
  theme_classic() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 8)) +
  ylim(-0.1, 0.2) 
# ggsave(filename = file.path("figures","rstar_final.png"), width = 15, height = 10)

