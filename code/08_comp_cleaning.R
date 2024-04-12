#March 25th, 2024 - Ijeoma Nwafor thesis 
##looking at competition data - make a box plot 

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot) 
library(janitor)
library(lubridate)
theme_set(theme_cowplot())

##this is the averages already cleaned up 
comp <- read_excel("data/comp_phycoprobe.xlsx", sheet = "clean_avg") %>% 
  clean_names() %>%
  rename("green_algae_ug" = green_algae_avg_3,
         "green_algae_ml" = green_algae_avg_4,
         "diatom_ug" = diatoms_avg_5,
         "diatom_ml" = diatoms_avg_6,
         "tot_conc_ug" = avg_total_conc,
         "tot_density_ml" = avg_tot_cell_count)

comp <- comp[!(row.names(comp) %in% c("1")),]

##double check comp as numeric 
comp <- mutate_at(comp, vars(replicate, green_algae_ug, green_algae_ml, diatom_ug, diatom_ml, tot_conc_ug, tot_density_ml), as.numeric)

comp <- mutate_at(comp, 
                  vars(green_algae_ug, green_algae_ml, diatom_ug, diatom_ml, tot_conc_ug, tot_density_ml), 
                  ~ round(., 2)) %>%
mutate(
  temperature = case_when(
    temp == "C" ~ "8C",
    temp == "H" ~ "30C",
    temp == "R" ~ "21C",
    TRUE ~ NA_character_  
  )
)
##now lets plot using a box plot
##plot byt tempertaures to look at all the replciates and then will average those and put them togther to look at everything at once

comp_ml <- comp %>%
  select(- green_algae_ug,
           - diatom_ug,
           - tot_conc_ug,
         - temp) %>%
  rename("S.quadricauda" = green_algae_ml,
                   "F.pelliculosa" = diatom_ml,
                   "Total Density" = tot_density_ml)

comp_ug <- comp %>%
  select(- green_algae_ml,
         - diatom_ml,
         - tot_density_ml,
         -temp) %>% 
  rename("S.quadricauda" = green_algae_ug,
                   "F.pelliculosa" = diatom_ug,
                   "Total Concentration" = tot_conc_ug)

remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  return(between(x, lower_bound, upper_bound))
}

# Remove outliers from comp_ug for the specified columns
comp_ug_filtered <- comp_ug %>%
  filter(remove_outliers(S.quadricauda) & 
           remove_outliers(F.pelliculosa) & 
           remove_outliers(`Total Concentration`))

# Reshape data for plotting
long_ug <- reshape2::melt(comp_ug_filtered, id.vars = c('temperature', 'replicate'))

df_ug <- subset(long_ug, variable %in% c("S.quadricauda", "F.pelliculosa", "Total Concentration"))

# Create box plot for green_algae_ug, diatom_ug, and tot_conc_ug
ggplot(df_ug, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_wrap(~ temperature, scales = 'free_y') +
  labs(x = '', y = 'Average Algae Concentration (ug)') +
  scale_fill_manual(values=c("#71C231","#925133","#D1BF58")) +
  guides(fill=guide_legend(title="")) +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 8, face = ifelse(seq_along(levels(df_ml$variable)) <= 2, "italic", "plain"))) +
  ggsave(filename = file.path("figures","comp_ml_final.png"), width = 15, height = 10)
  

comp_ml_filtered <- comp_ml %>%
  filter(remove_outliers(S.quadricauda) & 
           remove_outliers(F.pelliculosa) & 
           remove_outliers(`Total Density`))

# Subset the data for green_algae_ml, diatom_ml, and tot_conc_ml
long_ml <- reshape2::melt(comp_ml_filtered, id.vars = c('temperature', 'replicate'))

df_ml <- subset(long_ml, variable %in% c("S.quadricauda", "F.pelliculosa", "Total Density"))

# Create box plot for green_algae_ml, diatom_ml, and tot_conc_ml
ggplot(df_ml, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_wrap(~ temperature, scales = 'free') +
  labs(x = '', y = 'Average Algae Density (mL)') + 
  scale_fill_manual(values=c("#71C231","#925133","#D1BF58")) +
  guides(fill=guide_legend(title="")) +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 8, face = ifelse(seq_along(levels(df_ml$variable)) <= 2, "italic", "plain"))) +
  scale_x_discrete(labels = c(expression(italic("S.quadricauda")), expression(italic("F.pelliculosa")), "Total Density")) 
#+ ggsave(filename = file.path("figures","comp_ug_final.png"), width = 15, height = 10)


##THEN PERFORM ANOVA OR T TEST STATS

library(stats)
##linear regression almost works - same problem aviva had
lm_result_g <- lm(green_algae_ug ~ temp, data = comp)
lm_result_d <- lm(diatom_ug ~ temp, data = comp)

print(summary(lm_result_g))
print(summary(lm_result_d))

#Tukeys HSD post hoc test
model_2 <- aov(green_algae_ug ~ temp, data = comp)
tukey_results2 <- TukeyHSD(model_2)
view(tukey_results2)

model_g <- aov(diatom_ug ~ temp, data = comp)
tukey_results <- TukeyHSD(model_g)

view(tukey_results)

# repeat for ml 
model_3 <- aov(green_algae_ml ~ temp, data = comp)
tukey_results3 <- TukeyHSD(model_3)
view(tukey_results3)

model_4 <- aov(diatom_ml ~ temp, data = comp)
tukey_results4 <- TukeyHSD(model_4)

view(tukey_results4)
