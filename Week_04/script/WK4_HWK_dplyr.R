### Homework: dplyr wrangling ###
### Created by: Fuamai Tago 
### Created on: 2026-09-20
###############################################


###purpose: practice more codes and wrangling dplyr

#Load libraries 
library(palmerpenguins)
library(dplyr)
library(tidyverse)
library(here)
library (ggplot2)

#View and load data 
view(penguins) 
glimpse(penguins) 
head(penguins)


#Homework 4a - calculates the mean and variance of body mass by species, island, and sex without any NAs
#penguins_mean to rename this new column
#use piping to make it easy 
#drop.na() to remove NA's in dataframes
#group_ by () to group species, island and sex 
#summarise() to calculate for mean and put into new column

penguin_mean <- penguins

penguin_mean |>
  drop_na(sex) |>
  group_by(species, island, sex) |>
  summarise(mean_bodymass = mean (body_mass_g,na.rm = TRUE), #mean for bodymass among island, species, sex
            var_bodymass = var(body_mass_g, na.rm = TRUE), # looking for variance of bodymass among the island, species and sex
            n = n()) #look at sample size of group

#Homework 4b - filter out (i.e. excludes) male penguins, then calculates the log body mass, then selects only the columns for species, island, sex, and log body mass, then use these data to make any plot

penguin_plot <- penguins 

penguin_plot |>
  filter(sex == "male") |>
  mutate(log_bm = log(body_mass_g)) |>
  select(Spp = species, island, sex, log_bm) |>
  ggplot(aes(x = Spp, y = log_bm, color = island)) +
           geom_boxplot() + 
           facet_wrap(~island) +
           labs(title = "Log Body Mass of Species by Island", x = "species",
                y = "log body mass", caption = "Source: Palmer Station LTER/ palmerpenguins package") + 
           scale_color_manual(values = c("lightblue","orange", "brown"))+ 
           theme_classic()+
           theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                 plot.subtitle = element_text(face = "bold", size = 12, hjust = 0.5),
                 axis.title = element_text(face = "italic", size = 11, hjust = 0.5),
                 axis.text.x = element_text(size = 10),
                 axis.text.y = element_text(size = 10),
                 legend.box.background = element_rect(fill="linen"),
                 legend.text = element_text(size = 12),
                 panel.background = element_rect(fill = "linen"))

ggsave(here("Week_04","output","penguin_logmass.png"), width = 8)
         
         

         