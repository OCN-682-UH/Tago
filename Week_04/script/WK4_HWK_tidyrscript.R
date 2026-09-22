### Homework: Dplyr Wrangling###
### Created by: Fuamai Tago 
### Created on: 2026-09-15

###purpose: try out chemistry data and wrangling tidyr
##########################################################


#Load Libraries
library(dplyr)
library(tidyverse) #load core pckg 
library(here) #build robust file pathway
library (ggplot2)

#load data 
chem_dd <- read_csv(here("Week_04", "data", "chem_data_dictionary.csv"))
glimpse(chem_dd)

chemi_dd <- read_csv(here("Week_04", "data", "chemicaldata_maunalua.csv"))
glimpse(chemi_dd)
View(chemi_dd)

#remove all NA by drop_na() 
#separate_wider_delim by splitting messy Tide_time data, so one column holds one info

chemi_dd_clean <- chemi_dd |>
  drop_na() |> #removing all NA
  separate_wider_delim(cols = "Tide_time",
                       delim = "_",
                       names = c("Tide", "Time"),
                       cols_remove = FALSE)

head(chemi_dd_clean)

#filter by fall and spring season, tide, and time 
chemi_dd_clean|>
  filter(Season == "Fall", Season == "Spring", Tide == "Low") #filtering vor observation

glimpse(chemi_dd_clean)
head(chemi_dd_clean)

#pivot into long data 

chemi_dd_long <- chemi_dd_clean |> 
  pivot_longer(cols = Phosphate:Silicate,
               names_to = "Variables",
               values_to = "Values") 

#view long data 
head(chemi_dd_long) 
glimpse(chemi_dd_long)

#calculate summary statistics: mean, variance, SD for all variables by site, season and tide
#export summary stat file to output 

chemi_dd_long |>
  group_by(Variables, Site, Season, Tide) |> #mean, SD, variance  
  summarise(Param_mean = mean(Values, na.rm = TRUE), 
            Param_vars = var(Values, na.rm = TRUE),
            Params_sd = sd(Values, na.rm = TRUE)) |>
  write.csv(here("Week_04","output","chemi_summary_stat.csv")) #export summary stat as csv

head(chemi_dd_long)

#research question:Compare the phosphate or silicate in the fall and spring during low tides?
#make plot but not boxplot 

phosphate_silicate_plot <- ggplot(chemi_dd_long,
                                  mapping = aes(x = Variables,
                                                y = Values,
                                                color = Season)) + 
  geom_point(size = 2, alpha = 0.5) + 
  facet_wrap(~Site) +
  labs (title = "Nutrient Concentrations by Site and Season",
        subtitle = "Comparing Phosphate and Silicate Levels at Maunaloa during Fall and Spring", 
        x = "Nutrient Concentrations", 
        y = "Nutrient Levels (umol/L)",
        caption = "Data: Silbiger et al. (2020), Maunalua Bay, Hawaii") + 
  scale_color_manual(values = c("orange", "purple")) +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 12, hjust = 0.5),
        axis.title = element_text(face = "plain", size = 11, hjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.box.background = element_rect(fill="aliceblue",color="seashell"),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = "aliceblue"))

phosphate_silicate_plot

#save plot 
ggsave(here("Week_04", "output","phosphate_silicate_plot.png"), width = 8, height = 6)




