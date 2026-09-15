###Intro to Plotting: Homework###
### Created by: Fuamai Tago 
### Created on: 2026-09-13
################################################

##loading libraries or packages
library(tidyverse) #opens data toolkit
library(palmerpenguins) #penguin data
library (here)
library (devtools)
library (beyonce)
library (ggthemes)
library (colorBlindness)
glimpse (penguins) # view penguin data 

### create ggplot homework ###

# research question: Do bodymass differ across species sex, and if it does compare the bodymass across the three species?
# use boxplot to compactly display the distribution of bodymass per species sex
#facet_wrap() will split graph in species category
#theme_classic() improve the boxplot visualization
#theme() will enhance graph, clean up data 
#ggsave function to save image/graph

#removing the NA
penguins_clean <- penguins[!is.na(penguins$sex),]

#ggplot 
ggplot(data = penguins_clean,
       mapping = aes(x = sex,
                     y = body_mass_g,
                     color = sex))+ 
  geom_boxplot(linewidth = 0.5)+
  facet_wrap(~species)+
  labs(title = "Body Mass (g) by Sex Across Penguin Species",
       subtitle = "Comparing bodymass among Adelie, Chinstrap, and Gentoo Penguins",
       x = "sex",
       y = "Body Mass (g)", 
       caption = "Source: Palmer Station LTER/ palmerpenguins package")+
  scale_color_manual(values = c("pink","black"))+ 
  theme_classic() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 12, hjust = 0.5),
        axis.title = element_text(face = "italic", size = 11, hjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.box.background = element_rect(fill="aliceblue",color="black"),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = "aliceblue"))

ggsave(here("Week_03","output","penguinplot.png"), width = 8)

# this was honestly an interesting and fun plot.
# the time put into it and the various different combinations I had to put together to make this graph look pretty, was worth the effort. 

  