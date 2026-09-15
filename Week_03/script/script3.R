## Install package  
install.packages("palmerpenguins") 

##loading libraries or packages
library(tidyverse) #opens data toolkit
library(palmerpenguins) #penguin data
library (here) 
glimpse(penguins) #view raw penguin data

### create ggplot ###

##practice##

ggplot (data = penguins, 
        mapping = aes (x = bill_depth_mm, 
                       y = bill_length_mm, color = species)) + 
  geom_point() +
  geom_smooth()+
  labs (title = "Bill Depth and length", 
        subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins", 
        x = "Bill depth (mm)", y = "Bill length (mm)", 
        color = "Species", 
        caption = "Source: Palmer Station LTER/ palmerpenguins package") + 
  scale_color_viridis_d()












