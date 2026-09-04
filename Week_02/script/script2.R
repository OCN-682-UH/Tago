###This is my first script. I am learning how to import data. 
### Created by: Fuamai Tago 
### Created on: 2026-09-04

### load libraries ###
library(tidyverse)
library(here)

###Read in data 
weightdata <- read.csv(here("Week_02", "data", "weightdata.csv"))

### Data analysis ###

head(weightdata) #looks at top 6 lines of dataframe

tail(weightdata) #looks at bottom 6 lines of dataframe

view(weightdata) #opens new window to look at entire dataframe




