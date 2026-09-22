### Tidyr Wrangling 101###
### Created by: Fuamai Tago 
### Created on: 2026-09-15

###purpose: learn about and practice with tidyr
##########################################################


#load libraries 
library(tidyverse)
library(here)


#load data
chemi_dd <- read.csv(here("Week_04", "data", "chemicaldata_maunalua.csv"))
glimpse(chemi_dd)                     

#remove all NA

chemi_clean <- chemi_dd |>
  filter(complete.cases(chemi_dd)) #filters everything not in complete row; this function is similar to drop.na() but is the older version

?separate_wider_delim #split string into columns by delimiter; current replacement of separate()

#separate the two columns via separate_wider_delim function
#separate function splits the columns and deletes original column, use cols_remove = FALSE to keep it

chemi_clean <- chemi_dd |>
  filter(complete.cases(chemi_dd))|>
  separate_wider_delim(cols = "Tide_time",
                     delim = "_",
                     names = c("Tide","Time"),
                     cols_remove = FALSE) 


head(chemi_clean)

chemi_clean2 <- chemi_dd |>
  drop_na() |> #try out drop_na function
  separate_wider_delim(cols = "Tide_time",
                       delim = "_",
                       names = c("Tide", "Time"),
                       cols_remove = FALSE)

head(chemi_clean2)

#to combine two columns use paste()
# paste(x, y, sep = ".") this means x and y are joined by "."
# sep argument controls separator

paste("Maunalua", "Fringing", sep = ".") 

#combine Site and Zone columns in data frame 
chemi_clean <- chemi_dd |>
  filter(complete.cases(chemi_dd))|> #use drop.na() as it is simpler and is updated version
  separate_wider_delim(cols = "Tide_time",
                       delim = "_",
                       names = c("Tide","Time"),
                       cols_remove = FALSE) |>
  mutate(Site_Zone = paste(Site, Zone, sep = "."))

head(chemi_clean)

#pivot data 
# wide -> long : pivot_longer()
# long -> wide : pivot_wider()
#pivot data to be long data

chemi_long <- chemi_clean |>
  pivot_longer(cols = Temp_in:percent_sgd, #select columns to pivot
               names_to = "Variables", #new column for old column
               values_to = "Values") #new column for values

head(chemi_long)

#calculate meana and variance of data
#group_by - group variable name to get summary stat for every variable
#facet_wrap - make one plot per variable instead of multiple separate ones

chemi_long |>
  group_by(Variables, Site) |>
  summarise(Param_means = mean(Values, na.rm = TRUE), 
            Param_vars = var(Values, na.rm = TRUE))

#calculate mean, variance, std for all variables (site, zone, tide)

chemi_long |>
  group_by(Variables, Site, Zone, Tide) |>
  summarise(Param_means = mean(Values, na.rm = TRUE), 
            Param_vars = var(Values, na.rm = TRUE))


#create boxplots of ever parameter by site 
#use facet_wrap to make one plot
#fix the axes by scales = "free" release both x and y 

chemi_long |>
  ggplot(aes(x = Site, y = Values)) +
  geom_boxplot() +
  facet_wrap(~Variables, scales = "free")

ggsave(here("Week_04", "output", "chemi_long_boxplot.png"), width = 8)

#make data wide 

chemi_wide <- chemi_long |>
  pivot_wider(names_from = Variables, #names_from = extract data and transform into new columns
              values_from = Values) #values_from = where actual cell values should come from 

head(chemi_wide)

#full pipeline to the export using above codes

chemi_dd |>
  drop_na() |>
  separate_wider_delim(cols = "Tide_time",
                       delim = "_",
                       names = c("Tide","Time"),
                       cols_remove = FALSE) |>
  pivot_longer(cols = Temp_in:percent_sgd,
               names_to = "Variables",
               values_to = "Values") |>
  group_by(Variables, Site, Time) |>
  summarise(mean_vals = mean(Values, na.rm = TRUE)) |>
  pivot_wider(names_from = Variables,
              values_from = mean_vals) |>
  write_csv(here("Week_04","output", "summary.csv")) #write_csv saves file & returns to dataframe

#install package 

install.packages("cowsay")
library(cowsay)
say("I love tidy data!", by = "cat")
say("I love tidy data!", by = "fish")
say("I love R-studio!", by = "blowfish")

#this was cool!

