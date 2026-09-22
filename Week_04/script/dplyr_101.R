### Classwork: Dplyr Wrangling###
### Created by: Fuamai Tago 
### Created on: 2026-09-15

###purpose: practice more codes and ggplot
################################################


#Load Libraries
library(palmerpenguins)
library(tidyverse)
library(here)
library (ggplot2)


#Load Data
#Data is part of package and its called penguins
glimpse(penguins)
head (penguins)

#filter

girl_penguins <- filter(.data = penguins, sex == "female" ) #filter only for female penguins
head (girl_penguins)

filter(.data = girl_penguins, year == "2008" )
filter(.data = girl_penguins, body_mass_g > 5000)
filter(.data = penguins, year == 2008 )
filter(.data = girl_penguins, sex == "female", body_mass_g > 5000) # 2 variable factor and integer

filter(.data = girl_penguins, sex == "female" & body_mass_g > 5000) # use Boolean operator logic "&"

filter(.data = girl_penguins, year == 2008| year == 2009) #basic with no piping
girl_penguins |>
  filter(year %in% c(2008,2009)) #without piping; %ni% = function that checks if elements of one vector or value exist inside another vector; c() = join all things into vector

filter(.data = penguins, !island == "Dream")

nightmare <- filter(.data = penguins, !island == "Dream")
view (nightmare)

nightmare1 ->filter(.data = penguins, !island == "Dream")
head (nightmare1)

filter(.data = penguins, species == "Adelie"& species == "Gentoo")

filter(.data = penguins, species %in% c("Adelie","Gentoo"))

#mutate
mutate (.data = penguins,
        body_mass_kg = body_mass_g /1000) # divide and add new column

mutate (.data = penguins,
        body_mass_kg = body_mass_g /1000,
        bill_length_depth = bill_length_mm / bill_depth_mm) # change multiple columns at once

penguins |>
  mutate(across(where(is.numeric), ~ round(.x, 1))) # mutating columns at once you can use across() inside mutate()

#ifelse

mutate(.data = penguins,
       after_2008 = if_else(year > 2008, "After 2008", "Before 2008")) # 

mutate(.data = penguins,
       flipper_bodymass = flipper_length_mm + body_mass_g) # addition and creating new column for this sum

mutate (.data = penguins,
        obese = if_else(body_mass_g > 4000, "Big", "Small")) # new column 

penguins |> 
  mutate(chonk = if_else(body_mass_g > 4000, "big", "small")) # piped the data to this and called it chonk


#piping activity 

penguins |> 
  filter(sex == "female") |>
  mutate(log_mass = log(body_mass_g))

penguins |> 
  filter(sex == "female") |>
  mutate(log_mass = log(body_mass_g)) |>
  select(species, island, sex, log_mass) # select  to certain columns to remain in dataframe


penguins |> 
  filter(sex == "female") |>
  mutate(log_mass = log(body_mass_g)) |>
  select(Species = species, island, sex, log_mass) # rename columns 

penguins |> 
  arrange(body_mass_g) # sort rows by columns (ascending to default)

penguins |> 
  arrange(desc (body_mass_g)) # descending order

penguins |> 
  summarise (mean_flipper = mean(flipper_length_mm, na.rm = TRUE)) # computes a table pf summarized data; calculate the mean flipper length and exclude NA


penguins |> 
  summarise (mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
             min_flipper = min (flipper_length_mm, na.rm = TRUE)) # computes a table pf summarized data; calculate the mean flipper length and exclude NA

penguins |> 
  group_by(island) |>
  summarise (mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
             max_flipper = max(flipper_length_mm, na.rm = TRUE),
             n = n()) #group, calculate mean, max and n() counts number of rows in each group
penguins |> 
  group_by(island, sex) |>
  summarise (mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
             max_flipper = max(flipper_length_mm, na.rm = TRUE)) # group multiple variables 

penguins |>
  count(species) # count rows 

penguins |>
  count(species, island) # count multiple variables at once

penguins |>
  drop_na(sex) # drop rows with NA from specific column

penguins |> 
  drop_na(sex) |>
  group_by(island, sex) |>
  summarise (mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
             max_flipper = max(flipper_length_mm, na.rm = TRUE)) # combination!!

penguins |>
  drop_na(sex) |>
  ggplot(aes(x = sex, y = flipper_length_mm)) +
  geom_boxplot() # combine and make graph

ggsave(here("Week_04","output","flipperplot.png"), width = 8)

