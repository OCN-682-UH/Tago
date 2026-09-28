### Classwork: data join###
### Created by: Fuamai Tago 
### Created on: 2026-09-15

###purpose: join data and practice in 
##########################################################

library(dplyr)
library(tidyverse) #load core pckg 
library(here) #build robust file pathway
library (ggplot2)

#load data# 
data_dictionary<- read_csv(here("Week_05", "data", "data_dictionary.csv"))
topt_data <- read_csv(here("Week_05", "data", "Topt_data.csv"))
site_charact <- read_csv(here("Week_05", "data", "site.characteristics.data.csv"))

T1 <- tibble(
  Site.ID = c("A","B","C","D"),
  Temperature = c(14.1,16.7,15.3,12.8))

T1

T2 <- tibble(
Site.ID = c("A", "B", "D", "E"),
pH = c(7.3, 7.8, 8.1, 7.9))
  
T2

left_join(T1, T2) #keep all rows from left dataframe & add rows from right
right_join(T1,T2)
inner_join(T1,T2)
full_join(T1,T2)
semi_join(T1, T2)
anti_join(T1, T2)

T3 <- tibble(
SiteID = c("A", "B", "C", "D"),  # Note: different name!
Chlorophyll = c(2.3, 3.1, 1.9, 2.8))

T3

left_join(T1, T3, by = c("Site.ID" = "SiteID"))

T4 <- tibble(
Site.ID = c("A", "A", "B", "B"),
Year = c(2020, 2021, 2020, 2021),
Biomass = c(12.5, 15.3, 18.2, 16.9))

T4

T5 <- tibble(
Site.ID = c("A", "A", "B"),
Year = c(2020, 2021, 2021),
Nutrients = c(8.2, 7.9, 9.1))

T5


T6 <- tibble(
Site.ID = c("A", "B", "C"),
Notes = c("pristine", "degraded", "moderately impaired"))

T7 <- tibble(
Site.ID = c("A", "B", "D"),
Notes = c("sunny", "shaded", "partially shaded"),
Quality = c("good", "fair", "poor"))

# Don't specify how to join — creates ambiguity with 'Notes'
left_join(T6, T7, by = "Site.ID")

#add the remaining things 




#DATE/TIME/YEAR

now()
now(tzone = "EST")
now(tzone = "GMT")
now(tzone = "US/Hawaii")

today()
today(tzone = "GMT")

am(now()) # is it morning?

leap_year(now())

ymd("2021-02--24")
mdy("02/04/2021")
mdy("February 24 2021")
dmy("24/02/2021")

ymd_hms("2021-02-24 10:22:20 PM")
mdy_hms("02/24/2021 22:22:20")
mdy_hm("February 24 2021 10:22 PM")

datetimes <- c("02/24/2021 22:22:20","02/25/2021 11:21:10","02/26/2021 8:01:52")
datetimes

datetimes <- mdy_hms(datetimes)
datetimes

month(datetimes)
month(datetimes, label = TRUE) #converting 2 into February by putting label as TRUE
month(datetimes, label = TRUE, abbr = FALSE)
day (datetimes) #pull days for that month
wday(datetimes, label = TRUE)
hour(datetimes)
minute(datetimes)
second(datetimes)

datetimes + hours(4) #hour extracts hour component while hours adds hours to a datetime; so this means that adding 4hrs 
datetimes + days(2) #adding 2 days to datetime (making time units plural = addition)
datetimes + months(1)

round_date(datetimes, "minute")
round_date(datetimes, "5 mins") # you can round to any time unit


#create a datetime WITHOUT timezone info
#data collected in HI but computer is in EST, this object has NO timezone info
datetime_naive <- mdy_hms("02/24/2021 10:22:20")
datetime_naive

# Assume the naive time is in Hawaii
hawaii_time <- with_tz(datetime_naive, tzone = "US/Hawaii")
hawaii_time

# Same moment, viewed from EST
est_time <- with_tz(hawaii_time, tzone = "EST")
est_time

# Claim this was collected in Hawaii (though it was naive)
#force_tz changes clock reading bc youre assigning timezone info to a prev. naive datetime

force_hawaii <- force_tz(datetime_naive, tzone = "US/Hawaii")
force_hawaii

# Now convert to EST (this changes the clock time!)

with_tz(force_hawaii, tzone = "EST")


##CHALLENGE##

cond_data <- read_csv(here("Week_05", "data", "CondData.csv")) |>
mutate(datetime = mdy_hms(datetime_column))


#join site characteristics in Topt data 

topt_data <- read_csv (here("Week_05","data","Topt_data.csv"))
topt_data

site_charact <- read_csv(here("Week_05","data","site.characteristics.data.csv"))
site_charact

#pivot wide

wide_site_charact <- site_charact |>
  pivot_wider(names_from = "parameter.measured",
              values_from = "values")
glimpse(wide_site_charact)

joined_files <- full_join(topt_data, wide_site_charact)
view(joined_files)

#practice more 


#loading more ggplots

library(ggplot2)
install.packages("ggcats")
library(ggcats)

#create sample data 
df<- data.frame(x = c(1,2,3,4,5),
                y = c(2,4,3,5,4))
ggplot(df, aes(x,y)) + 
  geom_cat(cat = "nyancat",
           size = 4) +
  labs(title = "Cats!",
       x = "X", y = "Y") +
  theme_minimal()