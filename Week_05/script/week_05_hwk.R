### Homework: Week_05###
### Created by: Fuamai Tago 
### Created on: 2026-09-28
##########################################################


#load data 

cond_data <- read_csv(here("Week_05", "data", "CondData.csv"))
depth_data <- read_csv(here("Week_05", "data", "Depthdata.csv"))
glimpse (cond_data)
glimpse(depth_data)

#convert date columns appropriately  

clean_cond <- cond_data |>
  mutate(datetime = mdy_hms(date)) |>
  select(-date)

clean_depth <- depth_data |>
  mutate(datetime = ymd_hms(date)) |>
  select(-date)

#round counductivity data to nearest 10 secs 

clean_cond <- cond_data |>
  mutate(datetime = mdy_hms(date)) |>
  select(-date) |>
  mutate(datetime = round_date(datetime, seconds(10)))

#join dataframes using inner_join 
#relocate function to move datetime column to first column (cleaner look)

cond_depth <- inner_join(clean_cond, clean_depth) |>
  relocate(datetime,.before = Temperature)

View(cond_depth)

#Calculate averages of date, depth, temperature, and salinity by minute


avg_cond_depth <- cond_depth |> 
  mutate(estimate_date = round_date(datetime, "minute")) |>
  group_by(estimate_date) |>
  summarise(mean_date = mean(estimate_date,na.rm = TRUE),
            mean_temp = mean(Temperature, na.rm = TRUE),
            mean_salinity = mean(Salinity, na.rm = TRUE),
            mean_depth = mean(Depth, na.rm = TRUE)) |>
  pivot_longer(cols = mean_temp:mean_depth,
                                 names_to = "avg_variables",
                                 values_to = "avg_values") |>
  ggplot(aes(x = estimate_date,
             y = avg_values,
             color = avg_variables)) + geom_jitter(size = 2, alpha = 0.6) + 
  geom_smooth(colour = "#4A708B")+
  facet_wrap(~avg_variables, scales = "free", 
             nrow = 3,
             labeller = as_labeller(c("mean_depth" = "Depth (m)",
                                    "mean_salinity" = "Salinity (psu)",
                                    "mean_temp" = "Temperature (C)"))) +
  labs(title = "Average Depth, Salinity, Temperature collected in January 2021",
       x = "Time (hh:mm)",
       y = "Values",
       caption = "Danielles Data Collected in 2021") +
  scale_color_manual(values = c("lightblue","orange", "brown"))+ 
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "plain", size = 11, hjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_rect(fill = "linen"),
        legend.position = "none")

avg_cond_depth

##save output

ggsave(here("Week_05","output","wk_05_hwk.png"), width = 8, height = 5)


