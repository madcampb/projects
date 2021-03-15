library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(purrr)
library(tidyverse)
library(wesanderson)

# source data
#https://www.ncdc.noaa.gov/billions/time-series/US

# import data 
weather <- read_csv('/Users/madelinecampbell/Documents/GitHub/projects/weather_events/time-series-US-1980-2020.csv')

drought <- weather %>% 
  dplyr::select('Year', contains('Drought')) %>%
  set_names(~ str_to_lower(.) %>%
              str_replace_all("drought", "") %>%
              str_replace_all(" ", "")) %>%
  mutate(storm_type = 'drought')

flood <- weather %>% 
  dplyr::select('Year', contains('Flooding')) %>%
  set_names(~ str_to_lower(.) %>%
              str_replace_all("flooding", "") %>%
              str_replace_all(" ", "")) %>%
  mutate(storm_type = 'flooding')

freeze <- weather %>% 
  dplyr::select('Year', contains('Freeze')) %>%
  set_names(~ str_to_lower(.) %>%
              str_replace_all("freeze", "") %>%
              str_replace_all(" ", "")) %>%
  mutate(storm_type = 'freeze')

storm <- weather %>% 
  dplyr::select('Year', contains('Severe Storm')) %>%
  set_names(~ str_to_lower(.) %>%
              str_replace_all("severe storm", "") %>%
              str_replace_all(" ", "")) %>%
  mutate(storm_type = 'severe storm')

cyclone <- weather %>% 
  dplyr::select('Year', contains('Tropical Cyclone')) %>%
  set_names(~ str_to_lower(.) %>%
              str_replace_all("tropical cyclone", "") %>%
              str_replace_all(" ", "")) %>%
  mutate(storm_type = 'tropical cyclone')

wildfire <- weather %>% 
  dplyr::select('Year', contains('Wildfire')) %>%
  set_names(~ str_to_lower(.) %>%
              str_replace_all("wildfire", "") %>%
              str_replace_all(" ", "")) %>%
  mutate(storm_type = 'wildfire')

winter <- weather %>% 
  dplyr::select('Year', contains('Winter Storm')) %>%
  set_names(~ str_to_lower(.) %>%
              str_replace_all("winter storm", "") %>%
              str_replace_all(" ", "")) %>%
  mutate(storm_type = 'winter storm')

## append rows for 2021 storm

# 2021 Texas+ 
winter[nrow(winter) + 1,] = list(2021, 1, 20, 0, 0, 0, 0, 0, 0, 'winter storm')


all <- weather %>% 
  dplyr::select('Year', contains('All Disasters')) %>%
  set_names(~ str_to_lower(.) %>%
              str_replace_all("all disasters", "") %>%
              str_replace_all(" ", "")) %>%
  mutate(storm_type = 'all')


df_long <- bind_rows(drought, flood, freeze, storm, cyclone, wildfire, winter, all)

## AccuWeather estimates winter storm may cost $45-50 billion in damages 
# https://www.accuweather.com/en/winter-weather/accuweather-estimates-economic-impact-of-winter-storms/902563

# other estimates suggest storm could top Harvey for cost of damages.
# Hurricane harvey reportedly caused $125b in damages 

# plot 

ggplot(df_long %>% filter(storm_type != 'all'), aes(fill=storm_type, y=count, x=year)) + 
  geom_bar(position="stack", stat="identity") + 
  geom_line(data=all, aes(x=year, y=cost/18), color="red3", size=1, stat="identity", show.legend = FALSE) +
  geom_errorbar(data=all, aes(ymin=lower95/18, ymax=upper95/18), color="red3", width=0.5, size=.8, show.legend = FALSE) +
  geom_errorbar(aes(x=2021, ymin=20/18, ymax=125/18), color="red3", linetype="dashed", width=0.5, size=.8, show.legend = FALSE) +
  annotate("text", x = 2003, y = 15, label = "Hurricane\n Katrina", size=3) + 
  annotate("text", x = 2015, y = 23.5, label = "Hurricanes\n Harvey,\n Maria,\n and Irma", size=3) + 
  annotate("text", x = 2022, y = 8, label = "Winter\n Storm", size=3) + 
  scale_y_continuous(

    # Features of the first axis
    name = "Number of weather events",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*18, name="Cost in billions (CPI-adjusted $)")
  ) +
  theme_bw() + 
  scale_fill_manual(values=c("red3", "midnightblue", "steelblue", "steelblue1", "lightskyblue3", "aquamarine4", "gold2", "salmon2")) +  # "#56B4E9"))
  labs(
    title = "The damage is done",
    subtitle = "United States, Billion dollar disaster events, 1980-2021",
    x ="Year", 
    fill = '', 
    caption = "Data source: NOAA; Center for Climate and Energy Solutions") +
  theme(axis.line.y.right = element_line(color = "red3"), 
        axis.ticks.y.right = element_line(color = "red3"),
        axis.text.y.right = element_text(color = "red3"), 
        axis.title.y.right = element_text(color = "red3"),
        plot.caption = element_text(hjust = 0)
  )


## follow up analysis 

# not a notable increase in cost per storm 
# clear spikes for Katrina, Sandy, Harvey, and something in the late 80s
ggplot(all %>% mutate(avg_cost_per = cost / count) , aes(x=year, y=avg_cost_per)) + 
  geom_line()

# how much have costs gone up? 
cost1980 <- all %>% filter(year == 1980) %>%
  select(cost)
cost2020 <- all %>% filter(year == 2020) %>%
  select(cost)

p_change <- (cost2020[[1,1]] - cost1980[[1,1]]) / cost1980[[1,1]]



