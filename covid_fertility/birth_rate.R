library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(purrr)
library(tidyverse)
library(readxl)
library(usmap)
library(map)
library(mapdata)
library(zoo)

#############################################
##            import data                  ##
#############################################

# Google Trends (US) 
# March 2019 - Feb 2020
# March 2020 - Feb 2021

# pregnancy
preg20 <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_preg20.csv')
preg19 <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_preg19.csv')

preg19 <- preg19 %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                            variable = 'pregnancy', 
                            year = 2019, 
                            value = pregnancy19) %>%
  select(Week, shortweek, variable, year, value)

preg20 <- preg20 %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                            variable = 'pregnancy', 
                            year = 2020, 
                            value = pregnancy20) %>%
  select(Week, shortweek, variable, year, value)

preg <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_preg.csv')

preg <- preg %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                            variable = 'pregnancy', 
                            year = substr(Week, 1, 4), 
                            value = pregnancy) %>%
  select(Week, shortweek, variable, year, value)

# morning sickness
ms20 <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_mornsick20.csv')
ms19 <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_mornsick19.csv')

ms19 <- ms19 %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                        variable = 'morning sickness', 
                        year = 2019, 
                        value = morningsickness19) %>%
  select(Week, shortweek, variable, year, value)

ms20 <- ms20 %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                        variable = 'morning sickness', 
                        year = 2020, 
                        value = morningsickness20) %>%
  select(Week, shortweek, variable, year, value)

ms <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_ms.csv')

ms <- ms %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                        variable = 'morning sickness', 
                        year = substr(Week, 1, 4), 
                        value = morningsickness) %>%
  select(Week, shortweek, variable, year, value)

# clearblue (pregnancy test) 
cb20 <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_clearblue20.csv')
cb19 <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_clearblue19.csv')

cb19 <- cb19 %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                        variable = 'clearblue', 
                        year = 2019, 
                        value = clearblue19) %>%
  select(Week, shortweek, variable, year, value)

cb20 <- cb20 %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                        variable = 'clearblue', 
                        year = 2020, 
                        value = clearblue20) %>%
  select(Week, shortweek, variable, year, value)

cb <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_cb.csv')

cb <- cb %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                    variable = 'clearblue', 
                    year = substr(Week, 1, 4), 
                    value = clearblue) %>%
  select(Week, shortweek, variable, year, value)

# missed period

mp <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_mp.csv')

mp <- mp %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                    variable = 'missed period', 
                    year = substr(Week, 1, 4), 
                    value = missedperiod) %>%
  select(Week, shortweek, variable, year, value)

## BIG JOIN 

df_long <- bind_rows(preg19, preg20, ms19, ms20, cb19, cb20)
df_5yr <- bind_rows(preg, ms, cb, mp) %>% 
  group_by(variable) %>%
  mutate(rolling_avg=rollapply(value,10,mean,align='right',fill=NA))



# US states 2019 vs 2020
preg_states20 <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/geoMap_preg_US2020.csv')
preg_states19 <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/geoMap_preg_US2019.csv')

preg_states <- left_join(preg_states19, preg_states20, by=c('Region'))

preg_states <- preg_states %>% mutate_at('pregnancy19',as.numeric) %>% 
  mutate_at('pregnancy20',as.numeric) %>% 
  mutate(p_diff = (pregnancy20 - pregnancy19)/((pregnancy20 + pregnancy19)/2))

preg_states$fips <- fips(preg_states$Region)

#############################################
##            make a  map                  ##
#############################################

# state <- map_data("state")

plot_usmap(data=preg_states, values='p_diff') + 
  scale_fill_continuous( name = "Percent Difference", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Change in Google searches for 'pregnancy' between 2019 and 2020", caption = "Source: Google Trends") + 
  theme(panel.background=element_blank())


#############################################
##           time series plot              ##
#############################################


p <- ggplot(df_5yr %>% filter(year > 2016, variable != 'clearblue'), aes(x=shortweek, y=rolling_avg, color=year, group=year)) +
  geom_line() + 
  geom_vline(xintercept = 55, color='red3', linetype='dashed', size=.25) +
  facet_wrap(~ variable) + # scales="free_y"
  theme(axis.text.x = element_blank()) +
  scale_color_manual(values=blues9) + 
  labs(
    title = "Baby bust",
    subtitle = "United States, Fertility-related Google searches, 2017-2021",
    x ="Time", 
    y="Google searche frequency (rolling average)",
    fill = '', 
    caption = "Data source: Google Trends") 

p






#######################################################################################################################################


# # UK
# # https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/provisionalbirthsinenglandandwales
# # first three quarters of 2020 -- no good
# 
# uk_df <- read_excel('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/provisionalbirthtables2020.xlsx')

# Google trends
# https://trends.google.com/trends/explore?date=2019-03-10%202020-03-10&q=morning%20sickness

gt2019 <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/geoMap_2019.csv')
gt2020 <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/geoMap_2020.csv')

gt_df <- left_join(gt2019, gt2020, by=c('Country'))

gt_df_lim <- gt_df %>% mutate_at('pregnancy_19',as.numeric) %>% 
  mutate_at('pregnancy_20',as.numeric) %>% 
  filter(pregnancy_20 > 20) %>%
  mutate(p_diff = (pregnancy_20 - pregnancy_19)/((pregnancy_20 + pregnancy_19)/2))


# COVID data (infection rate)
# WHO
# https://covid19.who.int/table

covid <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/WHO COVID-19 global table data March 11th 2021 at 11.16.40 PM.csv')
# UK, Tanzania, US


# GDP (per capita)
# World Bank
# https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?view=map
wb_gdp <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/WB_GDP/GDP_PCAP.csv')
wb_gdp <- wb_gdp %>% select(Country.Name, Country.Code, Indicator.Name, X2019)

wb_metadata <-read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/WB_GDP/Metadata_Country.csv')

wb <- left_join(wb_gdp, wb_metadata, by=c('Country.Code'))


#############################################
##         combine everything              ##
#############################################

merge1 <- left_join(gt_df_lim, wb, by=c("Country" = "Country.Name")) 
final <- left_join(merge1, covid, by=c("Country" = "Name")) 

#############################################
##            make the chart               ##
#############################################

# x-axis: COVID
# y-axis: change in 'pregnant' Google searches
# make bubbles bigger by per capita GDP
# color based on region

g <- ggplot(final, aes(x=Cases...cumulative.total.per.100000.population, y=p_diff, color=Region, label=Country)) + 
  geom_point(aes(size=X2019)) +
  geom_text( hjust = 0, nudge_x = 0.5) # check_overlap = TRUE,
  # geom_smooth(method=lm)

g