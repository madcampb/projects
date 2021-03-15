library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(purrr)
library(tidyverse)
library(zoo)

#############################################
##            import data                  ##
#############################################


# pregnancy
preg <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_preg.csv')

preg <- preg %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                        variable = 'pregnancy', 
                        year = substr(Week, 1, 4), 
                        value = pregnancy) %>%
  select(Week, shortweek, variable, year, value)

# morning sickness
ms <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/covid_fertility/USA/multiTimeline_ms.csv')

ms <- ms %>% mutate(shortweek = substr(Week, nchar(Week)-5+1, nchar(Week)), 
                    variable = 'morning sickness', 
                    year = substr(Week, 1, 4), 
                    value = morningsickness) %>%
  select(Week, shortweek, variable, year, value)


# clearblue
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

### APPEND EVERYTHING
df_5yr <- bind_rows(preg, ms, cb, mp) %>% 
  group_by(variable) %>%
  mutate(rolling_avg=rollapply(value,10,mean,align='right',fill=NA))



#############################################
##           time series plot              ##
#############################################


p <- ggplot(df_5yr %>% filter(variable != 'clearblue'), aes(x=shortweek, y=rolling_avg, color=year, group=year)) +
  geom_line() + 
  geom_vline(xintercept = 45, color='red3', linetype='dashed', size=.25) +
  facet_wrap(~ variable) + # scales="free_y"
  theme(axis.text.x = element_blank()) +
  scale_color_manual(values=blues9) + 
  labs(
    title = "Baby bust",
    subtitle = "United States, Fertility-related Google searches, 2016-2021",
    x ="Time", 
    y="Google searche frequency (rolling average)",
    fill = '', 
    caption = "Data source: Google Trends") 

p
