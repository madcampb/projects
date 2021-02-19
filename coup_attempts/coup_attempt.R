library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

## source data 
# https://www.jonathanmpowell.com/coup-detat-dataset.html

## import data
data <- read_tsv('/Users/madelinecampbell/Documents/GitHub/projects/coup_attempts/powell_thyne_coups_final.txt', col_names = TRUE)

## append rows for 2021 coups

# Jan 6, 2021 USA coup attempt
data[nrow(data) + 1,] = list('USA', 2, 2021, 1, 6, 1, 'MC_update')

# Feb 1, 2021 Myanmar coup success
data[nrow(data) + 1,] = list('Myanmar (Burma)', 775, 2021, 2, 1, 2, 'MC_update')

# Feb 8, 2021 Haiti coup attempt
data[nrow(data) + 1,] = list('Haiti', 41, 2021, 2, 8, 1, 'MC_update')


## group by year and count

data_gpd <- data %>% 
           group_by(year, coup) %>% 
           summarise(n = n()) #%>%
           #pivot_wider(names_from = coup, values_from = n)

data_gpd$coup_result <- ifelse(data_gpd$coup ==2, "success", "attempt")


## plot 
# Grouped
ggplot(data_gpd, aes(fill=coup_result, y=n, x=year)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_bw() + 
  scale_fill_manual(values=c("#999999", "navyblue")) +  # "#56B4E9"))
  labs(x ="Year", y = "Coup d'etats", fill = 'Result')
  
  
# limited to Jan and Feb 
data_lim <- data %>% 
  filter(month < 3) %>%
  group_by(year, coup) %>% 
  summarise(n = n()) 

data_lim$coup_result <- ifelse(data_lim$coup ==2, "success", "attempt")
ggplot(data_lim, aes(fill=coup_result, y=n, x=year)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_bw() + 
  scale_fill_manual(values=c("#999999", "navyblue")) +  # "#56B4E9"))
  labs(x ="Year", y = "Coup d'etats", fill = 'Result')


  
