library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(purrr)
library(tidyverse)
library(zoo)


### read data
# opensecrets
# https://www.opensecrets.org/personal-finances/top-net-worth?year=2018
df <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/politicians_net_worth/data/net_worth_pchange.csv')


### data manipulation

df <- df %>% 
  arrange(name, year) %>%
  group_by(name) %>% 
  mutate(pct_change = (net_worth - lag(net_worth)) / lag(net_worth), # python calculation same
         raw_change = net_worth - lag(net_worth), 
         start_neg = case_when(lag(net_worth) <= 0 ~ 1, 
                               TRUE ~ 0),
         yr_rank = rank(year))
  

df %>% filter(pct_change > -100, 
              pct_change < 100, 
              party != 'I') %>%
  group_by(party) %>%
  summarise(nw_mean = mean(net_worth), 
            nw_median = median(net_worth),
            nw_max = max(net_worth), 
            nw_min = min(net_worth),
            pc_mean = mean(pct_change), 
            pc_median = median(pct_change),
            pc_max = max(pct_change), 
            pc_min = min(pct_change))

yr_range <- df %>% 
  group_by(name) %>%
  summarise(min_year = min(year), 
            max_year = max(year), 
            n_years = n()) %>% 
  ungroup()

df_yr <- df %>% 
  full_join(yr_range, by=c('name' = 'name')) %>%
  filter(year == min_year | year == max_year) %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(pct_change = (net_worth - lag(net_worth)) / lag(net_worth), # same thing (net_worth/lag(net_worth) - 1), 
         pct_change_an = pct_change/n_years, 
         raw_change = net_worth - lag(net_worth), 
         raw_change_an = raw_change/n_years, 
         start_neg = case_when(lag(net_worth) <= 0 ~ 1, 
                               TRUE ~ 0))


# what are the means by party

df %>% 
  filter(#pct_change < 1000, pct_change > -1000, 
         start_neg != 1, 
         !is.na(pct_change)) %>% 
  group_by(party) %>% 
  summarise(pct_c = mean(pct_change), 
            pct_c_median = median(pct_change),
            std = sd(pct_change),
            raw_c = mean(raw_change), 
            raw_c_median = median(raw_change),
            n_pol_yrs = n())

df_yr %>% 
  filter(#pct_change < 1000, pct_change > -1000, 
         start_neg != 1, 
         !is.na(pct_change_an), 
         party != 'I') %>% 
  group_by(party) %>% 
  summarise(pct_c = mean(pct_change_an), 
            pct_c_median = median(pct_change_an),
            std = sd(pct_change_an),
            raw_c = mean(raw_change_an), 
            raw_c_median = median(raw_change_an),
            n_pols = n())


### plots
# distributions

# annual pct change looks better logged
# can this be logged?? introducing nan?
ggplot(df %>% filter(pct_change < 1000 & pct_change > -1000), aes(log(pct_change))) + geom_density(fill="blue")

ggplot(df_yr %>% filter(pct_change < 1000 & pct_change > -1000), aes(pct_change)) + geom_density(fill="blue")

ggplot(df_yr %>% 
         filter(pct_change < 100 & pct_change > -100, party != 'I'), 
       aes(x=pct_change, group=party, fill=party)) + 
  geom_density() + 
  scale_fill_manual(values=c(D = "steelblue", R = "red3", I="darkgreen"))


# just the senate
ggplot(df_yr %>% 
         filter(pct_change < 1000 & pct_change > -1000, party != 'I', chamber =='Senate'), 
       aes(x=pct_change, group=party, fill=party)) + 
  geom_density() + 
  scale_fill_manual(values=c(D = "steelblue", R = "red3", I="darkgreen"))

# just the house
# long long tail here
ggplot(df_yr %>% 
         filter(pct_change < 100 & pct_change > -100, party != 'I', chamber =='House'), 
       aes(x=pct_change, group=party, fill=party)) + 
  geom_density() + 
  scale_fill_manual(values=c(D = "steelblue", R = "red3", I="darkgreen"))


# box plots

bx <- ggplot(df, aes(x=party, y=log(pct_change))) + 
  geom_violin() + 
  geom_boxplot(width=0.1)

bx

bx_an <- ggplot(df_yr, aes(x=party, y=pct_change_an)) + 
  geom_violin() + 
  geom_boxplot(width=0.1)

bx_an


# understand outliers
# double check calculation
# test for sig dif between the means 

# scatters

df %>% filter(pct_change > -100, 
              pct_change < 100) %>%
  ggplot(aes(x=year, y=pct_change, color=party)) + 
  geom_jitter() + 
  # geom_smooth(method = "lm") + 
  scale_color_manual(values=c(D = "steelblue", R = "red3", I="darkgreen"))


# no relationship
df_yr %>% 
  filter(pct_change_an > -100,
         pct_change_an < 100) %>%
  ggplot(aes(x=n_years, y=pct_change_an, color=party)) + 
  geom_jitter() + 
  geom_smooth(method = "lm") + 
  scale_color_manual(values=c(D = "steelblue", R = "red3", I="darkgreen"))

### regressions
  