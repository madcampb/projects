library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(purrr)
library(tidyverse)
library(zoo)
library(fuzzyjoin)
library(stringr)
library(rpart.plot)	
library(rpart)


#############################################
##            import data                  ##
#############################################

## UCDP
# one-sided violence
# https://ucdp.uu.se/downloads/index.html#onesided
ucdp <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/data/ucdp-onesided-201.csv')

# disaggregated data (more recent)
# https://ucdp.uu.se/downloads/index.html#ged_global
ucdp_disagg <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/data/ged201-ucdp-disagg.csv')

## Peacekeeping operations data
# https://www.ipinst.org/providing-for-peacekeeping-database
unpk <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/data/peacekeeper_full_data_2020.csv')

# group by mission, location, year
# sum peacekeeper counts
unpk_gp <- unpk %>% 
  select(Date, Contributor, Contributor_ISO.3, Mission, Mission_Country, 
         Mission_Country_ISO.3, Mission_HQ, Mission_HQ_Longitude, Mission_HQ_Latitude, 
         Mission_Continent, Mission_Region, Mission_UN_Bloc, Experts_on_Mission, 
         Formed_Police_Units, Inidividual_Police, Troops, Observers, Total) %>%
  mutate(year = substr(Date, 1, 4)) %>%
  group_by(year, Mission, Mission_Country, Mission_Country_ISO.3, Mission_Continent, 
           Mission_Region, Mission_UN_Bloc) %>%
  summarise(experts = sum(Experts_on_Mission), 
            formed_police = sum(Formed_Police_Units), 
            individual_police = sum(Inidividual_Police), 
            troops = sum(Troops), 
            observers = sum(Observers), 
            total = sum(Total)
            ) %>%
  mutate_at('year',as.numeric)

mission <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/data/Mission-level-data.csv')
mission <- mission %>% mutate(year = substr(Date, 1, 4))


## Country variables
# kaggle country profiles -- UN Data 2018
# https://www.kaggle.com/sudalairajkumar/undata-country-profiles
country_profiles <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/data/country_profile_variables.csv')

colNames <- c('country', 'region', 'surface_area', 'pop_thousands', 'pop_density', 'sex_ratio', 'gdp', 'gdp_growth', 'gdp_per_capita', 'economy_ag', 'economy_ind', 'economy_serv', 'employ_ag', 'employ_ind', 'employ_serv', 'unemploy', 'labor_force_part', 'ag_prod', 'food_prod', 'trade_exp', 'trade_imp', 'trade_bal', 'bal_payments', 'pop_growth', 'urban_pop', 'urban_pop_growth', 'fertility_rate', 'life_exp', 'pop_age_dist', 'int_migrant_stock', 'refugees', 'inf_mortality', 'health_exp', 'physicians_per_cap', 'ed_gov_exp', 'ed_prim_enrol', 'ed_sec_enrol', 'ed_tert_enrol', 'parlimentary_seats_held_by_women', 'mobile_cell_sub', 'mobile_cell_sub2', 'internet', 'threatened_species', 'forested_area', 'co2_emissions', 'energy_prod', 'energy_supply_per_cap', 'pop_drinking', 'pop_sanitation', 'net_dev_assist')
country_profiles <- country_profiles %>% 
  set_names(colNames) 


#############################################
##            reconfigure                  ##
#############################################

ucdp

ucdp_un <- ucdp %>% full_join(unpk_gp, by = c("location" = "Mission_Country", 
                                                       "year" = "year"))

ucdp_un_fuzzy <- ucdp %>% fuzzy_full_join(unpk_gp, by = c("location" = "Mission_Country", 
                                                  "year" = "year"), 
                                       match_fun = str_detect)

# TODO: melt rows with multiple countries
# TODO: make sure country names align (delete parentheticals, etc)
# congo
# ivory coast
# syria
# venezuela?
# lists!! melt if ','

# left join

uc_un_temp <- ucdp %>% left_join(unpk_gp, by = c("location" = "Mission_Country", 
                                                 "year" = "year")) %>% 
  mutate(intervention = case_when(is.na(Mission) ~ 0,
                                  TRUE ~ 1)) %>% 
  left_join(country_profiles, by=c('location' = 'country'))


#############################################
##              modeling                   ##
#############################################

# limit data add dummy variables
# drop nulls
combined_lim <- uc_un_temp %>% 
  select(location, year, best_fatality_estimate, is_government_actor, intervention, region.y, gdp, gdp_per_capita, trade_bal, pop_growth, fertility_rate, internet, co2_emissions, net_dev_assist) %>%
  rename(un_region = region.y) %>% 
  mutate(s_asia = case_when(un_region == 'SouthernAsia' ~ 1, TRUE ~ 0), 
         w_asia = case_when(un_region == 'WesternAsia' ~ 1, TRUE ~ 0), 
         se_asia = case_when(un_region == 'South-easternAsia' ~ 1, TRUE ~ 0),
         e_asia = case_when(un_region == 'EasternAsia' ~ 1, TRUE ~ 0),
         c_asia = case_when(un_region == 'CentralAsia' ~ 1, TRUE ~ 0),

         s_europe = case_when(un_region == 'SouthernEurope' ~ 1, TRUE ~ 0), 
         w_europe = case_when(un_region == 'WesternEurope' ~ 1, TRUE ~ 0), 
         e_europe = case_when(un_region == 'EasternEurope' ~ 1, TRUE ~ 0),
         n_europe = case_when(un_region == 'NorthernEurope' ~ 1, TRUE ~ 0),
         
         n_africa = case_when(un_region == 'NorthernAfrica' ~ 1, TRUE ~ 0), 
         s_africa = case_when(un_region == 'SouthernAfrica' ~ 1, TRUE ~ 0),
         w_africa = case_when(un_region == 'WesternAfrica' ~ 1, TRUE ~ 0), 
         m_africa = case_when(un_region == 'MiddleAfrica' ~ 1, TRUE ~ 0), 
         e_africa = case_when(un_region == 'EasternAfrica' ~ 1, TRUE ~ 0), 
         
         polynesia = case_when(un_region == 'Polynesia' ~ 1, TRUE ~ 0), 
         oceania = case_when(un_region == 'Oceania' ~ 1, TRUE ~ 0), 
         melanesia = case_when(un_region == 'Melanesia' ~ 1, TRUE ~ 0), 
         micronesia = case_when(un_region == 'Micronesia' ~ 1, TRUE ~ 0), 
          
         c_america = case_when(un_region == 'CentralAmerica' ~ 1, TRUE ~ 0), 
         n_america = case_when(un_region == 'NorthernAmerica' ~ 1, TRUE ~ 0),
         caribbean = case_when(un_region == 'Caribbean' ~ 1, TRUE ~ 0), 
         s_america = case_when(un_region == 'SouthAmerica' ~ 1, TRUE ~ 0)
         ) %>%
  select(-c(un_region, location, year)) %>%
  drop_na() %>%
  mutate_all(as.numeric)

# split test and train
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(combined_lim, 0.8, train = TRUE)
data_test <- create_train_test(combined_lim, 0.8, train = FALSE)

# decision tree 
fit <- rpart(intervention~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)


#############################################
##              visualize                  ##
#############################################
