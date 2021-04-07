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
library(foreign)
library(gsubfn)
library(rpart.plot)	
library(rpart)
library(caret)
library(scales)
library(ISLR)
library(ROCR)


#############################################
##            import data                  ##
#############################################

####### Dependent variable 

## Peacekeeping operations data
# https://www.ipinst.org/providing-for-peacekeeping-database
unpk <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/data/peacekeeper_full_data_2020.csv')

# group by mission, location, year
# sum peacekeeper counts
unpk_lim <- unpk %>% 
  select(Date, Contributor, Contributor_ISO.3, Mission, Mission_Country, 
         Mission_Country_ISO.3, Mission_HQ, Mission_HQ_Longitude, Mission_HQ_Latitude, 
         Mission_Continent, Mission_Region, Mission_UN_Bloc, Experts_on_Mission, 
         Formed_Police_Units, Inidividual_Police, Troops, Observers, Total) 

unpk_gp <- unpk_lim %>%
  mutate(year = substr(Date, 1, 4)) %>%
  group_by(year, Mission_Country, Mission) %>%
  summarise(experts = sum(Experts_on_Mission), 
            formed_police = sum(Formed_Police_Units), 
            individual_police = sum(Inidividual_Police), 
            troops = sum(Troops), 
            observers = sum(Observers), 
            pk_total = sum(Total)
  ) %>%
  mutate_at('year',as.numeric) %>% 
  mutate(Mission_Country = case_when(Mission_Country == "South Sudan'" ~ 'South Sudan', 
                                     Mission_Country == 'Western Sahara' ~ 'Morocco',
                                     TRUE ~ Mission_Country))

unpk_lim <- unpk_gp %>%
  select(year, Mission_Country, Mission, pk_total) %>% 
  group_by(year, Mission_Country) %>% 
  summarise(pk_total = sum(pk_total))

unpk_countries <- sort(unique(unpk_gp$Mission_Country))

# mission <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/data/Mission-level-data.csv')
# mission <- mission %>% mutate(year = substr(Date, 1, 4))


####### Features

####### One sided violence 

## UCDP
# ethnic one-sided violence
# https://ucdp.uu.se/downloads/index.html#eosv

# eosv <- read.dta('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/violence_data/EOSV Dataset.dta')

## UCDP
# one-sided violence
# https://ucdp.uu.se/downloads/index.html#onesided
ucdp <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/data/ucdp-onesided-201.csv')
ucdp_raw <- ucdp %>%
  mutate(country_char = nchar(location), 
         country = strsplit(location,","),
         gwno = strsplit(gwno_location,","),
         n_countries = 1 + str_count(location, ","))

ucdp_lim <- ucdp_raw %>%
  filter(n_countries < 5) %>% # more than 4 countries experiencing violence is too distributed
  unnest(country) %>% 
  mutate(best_fatality_estimate = best_fatality_estimate / n_countries,
         country = str_trim(country), 
         country = case_when(
           country == 'Bosnia-Herzegovina' ~ 'Bosnia and Herzegovina',
           country == 'Cambodia (Kampuchea)' ~ 'Cambodia',
           country == 'DR Congo (Zaire)' ~ 'DR Congo',
           country == 'Ivory Coast' ~ 'Cote d Ivoire',
           country == 'Kingdom of eSwatini (Swaziland)' ~ 'Swaziland',
           country == 'Madagascar (Malagasy)' ~ 'Madagascar',
           country == 'Myanmar (Burma)' ~ 'Myanmar',
           country == 'Russia (Soviet Union)' ~ 'Russia',
           country == 'Serbia (Yugoslavia)' ~ 'Serbia',
           country == 'Syria' ~ 'Syrian Arab Republic',
           country == 'Yemen (North Yemen)' ~ 'Yemen',
           country == 'Zimbabwe (Rhodesia)' ~ 'Zimbabwe',
           country == 'Macedonia, FYR' ~ 'Macedonia',
           country == 'United States of America' ~ 'United States',
            TRUE ~ country), 
         region = case_when(
           country == 'Afghanistan' ~ '3', 
           country == 'Russia' ~ '1', 
           country == 'Pakistan' ~ '3', 
           country == 'United Kingdom' ~ '1', 
           TRUE ~ region), 
         region_name = case_when(
           region == '1' ~ 'Europe', 
           region == '2' ~ 'Middle East', 
           region == '3' ~ 'Asia', 
           region == '4' ~ 'Africa', 
           region == '5' ~ 'Americas'
         )) %>%
  select(year, best_fatality_estimate, country, region, region_name)

ucdp_countries <- sort(unique(ucdp_lim$country))

# disaggregated data (more recent)
# https://ucdp.uu.se/downloads/index.html#ged_global

ucdp_disagg <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/data/ged201-ucdp-disagg.csv')

ucdp_ged <- ucdp_disagg %>%
  mutate(violence_type = case_when(
      type_of_violence == '1' ~ 'state',
      type_of_violence == '2' ~ 'non_state',
      type_of_violence == '3' ~ 'one_sided')) %>%
  group_by(year, country, region, violence_type) %>% # type_of_violence, conflict_new_id
  summarise(best = sum(best)) %>%
  pivot_wider(names_from = violence_type, values_from = best, values_fill = 0) %>%
  mutate(best_fatality_estimate = state + non_state + one_sided, 
         type_of_violence = case_when(
           (state > non_state) & (state > one_sided) ~ 'state', 
           (non_state > state) & (non_state > one_sided) ~ 'non-state',
           (one_sided > non_state) & (one_sided > state) ~ 'one-sided', 
           one_sided == state ~ 'one-sided'
         ),
         country = str_trim(country), 
         country = case_when(
           country == 'Bosnia-Herzegovina' ~ 'Bosnia and Herzegovina',
           country == 'Cambodia (Kampuchea)' ~ 'Cambodia',
           country == 'DR Congo (Zaire)' ~ 'DR Congo',
           country == 'Ivory Coast' ~ 'Cote d Ivoire',
           country == 'Kingdom of eSwatini (Swaziland)' ~ 'Swaziland',
           country == 'Madagascar (Malagasy)' ~ 'Madagascar',
           country == 'Myanmar (Burma)' ~ 'Myanmar',
           country == 'Russia (Soviet Union)' ~ 'Russia',
           country == 'Serbia (Yugoslavia)' ~ 'Serbia',
           country == 'Syria' ~ 'Syrian Arab Republic',
           country == 'Yemen (North Yemen)' ~ 'Yemen',
           country == 'Zimbabwe (Rhodesia)' ~ 'Zimbabwe',
           country == 'Macedonia, FYR' ~ 'Macedonia',
           country == 'United States of America' ~ 'United States',
           TRUE ~ country)) %>%
  select(-c(one_sided, state, non_state))
  # group_by(country, region, type_of_violence, conflict_new_id) %>%
  # summarise(min_year = min(year), 
  #           max_year = max(year), 
  #           best_fatality_estimate = sum(best_fatality_estimate)) %>%
  # mutate(violence_type = case_when(
  #   type_of_violence == '1' ~ 'state', 
  #   type_of_violence == '2' ~ 'non-state',
  #   type_of_violence == '3' ~ 'one-sided'
  # ))


####### Refugees
## https://www.unhcr.org/refugee-statistics/download/?url=bWoY55

refugee <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/feature_data/unhcr_refugee_population.csv')

r_lim <- refugee %>%
  mutate(refugees = Refugees.under.UNHCR.s.mandate, 
         Country.of.origin = case_when(
           Country.of.origin == 'Central African Rep.' ~ 'Central African Republic',
           Country.of.origin == "Cote d'Ivoire" ~ 'Cote d Ivoire',
           Country.of.origin == 'Dem. Rep. of the Congo' ~ 'DR Congo',
           Country.of.origin == 'Dominican Rep.' ~ 'Dominican Republic',
           Country.of.origin == 'Iran (Islamic Rep. of)' ~ 'Iran',
           Country.of.origin == "Lao People's Dem. Rep." ~ 'Laos',
           Country.of.origin == 'Syrian Arab Rep.' ~ 'Syrian Arab Republic',
           Country.of.origin == 'Russian Federation' ~ 'Russia',
           Country.of.origin == 'United Kingdom of Great Britain and Northern Ireland' ~ 'United Kingdom',
           Country.of.origin == 'United Rep. of Tanzania' ~ 'Tanzania',
           Country.of.origin == 'Venezuela (Bolivarian Republic of)' ~ 'Venezuela',
           Country.of.origin == 'North Macedonia' ~ 'Macedonia',
           Country.of.origin == 'United States of America' ~ 'United States',
           TRUE ~ Country.of.origin), 
         Country.of.origin = case_when(
           grepl("Serbia", Country.of.origin) ~ 'Serbia',
           TRUE ~ Country.of.origin)) %>%
  select(Country.of.origin, Year, refugees)

####### Military might  
## https://data.worldbank.org/indicator/MS.MIL.TOTL.TF.ZS?view=map

#military <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/feature_data/API_MS.MIL.XPND.GD.ZS_DS2_en_csv_v2_2055780/API_MS.MIL.XPND.GD.ZS_DS2_en_csv_v2_2055780.csv')

military <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/feature_data/API_MS.MIL.XPND.CD_DS2_en_csv_v2_2055783/API_MS.MIL.XPND.CD_DS2_en_csv_v2_2055783.csv')

military <- military %>% 
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "year", 
    values_to = "military_exp",
    values_drop_na = TRUE
  ) %>%
  mutate(year = as.numeric(sub('X', '', year)), 
         Country.Name = case_when(
           Country.Name == 'Congo, Rep.' ~ 'Congo',
           Country.Name == "Cote d'Ivoire" ~ 'Cote d Ivoire',
           Country.Name == 'Congo, Dem. Rep.' ~ 'DR Congo',
           Country.Name == 'Egypt, Arab Rep.' ~ 'Egypt',
           Country.Name == 'Gambia, The' ~ 'Gambia',
           Country.Name == 'Iran, Islamic Rep.' ~ 'Iran',
           Country.Name == "Lao PDR" ~ 'Laos',
           Country.Name == 'Russian Federation' ~ 'Russia',
           Country.Name == 'Venezuela, RB' ~ 'Venezuela',
           Country.Name == 'Yemen, Rep.' ~ 'Yemen',
           Country.Name == 'North Macedonia' ~ 'Macedonia',
           Country.Name == 'Kyrgyz Republic' ~ 'Kyrgyzstan',
           TRUE ~ Country.Name))

m_lim <- military %>%
  select(Country.Name, year, military_exp)

####### Sunk cost  
## https://data.worldbank.org/indicator/DT.ODA.ALLD.KD?view=map&year=1960

aid <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/feature_data/API_DT.ODA.ALLD.KD_DS2_en_csv_v2_2062608/API_DT.ODA.ALLD.KD_DS2_en_csv_v2_2062608.csv')

aid <- aid %>% 
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "year", 
    values_to = "aid_dollars",
    values_drop_na = TRUE
  ) %>%
  mutate(year = as.numeric(sub('X', '', year)), 
         Country.Name = case_when(
           Country.Name == 'Congo, Rep.' ~ 'Congo',
           Country.Name == "Cote d'Ivoire" ~ 'Cote d Ivoire',
           Country.Name == 'Congo, Dem. Rep.' ~ 'DR Congo',
           Country.Name == 'Egypt, Arab Rep.' ~ 'Egypt',
           Country.Name == 'Gambia, The' ~ 'Gambia',
           Country.Name == 'Iran, Islamic Rep.' ~ 'Iran',
           Country.Name == "Lao PDR" ~ 'Laos',
           Country.Name == 'Russian Federation' ~ 'Russia',
           Country.Name == 'Venezuela, RB' ~ 'Venezuela',
           Country.Name == 'Yemen, Rep.' ~ 'Yemen',
           Country.Name == 'North Macedonia' ~ 'Macedonia',
           Country.Name == 'Kyrgyz Republic' ~ 'Kyrgyzstan',
           TRUE ~ Country.Name)) %>%
  arrange(Country.Name, year) %>%
  group_by(Country.Name) %>%
  mutate(
    aid_dollars = case_when(aid_dollars <= 0 ~ 1, TRUE ~ aid_dollars),
    aid_lag_2yrsum = lag(aid_dollars, 2) + lag(aid_dollars, 1)) 

a_lim <- aid %>%
  select(Country.Name, year, aid_dollars, aid_lag_2yrsum)




### percent pop muslim 
# just one value per country (not annual)
# PEW -- https://www.pewforum.org/chart/interactive-data-table-world-muslim-population-by-country/

muslim_pop <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/data/muslim_pop.csv')

m_pop <- muslim_pop %>% 
  mutate(p_muslim = as.numeric(Percentage.of.Population.that.is.Muslim)) %>% 
  select(c(Country, p_muslim))


### GDP (per capita)
# https://data.worldbank.org/indicator/NY.GDP.PCAP.CD

gdp_pc <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2163510/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2163510.csv')

gdp_pc <- gdp_pc %>% 
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "year", 
    values_to = "gdp_pc",
    values_drop_na = TRUE
  ) %>%
  mutate(year = as.numeric(sub('X', '', year)), 
         Country.Name = case_when(
           Country.Name == 'Congo, Rep.' ~ 'Congo',
           Country.Name == "Cote d'Ivoire" ~ 'Cote d Ivoire',
           Country.Name == 'Congo, Dem. Rep.' ~ 'DR Congo',
           Country.Name == 'Egypt, Arab Rep.' ~ 'Egypt',
           Country.Name == 'Gambia, The' ~ 'Gambia',
           Country.Name == 'Iran, Islamic Rep.' ~ 'Iran',
           Country.Name == "Lao PDR" ~ 'Laos',
           Country.Name == 'Russian Federation' ~ 'Russia',
           Country.Name == 'Venezuela, RB' ~ 'Venezuela',
           Country.Name == 'Yemen, Rep.' ~ 'Yemen',
           Country.Name == 'North Macedonia' ~ 'Macedonia',
           Country.Name == 'Kyrgyz Republic' ~ 'Kyrgyzstan',
           TRUE ~ Country.Name)) 

gdp_lim <- gdp_pc %>%
  select(c(Country.Name, year, gdp_pc))

### Exports
# https://data.worldbank.org/indicator/NE.EXP.GNFS.CD

exports <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/peacekeeping/API_NE.EXP.GNFS.CD_DS2_en_csv_v2_2165684/API_NE.EXP.GNFS.CD_DS2_en_csv_v2_2165684.csv')

exports <- exports %>% 
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "year", 
    values_to = "exports",
    values_drop_na = TRUE
  ) %>%
  mutate(year = as.numeric(sub('X', '', year)), 
         Country.Name = case_when(
           Country.Name == 'Congo, Rep.' ~ 'Congo',
           Country.Name == "Cote d'Ivoire" ~ 'Cote d Ivoire',
           Country.Name == 'Congo, Dem. Rep.' ~ 'DR Congo',
           Country.Name == 'Egypt, Arab Rep.' ~ 'Egypt',
           Country.Name == 'Gambia, The' ~ 'Gambia',
           Country.Name == 'Iran, Islamic Rep.' ~ 'Iran',
           Country.Name == "Lao PDR" ~ 'Laos',
           Country.Name == 'Russian Federation' ~ 'Russia',
           Country.Name == 'Venezuela, RB' ~ 'Venezuela',
           Country.Name == 'Yemen, Rep.' ~ 'Yemen',
           Country.Name == 'North Macedonia' ~ 'Macedonia',
           Country.Name == 'Kyrgyz Republic' ~ 'Kyrgyzstan',
           TRUE ~ Country.Name)) 

exp_lim <- exports %>%
  select(c(Country.Name, year, exports))




#############################################
##               big join                  ##
#############################################

# left join
# do I need a lag on year?
# TODO
df_feats <- ucdp_ged %>% full_join(unpk_lim, by = c("country" = "Mission_Country",
                                                    "year" = "year")) %>%
  filter(year >= 1990, 
         year <= 2019) %>%
  mutate(intervention = case_when(pk_total >= 1 ~ 1,
                                  TRUE ~ 0)) %>% 
  # left_join(frac_bind2, by=c('country' = 'Country', 
  #                      "year" = "Year")) %>%
  left_join(r_lim, by=c('country' = 'Country.of.origin', 
                        "year" = "Year")) %>%
  left_join(m_lim, by=c('country' = 'Country.Name', 
                        "year" = "year")) %>%
  left_join(a_lim, by=c('country' = 'Country.Name', 
                        "year" = "year")) %>%
  left_join(gdp_lim, by=c('country' = 'Country.Name', 
                        "year" = "year")) %>%
  left_join(exp_lim, by=c('country' = 'Country.Name', 
                          "year" = "year")) %>%
  left_join(m_pop, by=c('country' = 'Country')) %>%
  arrange(country, year) %>% 
  group_by(country) %>% 
  fill(refugees, .direction = "downup") %>%
  fill(military_exp, .direction = "downup") %>%
  fill(aid_dollars, .direction = "downup") %>%
  fill(gdp_pc, .direction = "downup") %>%
  fill(exports, .direction = "downup") %>%
  mutate(best_fatality_estimate = replace_na(best_fatality_estimate, 0),
         pk_total = replace_na(pk_total, 0), 
         refugees = replace_na(refugees, 1), 
         military_exp = replace_na(military_exp, 1),
         aid_dollars = replace_na(aid_dollars, 1),
         gdp_pc = case_when(country == 'Moldova' & year == 1992 ~ 593.8, TRUE ~ gdp_pc),
         aid_lag_2yrsum = case_when(aid_dollars == 0 ~ 1 , TRUE ~ aid_lag_2yrsum),
         aid_lag_2yrsum = case_when(is.na(aid_lag_2yrsum) ~ aid_dollars*2, TRUE ~ aid_lag_2yrsum), #TEMP FIX
         region = case_when(country == 'Pakistan' ~ 'Middle East', country == 'Afghanistan' ~ 'Middle East', TRUE ~ region),
         state = case_when(type_of_violence == 'state' ~ 1, TRUE ~ 0), 
         one_sided = case_when(type_of_violence == 'one-sided' ~ 1, TRUE ~ 0), 
         non_state = case_when(type_of_violence == 'non-state' ~ 1, TRUE ~ 0))

# Western Sahara *not in there
ws <- c('Western Sahara', 'Africa')
# Cyprus *not in there
cy <- c('Cyprus', 'Europe')
# Timor-Leste *not in there
tl <- c('Timor-Leste', 'Asia')

country_region <- df_feats %>% 
  ungroup() %>% 
  select(country, region) %>% 
  unique() %>%
  drop_na() %>%
  rename(region_full = region)

country_region <- rbind(country_region, ws, cy, tl)

df_feats <- df_feats %>% 
  full_join(country_region, by=c('country' = 'country')) %>% 
  mutate(region = region_full, 
         asia = case_when(region == 'Asia' ~ 1, TRUE ~ 0), 
         africa = case_when(region == 'Africa' ~ 1, TRUE ~ 0), 
         europe = case_when(region == 'Europe' ~ 1, TRUE ~ 0),
         middle_east = case_when(region == 'Middle East' ~ 1, TRUE ~ 0),
         americas = case_when(region == 'Americas' ~ 1, TRUE ~ 0)) %>% 
  drop_na(year)

#############################################
##          exploratory plots              ##
#############################################


# peacekeeper distribution
ggplot(df_feats, aes(log(pk_total))) + geom_density(fill="blue")
# at least 100 peacekeepers to count as intervention
# some # of fatalities? 

ggplot(df_feats, aes(exports)) + geom_density(fill="blue")
ggplot(df_feats, aes(log(gdp_pc))) + geom_density(fill="blue")
ggplot(df_feats, aes(log(best_fatality_estimate))) + geom_density(fill="blue")
ggplot(df_feats, aes(log(military_exp))) + geom_density(fill="blue")
ggplot(df_feats, aes(log(aid_lag_2yrsum))) + geom_density(fill="blue")
ggplot(df_feats, aes(log(refugees))) + geom_density(fill="blue")


# lineplot fatalities and refugees over time
df_feats %>%
  group_by(year) %>%
  summarise(fatalities = sum(best_fatality_estimate), 
            refugees = sum(refugees)) %>%
  ggplot(aes(x=year, y=fatalities)) + 
  geom_line() + 
  geom_line(aes(x=year, y=refugees/30), color='steelblue') + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Fatalities",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*30, name="Refugees")
  ) + 
  theme(axis.line.y.right = element_line(color = "steelblue"), 
        axis.ticks.y.right = element_line(color = "steelblue"),
        axis.text.y.right = element_text(color = "steelblue"), 
        axis.title.y.right = element_text(color = "steelblue")
  )



## STACKED BAR

df_feats %>%
  filter(!(country == 'Rwanda' & year==1994)) %>%
  group_by(year, region) %>%
  summarise(fatalities = sum(best_fatality_estimate), 
            refugees = sum(refugees)) %>%
  ggplot(aes(x=year, y=fatalities, fill=region)) + 
  geom_area() + 
  theme_bw()

df_feats %>%
  #filter(!(country == 'Rwanda' & year==1994)) %>%
  group_by(year, region) %>%
  summarise(peacekeepers = sum(pk_total), 
            refugees = sum(refugees)) %>%
  ggplot(aes(x=year, y=peacekeepers, fill=region)) + 
  geom_area() + 
  theme_bw()
  # geom_bar(position="stack", stat="identity") 

df_feats %>%
  filter(!(country == 'Rwanda' & year==1994)) %>%
  group_by(year, region) %>%
  summarise(peacekeepers = sum(pk_total), 
            refugees = sum(refugees), 
            fatalities = sum(best_fatality_estimate)) %>%
  ggplot(aes(x=year, y=peacekeepers, fill=region)) + 
  geom_area() + 
  geom_area(aes(x=year, y=fatalities*-1, fill=region)) + 
  geom_hline(yintercept=0, linetype="solid", color = "black") + 
  theme_bw()


# library("gridExtra")
library(ggpubr)

pk_plot <- df_feats %>%
  #filter(!(country == 'Rwanda' & year==1994)) %>%
  group_by(year, region) %>%
  summarise(peacekeepers = sum(pk_total), 
            refugees = sum(refugees)) %>%
  ggplot(aes(x=year, y=peacekeepers, fill=region)) + 
  geom_area() + 
  theme_bw() + 
  scale_fill_manual(values=c("steelblue", "aquamarine4", "gold2", "salmon2", "midnightblue")) +  # "#56B4E9"))
  labs(
    title = "War and Peace",
    subtitle = "United Nations Peacekeeping, 1990-2019",
    y = "Number of peacekeepers",
    x ="", 
    fill = ''
    #caption = "Data source: NOAA; Center for Climate and Energy Solutions"
    ) +
  ggplot2::annotate("text", x = 2013, y = 700000, label = "Africa", color='white', size=3)  + 
  ggplot2::annotate("text", x = 1994, y = 200000, label = "Europe", color='white', size=3)  + 
  ggplot2::annotate("text", x = 2001, y = 240000, label = "Asia", color='white', size=3)  + 
  ggplot2::annotate("text", x = 2010, y = 270000, label = "Americas", color='white', size=3)  + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position = "none"
  )

fat_plot <- df_feats %>%
  filter(!(country == 'Rwanda' & year==1994)) %>%
  group_by(year, region) %>%
  summarise(fatalities = sum(best_fatality_estimate)*-1, 
            refugees = sum(refugees)) %>%
  ggplot(aes(x=year, y=fatalities, fill=region)) +
  geom_area() + 
  theme_bw() + 
  scale_fill_manual(values=c("steelblue", "aquamarine4", "gold2", "salmon2", "midnightblue")) +  # "#56B4E9"))
  labs(
    y = "Lives lost to one-sided violence",
    x ="", 
    #fill = '',
    caption = "Data source: International Peace Institute; Uppsala Conflict Data Program") + 
  ggplot2::annotate("text", x = 2015, y = -50000, label = "Middle East", color='white', size=3)  + 
  ggplot2::annotate("text", x = 1997, y = -125000, label = "* 1994 Rwandan genocide removed\n 5 hundred thousand lives lost", color='steelblue', size=3)  + 
  theme(legend.position = "none")

figure <- ggarrange(pk_plot, fat_plot, # + font("x.text", size = 10),
                    ncol = 1, nrow = 2)
figure

df_feats %>%
  #filter(!(country == 'Rwanda' & year==1994)) %>%
  group_by(year, region) %>%
  summarise(peacekeepers = sum(pk_total),
            pk_missions = sum(intervention),
            refugees = sum(refugees)) %>%
  ggplot(aes(x=year, y=refugees, fill=region)) + 
  geom_bar(position="stack", stat="identity") 

df_feats %>%
  #filter(!(country == 'Rwanda' & year==1994)) %>%
  group_by(year, region) %>%
  summarise(peacekeepers = sum(pk_total),
            pk_missions = sum(intervention),
            refugees = sum(refugees)) %>%
  ggplot(aes(x=year, y=pk_missions, fill=region)) + 
  geom_bar(position="stack", stat="identity") 


df_feats %>%
  #filter(!(country == 'Rwanda' & year==1994)) %>%
  group_by(year, region) %>%
  summarise(peacekeepers = sum(pk_total), 
            refugees = sum(refugees), 
            sunk_cost = sum(aid_lag_2yrsum)) %>%
  ggplot(aes(x=year, y=sunk_cost, fill=region)) + 
  geom_bar(position="stack", stat="identity") 

# lineplot fatalities by region
df_feats %>%
  group_by(year, region) %>%
  summarise(fatalities = sum(best_fatality_estimate), 
            refugees = sum(refugees)) %>%
  ggplot(aes(x=year, y=fatalities, color=region)) + 
  geom_line() 


# lineplot refugees by region
df_feats %>%
  group_by(year, region) %>%
  summarise(fatalities = sum(best_fatality_estimate), 
            refugees = sum(refugees)) %>%
  ggplot(aes(x=year, y=refugees, color=region)) + 
  geom_line() 


# scatter log(fatalities) and log(pk_total)
# lm for each type of violence
p1 <- df_feats %>%
  filter(intervention == 1, 
         pk_total > 0, 
         refugees > 0) %>%
  ggplot(aes(x=log(refugees) , y=log(pk_total))) + 
  geom_point() + 
  geom_smooth(method = "lm")

p1

# scatter log(refugees) and log(aid)
p3 <- df_feats %>%
  ggplot(aes(x=log(refugees) , y=log(aid_lag_2yrsum), color=intervention)) + 
  geom_point() 

p3

# scatter log(military_exp) and log(aid)
p4 <- df_feats %>%
  ggplot(aes(x=log(military_exp) , y=log(aid_lag_2yrsum), color=intervention)) + 
  geom_point() 

p4

# scatter for each region
# refugees by peacekeepers
# color by fatality
p2 <- df_feats %>%
  filter(intervention == 1, 
         pk_total > 100, 
         refugees > 0) %>%
  ggplot(aes(x=log(refugees), y=log(pk_total), color=log(best_fatality_estimate))) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~ region, ncol=5, scales="free_x")  

p2

# scatter for each region
# fatality by peacekeepers
# color by refugees
p3 <- df_feats %>%
  filter(intervention == 1, 
         pk_total > 100) %>%
  ggplot(aes(x=log(best_fatality_estimate), y=log(pk_total), color=log(refugees))) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~ region, ncol=5, scales="free_x")  

p3

#############################################
##                modeling                 ##
#############################################

## classification = predict whether there will BE a peacekeeping operation (at least 1 peacekeeper)

# keep feats
class_df <- df_feats %>%
  ungroup() %>%
  select(-c(country, type_of_violence, region, Mission, pk_total, aid_dollars, year)) %>% # year??
  mutate_all(as.numeric) %>%
  filter(aid_lag_2yrsum >= 0) %>%
  mutate(aid_lag_2yrsum = case_when(aid_lag_2yrsum == 0 ~ 1, TRUE ~ aid_lag_2yrsum),
         log_aid = log(aid_lag_2yrsum), 
         log_ref = log(refugees), 
         log_mil = log(military_exp), 
         log_fat = log(best_fatality_estimate)) %>%
  select(-c(aid_lag_2yrsum, refugees, best_fatality_estimate, military_exp))

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


temp <- class_df %>% select(-c(americas, asia, africa, europe, middle_east))
data_train <- create_train_test(temp, 0.8, train = TRUE)
data_test <- create_train_test(temp, 0.8, train = FALSE)

# decision tree 
fit <- rpart(intervention~., data = data_train, method = 'class', maxdepth=3) #, control=rpart.control(minsplit=30, cp=0.001) )
rpart.plot(fit, extra = 106)

printcp(fit)	#display cp table
plotcp(fit)	#plot cross-validation results
# summary(fit)	#detailed results including surrogate splits

# predict
t_pred = predict(fit, data_test, type="class")

# confusion matrix
confMat <- table(data_test$intervention, t_pred)
accuracy <- sum(diag(confMat))/sum(confMat)
print(accuracy)

# variable importance
varImp(fit)

# accuracy =  0.7493
# Overall
# log_aid   38.68
# log_fat   14.90
# log_mil   10.63
# log_ref   35.01


## TRY A LOGIT

log_df <- df_feats %>%
  ungroup() %>%
  select(-c(region_full, state, non_state, one_sided, country, type_of_violence, region, aid_dollars, year)) %>% # year??
  mutate_all(as.numeric) %>%
  filter(aid_lag_2yrsum >= 0, 
         best_fatality_estimate > 0) %>%
  mutate(aid_lag_2yrsum = case_when(aid_lag_2yrsum == 0 ~ 1, TRUE ~ aid_lag_2yrsum),
         refugees = case_when(refugees == 0 ~ 1, TRUE ~ refugees),
         log_exp = log(exports),
         log_gdp = log(gdp_pc),
         log_aid = log(aid_lag_2yrsum), 
         log_ref = log(refugees), 
         log_mil = log(military_exp), 
         log_fat = log(best_fatality_estimate)) %>%
  select(-c(aid_lag_2yrsum, exports, log_exp, gdp_pc, p_muslim, refugees, military_exp)) #, americas, asia, middle_east, africa, europe)) #refugees, best_fatality_estimate, 

all <- log_df %>% select(-c(asia, africa, middle_east, americas, europe, pk_total, best_fatality_estimate))
log_train <- create_train_test(all, 0.8, train = TRUE)
log_test <- create_train_test(all, 0.8, train = FALSE)

glm_model <- glm(intervention ~.,family=binomial(link='logit'),data=log_train)

summary(glm_model)

# Coefficients:
#   Estimate Std. Error z value      Pr(>|z|)    
#   (Intercept)  -8.8588     1.3746   -6.44 0.00000000012 ***
#   log_aid       0.2168     0.0641    3.38       0.00072 ***
#   log_ref       0.2128     0.0336    6.34 0.00000000023 ***
#   log_mil       0.0182     0.0335    0.54       0.58689    
#   log_fat       0.0963     0.0351    2.74       0.00612 ** 

anova(glm_model, test="Chisq")

fitted.results <- predict(glm_model,newdata=log_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != log_test$intervention)
print(paste('Accuracy',1-misClasificError))
# Accuracy 0.879365079365079 

p <- predict(glm_model, newdata=log_test, type="response")
pr <- prediction(p, log_test$intervention)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# AUC 0.8694

or_all <- exp(cbind(OR = coef(glm_model), confint(glm_model)))



## predicted vs actual 
pred_res <- predict(glm_model, newdata=all, type="response")
log_all <- log_df %>% add_column(pred = pred_res)

log_all %>%
  filter(pk_total > 0) %>%
  mutate(region = case_when(americas == 1 ~ 'Americas', 
                            asia == 1 ~ 'Asia', 
                            africa == 1 ~ 'Africa', 
                            europe == 1 ~ 'Europe', 
                            middle_east == 1 ~ 'Middle East')) %>%
  ggplot(aes(x=pred, y=log(pk_total), color=region, size=best_fatality_estimate)) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "aquamarine4", "gold2", "salmon2", "midnightblue")) + 
  theme_bw() + 
  labs(title="",
       x ="Modelled probability that a country will have at least one peacekeeper in a given year", 
       y = "Number of peacekeepers (log scale)", 
       color = "Region") + 
  guides(size = FALSE)

coef <- summary(glm_model)$coefficients[, 1]
std_er <- summary(glm_model)$coefficients[, 2]

all_coef <- data.frame(coef, std_er)
all_coef <- cbind(feature = rownames(all_coef), all_coef)
all_coef <- all_coef %>% 
  mutate(model = 'all')

##########
# africa #
##########
africa <- log_df %>% filter(africa == 1) %>% select(-c(asia, africa, middle_east, americas, europe, pk_total, best_fatality_estimate))
log_train <- create_train_test(africa, 0.8, train = TRUE)
log_test <- create_train_test(africa, 0.8, train = FALSE)

glm_model <- glm(intervention ~.,family=binomial(link='logit'),data=log_train)

summary(glm_model)

# Coefficients:
#   Estimate Std. Error z value         Pr(>|z|)    
#   (Intercept) -14.9105     3.1092   -4.80 0.00000162171495 ***
#   log_aid       0.0502     0.1288    0.39            0.697    
#   log_ref       0.7172     0.0982    7.30 0.00000000000029 ***
#   log_mil       0.2288     0.0970    2.36            0.018 *  
#   log_fat       0.0258     0.0599    0.43            0.666    

anova(glm_model, test="Chisq")

fitted.results <- predict(glm_model,newdata=log_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != log_test$intervention)
print(paste('Accuracy',1-misClasificError))
# Accuracy 0.863636363636364

p <- predict(glm_model, newdata=log_test, type="response")
pr <- prediction(p, log_test$intervention)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# AUC 0.9121

or_af <- exp(cbind(OR = coef(glm_model), confint(glm_model)))


## predicted vs actual 
pred_res <- predict(glm_model, newdata=africa, type="response")
log_af <- log_df %>% filter(africa == 1) %>% add_column(pred = pred_res)

coef <- summary(glm_model)$coefficients[, 1]
std_er <- summary(glm_model)$coefficients[, 2]

africa_coef <- data.frame(coef, std_er)
africa_coef <- cbind(feature = rownames(africa_coef), africa_coef)
africa_coef <- africa_coef %>% 
  mutate(model = 'africa')


########
# asia #
########
asia <- log_df %>% filter(asia == 1) %>% select(-c(asia, africa, middle_east, americas, europe, pk_total, best_fatality_estimate))
log_train <- create_train_test(asia, 0.7, train = TRUE)
log_test <- create_train_test(asia, 0.3, train = FALSE)

glm_model <- glm(intervention ~.,family=binomial(link='logit'),data=log_train)

summary(glm_model)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
#   (Intercept)   0.9762     3.5491    0.28    0.783  
#   log_aid      -0.1021     0.1247   -0.82    0.413  
#   log_ref       0.0736     0.1807    0.41    0.684  
#   log_mil      -0.2008     0.1009   -1.99    0.047 *
#   log_fat       0.1675     0.2514    0.67    0.505  

anova(glm_model, test="Chisq")

fitted.results <- predict(glm_model,newdata=log_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != log_test$intervention)
print(paste('Accuracy',1-misClasificError))
# Accuracy 0.955882352941177

p <- predict(glm_model, newdata=log_test, type="response")
pr <- prediction(p, log_test$intervention)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# AUC 0.8205

or_as <- exp(cbind(OR = coef(glm_model), confint(glm_model)))


## predicted vs actual 
pred_res <- predict(glm_model, newdata=asia, type="response")
log_as <- log_df %>% filter(asia == 1) %>% add_column(pred = pred_res)


coef <- summary(glm_model)$coefficients[, 1]
std_er <- summary(glm_model)$coefficients[, 2]

asia_coef <- data.frame(coef, std_er)
asia_coef <- cbind(feature = rownames(asia_coef), asia_coef)
asia_coef <- asia_coef %>% 
  mutate(model = 'asia')


###############
# middle east #
###############
middle_east <- log_df %>% filter(middle_east == 1) %>% select(-c(asia, africa, middle_east, americas, europe, pk_total, best_fatality_estimate))
log_train <- create_train_test(middle_east, 0.8, train = TRUE)
log_test <- create_train_test(middle_east, 0.2, train = FALSE)

glm_model <- glm(intervention ~.,family=binomial(link='logit'),data=log_train)

summary(glm_model)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -10.7108     4.1909   -2.56    0.011 *  
#   log_aid       0.4828     0.1151    4.19 0.000027 ***
#   log_ref      -0.0854     0.0706   -1.21    0.226    
#   log_mil       0.0382     0.1277    0.30    0.765    
#   log_fat       0.2261     0.0785    2.88    0.004 ** 

anova(glm_model, test="Chisq")

fitted.results <- predict(glm_model,newdata=log_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != log_test$intervention)
print(paste('Accuracy',1-misClasificError))
# Accuracy 0.713636363636364

p <- predict(glm_model, newdata=log_test, type="response")
pr <- prediction(p, log_test$intervention)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# AUC 0.7203

or_me <- exp(cbind(OR = coef(glm_model), confint(glm_model)))


## predicted vs actual 
pred_res <- predict(glm_model, newdata=middle_east, type="response")
log_me <- log_df %>% filter(middle_east == 1) %>% add_column(pred = pred_res)



coef <- summary(glm_model)$coefficients[, 1]
std_er <- summary(glm_model)$coefficients[, 2]

me_coef <- data.frame(coef, std_er)
me_coef <- cbind(feature = rownames(me_coef), me_coef)
me_coef <- me_coef %>% 
  mutate(model = 'middle_east')

##########
# europe #
##########
europe <- log_df %>% filter(europe == 1) %>% select(-c(asia, africa, middle_east, americas, europe, pk_total, best_fatality_estimate))
log_train <- create_train_test(europe, 0.8, train = TRUE)
log_test <- create_train_test(europe, 0.2, train = FALSE)

glm_model <- glm(intervention ~.,family=binomial(link='logit'),data=log_train)

summary(glm_model)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)  -0.5363     2.0524   -0.26     0.79
# log_aid       0.0110     0.0718    0.15     0.88
# log_ref       0.0232     0.1506    0.15     0.88
# log_mil      -0.1433     0.0874   -1.64     0.10
# log_fat       0.1750     0.1430    1.22     0.22

anova(glm_model, test="Chisq")

fitted.results <- predict(glm_model,newdata=log_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != log_test$intervention)
print(paste('Accuracy',1-misClasificError))
# Accuracy 0.900826446280992

p <- predict(glm_model, newdata=log_test, type="response")
pr <- prediction(p, log_test$intervention)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# AUC 0.862

or_eu <- exp(cbind(OR = coef(glm_model), confint(glm_model)))


## predicted vs actual 
pred_res <- predict(glm_model, newdata=europe, type="response")
log_eu <- log_df %>% filter(europe == 1) %>% add_column(pred = pred_res)



coef <- summary(glm_model)$coefficients[, 1]
std_er <- summary(glm_model)$coefficients[, 2]

euro_coef <- data.frame(coef, std_er)
euro_coef <- cbind(feature = rownames(euro_coef), euro_coef)
euro_coef <- euro_coef %>% 
  mutate(model = 'europe')


############
# americas #
############
americas <- log_df %>% filter(americas == 1) %>% select(-c(asia, africa, middle_east, americas, europe, pk_total, best_fatality_estimate))
log_train <- create_train_test(americas, 0.8, train = TRUE)
log_test <- create_train_test(americas, 0.2, train = FALSE)

glm_model <- glm(intervention ~.,family=binomial(link='logit'),data=log_train)

summary(glm_model)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)  -70.427     30.666   -2.30   0.0216 * 
#   log_aid        4.045      1.770    2.29   0.0223 * 
#   log_ref        2.087      0.698    2.99   0.0028 **
#   log_mil       -1.712      0.570   -3.00   0.0027 **
#   log_fat       -0.841      0.309   -2.73   0.0064 **

anova(glm_model, test="Chisq")

fitted.results <- predict(glm_model,newdata=log_test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != log_test$intervention)
print(paste('Accuracy',1-misClasificError))
# Accuracy 0.936708860759494"

p <- predict(glm_model, newdata=log_test, type="response")
pr <- prediction(p, log_test$intervention)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# AUC 0.9683

or_am <- exp(cbind(OR = coef(glm_model), confint(glm_model)))


## predicted vs actual 
pred_res <- predict(glm_model, newdata=americas, type="response")
log_am <- log_df %>% filter(americas == 1) %>% add_column(pred = pred_res)


coef <- summary(glm_model)$coefficients[, 1]
std_er <- summary(glm_model)$coefficients[, 2]

amer_coef <- data.frame(coef, std_er)
amer_coef <- cbind(feature = rownames(amer_coef), amer_coef)
amer_coef <- amer_coef %>% 
  mutate(model = 'americas')



#### 
# append everything together

model_coefs <- rbind(all_coef, africa_coef, asia_coef, me_coef, amer_coef, euro_coef)
model_coefs <- model_coefs %>%
  mutate(feature_name = case_when(feature == '(Intercept)' ~ 'intercept', 
                                  feature == 'log_aid' ~ 'sunk cost', 
                                  feature == 'log_mil' ~ 'military spending', 
                                  feature == 'log_ref' ~ 'refugees', 
                                  feature == 'log_fat' ~ 'fatalities', 
                                  feature == 'log_gdp' ~ 'gdp per capita'))


logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

model_coefs <- model_coefs %>%
  mutate(coef_prob = logit2prob(coef))

## OR and confidence intervals 
or_all = exp(cbind(OR = coef(glm_model), confint(glm_model)))

or_all <- data.frame(or_all) %>% mutate(model = 'all')
or_all <- cbind(feature = rownames(or_all), or_all)
or_af <- data.frame(or_af) %>% mutate(model = 'Africa')
or_af <- cbind(feature = rownames(or_af), or_af)
or_as <- data.frame(or_as) %>% mutate(model = 'asia')
or_as <- cbind(feature = rownames(or_as), or_as)
or_am <- data.frame(or_am) %>% mutate(model = 'america')
or_am <- cbind(feature = rownames(or_am), or_am)
or_eu <- data.frame(or_eu) %>% mutate(model = 'europe')
or_eu <- cbind(feature = rownames(or_eu), or_eu)
or_me <- data.frame(or_me) %>% mutate(model = 'Middle East')
or_me <- cbind(feature = rownames(or_me), or_me)

or_df <- rbind(or_all, or_af, or_as, or_am, or_eu, or_me)
or_df <- or_df %>% 
  mutate(feature_name = case_when(feature== 'log_aid' ~ 'Sunk cost', 
                                  feature=='log_fat' ~ 'Fatalities', 
                                  feature=='log_gdp' ~ 'GDP per capita', 
                                  feature=='log_mil' ~ 'Military expenditure', 
                                  feature=='log_ref' ~ 'Refugees', 
                                  TRUE ~ feature))

g <- or_df %>%
  filter(model != 'all', 
         feature != '(Intercept)', 
         model != 'america', 
         model != 'asia', 
         model != 'europe') %>% # model != 'americas'
  ggplot(aes(feature_name, OR)) +
  geom_errorbar(
    aes(ymin = X2.5.., ymax = X97.5.., color = model),
    position = position_dodge(0.5), width = .3
  ) +
  ggplot2::annotate("text", x = 0.75, y = 1.75, label = "↑ increase in\n odds of\n intervention", size=3, color="red3") +
  ggplot2::annotate("text", x = 1.35, y = 0.7, label = "↓ decrease in\n odds of\n intervention", size=3, color="red3") +
  geom_point(aes(color = model), position = position_dodge(0.5)) +
  geom_hline(yintercept=1, linetype="dashed", color = "red3") + 
  scale_color_manual(values = c("steelblue",  "midnightblue")) + 
  theme_bw() + 
  labs(title="",
       x ="Model feature", 
       y = "Odds ratio", 
       color = "Region", 
       caption = "* log unit increase in the feature value corresponds to an increase in the odds \nof peacekeeper intervention by a factor of the odds ratio") + 
  theme(legend.position = c(0.9, 0.9), 
        legend.title = element_blank(), 
        axis.title.x=element_blank()
        )

g



## Plot coefficients 

g <- model_coefs %>%
  filter(model != 'all', 
         feature_name != 'intercept') %>%
  ggplot(aes(x=feature_name, y=coef, color=model)) + 
  geom_point()

# (2) Standard error bars
g <- model_coefs %>%
  filter(model != 'all', 
         feature_name != 'intercept') %>% # model != 'americas'
  ggplot(aes(feature_name, coef)) +
  geom_errorbar(
    aes(ymin = coef-(1.96*std_er), ymax = coef+(1.96*std_er), color = model),
    position = position_dodge(0.5), width = 0.2
  ) +
  geom_point(aes(color = model), position = position_dodge(0.5)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red3") + 
  scale_color_manual(values = c("steelblue", "aquamarine4", "gold2", "salmon2", "midnightblue")) + 
  theme_bw() + 
  labs(title="",
       x ="Model feature", 
       y = "Coefficient", 
       color = "Region")
  
g


# africa and middle east

g <- model_coefs %>%
  filter(model != 'all', 
         feature_name != 'intercept',
         model != 'americas', 
         model != 'asia', 
         model != 'europe') %>%
  ggplot(aes(feature_name, coef)) +
  geom_errorbar(
    aes(ymin = coef-(1.96*std_er), ymax = coef+(1.96*std_er), color = model),
    position = position_dodge(0.5), width = 0.2
  ) +
  geom_point(aes(color = model), position = position_dodge(0.5)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red3") + 
  scale_color_manual(values = c("steelblue", "midnightblue")) + 
  theme_bw() + 
  labs(title="",
       x ="Model feature", 
       y = "Coefficient", 
       color = "Region")

g


### append all the predicted results together 
### make a cute plot

all_pred <- rbind(log_am, log_af, log_as, log_eu, log_me)
all_pred %>%
  filter(pk_total > 0, 
         middle_east == 1 | africa == 1) %>%
  mutate(region = case_when(americas == 1 ~ 'Americas', 
                            asia == 1 ~ 'Asia', 
                            africa == 1 ~ 'Africa', 
                            europe == 1 ~ 'Europe', 
                            middle_east == 1 ~ 'Middle East')) %>%
  ggplot(aes(x=pred, y=pk_total, color=region, size=best_fatality_estimate)) + 
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE, linetype = "dashed") +
  scale_color_manual(values = c("steelblue",  "midnightblue")) + #"aquamarine4", "gold2", "salmon2",
  theme_bw() + 
  labs(title="",
       x ="Modelled probability that a country will have at least one peacekeeper in a given year", 
       y = "Number of peacekeepers", 
       color = "") + 
  guides(size = FALSE) + 
  theme(legend.position = c(0.9, 0.9), 
        legend.title = element_blank())

# interactions

log_df2 <- log_df %>%
  mutate(# aid_ref = log_aid*log_ref, 
         #aid_fat = log_aid*log_fat, 
         # aid_mil = log_aid*log_mil, 
         #ref_fat = log_ref*log_fat, 
         #ref_mil = log_ref*log_mil, 
         #fat_mil = log_fat*log_mil
        
         # me_ref = middle_east*log_ref, 
         # me_aid = middle_east*log_aid, 
         # me_fat = middle_east*log_fat, 
         me_mil = middle_east*log_mil,
         # as_ref = asia*log_ref, 
         # as_aid = asia*log_aid, 
         # as_fat = asia*log_fat, 
         # as_mil = asia*log_mil,
         af_ref = africa*log_ref,
         # af_aid = africa*log_aid, 
         af_fat = africa*log_fat,
         af_mil = africa*log_mil
         ) %>% 
  select(-c(asia, africa, middle_east))

log_train2 <- create_train_test(log_df2, 0.8, train = TRUE)
log_test2 <- create_train_test(log_df2, 0.8, train = FALSE)

glm_model2 <- glm(intervention ~ .,family=binomial(link='logit'),data=log_train2)
# including all interactions (btw aid, refugees, fatalities, and military)
# aid-military interaction is significant
# aid-refugees interaction is significant
# fatalities not significant??

# including region-variable interactions (for middle east, asia and afria)
# Middle east-military is significant
# Asia -- 0.05 significance for military and aid
# Africa -- refugees, fatalities and military are significant

summary(glm_model2)

anova(glm_model2, test="Chisq")

fitted.results <- predict(glm_model2,newdata=log_test2,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != log_tes2t$intervention)
print(paste('Accuracy',1-misClasificError))
# "Accuracy 0.723270440251572"



## validation over time
class_df_time <- df_feats %>%
  ungroup() %>%
  select(-c(country, type_of_violence, region, Mission, pk_total, aid_dollars, americas, asia)) %>% # year??
  mutate_all(as.numeric) %>%
  filter(aid_lag_2yrsum >= 0) %>%
  mutate(aid_lag_2yrsum = case_when(aid_lag_2yrsum == 0 ~ 1, TRUE ~ aid_lag_2yrsum),
         log_aid = log(aid_lag_2yrsum), 
         log_ref = log(refugees), 
         log_mil = log(military_exp), 
         log_fat = log(best_fatality_estimate)) %>%
  select(-c(aid_lag_2yrsum, refugees, best_fatality_estimate, military_exp, middle_east, europe, africa))


train_pre2010 <- class_df_time %>% filter(year <= 2010) %>% select(-c(year))
test_post2010 <- class_df_time %>% filter(year > 2010) %>% select(-c(year))

# decision tree 
fit_pre2010 <- rpart(intervention~., data = train_pre2010, method = 'class', maxdepth=3) #, control=rpart.control(minsplit=30, cp=0.001) )
rpart.plot(fit_pre2010, extra = 106)

# predict
t_pred = predict(fit_pre2010, test_post2010, type="class")

# confusion matrix
confMat <- table(test_post2010$intervention, t_pred)
accuracy_time <- sum(diag(confMat))/sum(confMat)
print(accuracy_time)

# variable importance
varImp(fit_pre2010)

## train on africa predict in the mid-east
class_df_region <- df_feats %>%
  ungroup() %>%
  select(-c(country, type_of_violence, year, Mission, pk_total, aid_dollars, americas, asia)) %>% # year??
  filter(aid_lag_2yrsum >= 0) %>%
  mutate(aid_lag_2yrsum = case_when(aid_lag_2yrsum == 0 ~ 1, TRUE ~ aid_lag_2yrsum),
         log_aid = log(aid_lag_2yrsum), 
         log_ref = log(refugees), 
         log_mil = log(military_exp), 
         log_fat = log(best_fatality_estimate)) %>%
  select(-c(aid_lag_2yrsum, refugees, best_fatality_estimate, military_exp, middle_east, europe, africa))

train_africa <- class_df_region %>% filter(region == 'Africa') %>% select(-c(region))
test_me <- class_df_region %>% filter(region == 'Asia') %>% select(-c(region))

# decision tree 
fit_africa <- rpart(intervention~., data = train_africa, method = 'class', maxdepth=3) #, control=rpart.control(minsplit=30, cp=0.001) )
rpart.plot(fit_africa, extra = 106)

# predict
t_pred = predict(fit_africa, test_me, type="class")

# confusion matrix
confMat <- table(test_me$intervention, t_pred)
accuracy_reg <- sum(diag(confMat))/sum(confMat)
print(accuracy_reg)

# variable importance
varImp(fit_africa)


### 90s predict 2000s
train_90s <- class_df_time %>% filter(year < 2000) %>% select(-c(year))
test_00s <- class_df_time %>% filter(year >= 2000, year < 2010) %>% select(-c(year))

# decision tree 
fit_90s <- rpart(intervention~., data = train_90s, method = 'class', maxdepth=3) #, control=rpart.control(minsplit=30, cp=0.001) )
rpart.plot(fit_90s, extra = 106)

# predict
t_pred = predict(fit_90s, test_00s, type="class")

# confusion matrix
confMat <- table(test_00s$intervention, t_pred)
accuracy_00s <- sum(diag(confMat))/sum(confMat)
print(accuracy_00s)

# variable importance
varImp(fit_90s)


### 2000s predict 2010s
train_00s <- class_df_time %>% filter(year >= 2000, year < 2010) %>% select(-c(year))
test_10s <- class_df_time %>% filter(year >= 2010) %>% select(-c(year))

# decision tree 
fit_00s <- rpart(intervention~., data = train_00s, method = 'class', maxdepth=3) #, control=rpart.control(minsplit=30, cp=0.001) )
rpart.plot(fit_00s, extra = 106)

# predict
t_pred = predict(fit_00s, test_10s, type="class")

# confusion matrix
confMat <- table(test_10s$intervention, t_pred)
accuracy_10s <- sum(diag(confMat))/sum(confMat)
print(accuracy_10s)

# variable importance
varImp(fit_00s)

## What percentage of my dataset is intervention? 
p_intervene <- sum(df_feats$intervention) / length(df_feats$intervention)
p_intervene


## model by region 

## africa
df_af <- class_df_region %>% filter(region == 'Africa') %>% select(-c(region))
train_af<- create_train_test(df_af, 0.8, train = TRUE)
test_af <- create_train_test(df_af, 0.8, train = FALSE)

# decision tree 
fit_af <- rpart(intervention~., data = train_af, method = 'class', maxdepth=3) #, control=rpart.control(minsplit=30, cp=0.001) )
rpart.plot(fit_af, extra = 106)

# predict
t_pred = predict(fit_af, test_af, type="class")

# confusion matrix
confMat <- table(test_af$intervention, t_pred)
accuracy_af <- sum(diag(confMat))/sum(confMat)
print(accuracy_af)
# 0.7838

# variable importance
varImp(fit_af)
# Overall
# log_aid   41.09
# log_fat   27.05
# log_mil   34.42
# log_ref   42.43

## military spend more imp in Africa 
## but not the way you think -- higher spending = more likely to send peacekeepers
## probably bc state violence
## aid and refugees still most important 

## middle east
df_me <- class_df_region %>% filter(region == 'Middle East') %>% select(-c(region))
train_me<- create_train_test(df_me, 0.8, train = TRUE)
test_me <- create_train_test(df_me, 0.8, train = FALSE)

# decision tree 
fit_me <- rpart(intervention~., data = train_me, method = 'class', maxdepth=3) #, control=rpart.control(minsplit=30, cp=0.001) )
rpart.plot(fit_me, extra = 106)

# predict
t_pred = predict(fit_me, test_me, type="class")

# confusion matrix
confMat <- table(test_me$intervention, t_pred)
accuracy_me <- sum(diag(confMat))/sum(confMat)
print(accuracy_me)
# 0.5556

# variable importance
varImp(fit_me)
# Overall
# log_aid   19.79
# log_fat   12.73
# log_mil   19.37
# log_ref   32.05

## hmm, maybe not enough data here only 221 obs
## refugees still most important

## asia
df_as <- class_df_region %>% filter(region == 'Asia') %>% select(-c(region))
train_as <- create_train_test(df_as, 0.8, train = TRUE)
test_as <- create_train_test(df_as, 0.8, train = FALSE)

# decision tree 
fit_as <- rpart(intervention~., data = train_as, method = 'class', maxdepth=3) #, control=rpart.control(minsplit=30, cp=0.001) )
rpart.plot(fit_as, extra = 106)

# predict
t_pred = predict(fit_as, test_as, type="class")

# confusion matrix
confMat <- table(test_as$intervention, t_pred)
accuracy_as <- sum(diag(confMat))/sum(confMat)
print(accuracy_as)
# 0.6528

# variable importance
varImp(fit_as)
# Overall
# log_aid   8.300
# log_fat   9.635
# log_mil  18.980
# log_ref  11.083

## hmm, maybe not enough data here only 359 obs
## military most important in asia
## looks bad for myanmar 

## europe (maybe not enough data)
df_eu <- class_df_region %>% filter(region == 'Europe') %>% select(-c(region))
train_eu <- create_train_test(df_eu, 0.8, train = TRUE)
test_eu <- create_train_test(df_eu, 0.8, train = FALSE)

# decision tree 
fit_eu <- rpart(intervention~., data = train_eu, method = 'class', maxdepth=3) #, control=rpart.control(minsplit=30, cp=0.001) )
rpart.plot(fit_eu, extra = 106)

# predict
t_pred = predict(fit_eu, test_eu, type="class")

# confusion matrix
confMat <- table(test_eu$intervention, t_pred)
accuracy_eu <- sum(diag(confMat))/sum(confMat)
print(accuracy_eu)
# 1

# variable importance
# varImp(fit_eu)

## hmm, maybe not enough data here only 167 obs
## they're not going in in Europe... 

## americas (maybe not enough data)
df_am <- class_df_region %>% filter(region == 'Americas') %>% select(-c(region))
train_am <- create_train_test(df_am, 0.8, train = TRUE)
test_am <- create_train_test(df_am, 0.8, train = FALSE)

# decision tree 
fit_am <- rpart(intervention~., data = train_am,  method = 'class', maxdepth=3) #, control=rpart.control(minsplit=30, cp=0.001) )
rpart.plot(fit_am, extra = 106)

# predict
t_pred = predict(fit_am, test_am, type="class")

# confusion matrix
confMat <- table(test_am$intervention, t_pred)
accuracy_am <- sum(diag(confMat))/sum(confMat)
print(accuracy_am)
# 0.8837

# variable importance
varImp(fit_am)
# Overall
# log_aid   3.055
# log_fat   8.969
# log_mil   8.479
# log_ref  12.143

## hmm, maybe not enough data here only 211 obs
## refugees most important


# 19% 
# is 75% accuracy any good?

######### Not enough data to meaningfully predict size of peacekeeping force

## regression = predict size of peacekeeping operation

reg_df <- df_feats %>%
  filter(intervention == 1,
         pk_total > 0) %>%
  # filter(intervention == 1, 
  #        pk_total > 0, 
  #        best_fatality_estimate > 0, 
  #        refugees > 0) %>%
  ungroup() %>%
  select(-c(country, type_of_violence, state, one_sided, non_state, region, region_full, aid_dollars, year)) %>% # year??
  mutate_all(as.numeric) %>%
  filter(aid_lag_2yrsum >= 0) %>%
  mutate(#pk_total = case_when(pk_total == 0 ~ 1, TRUE ~ pk_total),
         aid_lag_2yrsum = case_when(aid_lag_2yrsum == 0 ~ 1, TRUE ~ aid_lag_2yrsum),
         refugees = case_when(refugees == 0 ~ 1, TRUE ~ refugees),
         best_fatality_estimate = case_when(best_fatality_estimate == 0 ~ 1, TRUE ~ best_fatality_estimate),
         log_aid = log(aid_lag_2yrsum),
         log_ref = log(refugees),
         log_mil = log(military_exp),
         log_fat = log(best_fatality_estimate)) %>%
  select(-c(best_fatality_estimate, refugees, aid_lag_2yrsum, military_exp, asia, africa, middle_east, europe, americas, p_muslim, intervention)) %>%
  drop_na() 

# best_fatality_estimate, refugees,
#  asia, africa, middle_east, europe, americas,
# split test train

data_train_reg <- create_train_test(reg_df, 0.8, train = TRUE)
data_test_reg <- create_train_test(reg_df, 0.8, train = FALSE)

# explore the data.
ggplot(data_train_reg, aes(pk_total)) + geom_density(fill="blue")
ggplot(data_train_reg, aes(log(pk_total))) + geom_density(fill="blue")
ggplot(data_train_reg, aes(sqrt(pk_total))) + geom_density(fill="blue")

# ggplot(data_train_reg, aes(refugees)) + geom_density(fill="blue")
# ggplot(data_train_reg, aes(log(refugees))) + geom_density(fill="blue")
# ggplot(data_train_reg, aes(sqrt(refugees))) + geom_density(fill="blue")
# 
# ggplot(data_train_reg, aes(military_exp)) + geom_density(fill="blue")
# ggplot(data_train_reg, aes(log(military_exp))) + geom_density(fill="blue")
# ggplot(data_train_reg, aes(sqrt(military_exp))) + geom_density(fill="blue")
# 
# ggplot(data_train_reg, aes(aid_lag_2yrsum)) + geom_density(fill="blue")
# ggplot(data_train_reg, aes(log(aid_lag_2yrsum))) + geom_density(fill="blue")
# ggplot(data_train_reg, aes(sqrt(aid_lag_2yrsum))) + geom_density(fill="blue")

#Let’s make default model.
reg_model = lm(log(pk_total)~., data=data_train_reg)
summary(reg_model)
par(mfrow=c(2,2))
plot(reg_model)


#############################################
##              visualize                 ##
#############################################

fig_df <- df_feats %>%
  ungroup() %>%
  # select(-c(country, type_of_violence, region, Mission, pk_total, aid_dollars, year)) %>% # year??
  # mutate_all(as.numeric) %>%
  filter(aid_lag_2yrsum >= 0) %>%
  mutate(aid_lag_2yrsum = case_when(aid_lag_2yrsum == 0 ~ 1, TRUE ~ aid_lag_2yrsum),
         log_aid = log(aid_lag_2yrsum), 
         log_ref = log(refugees), 
         log_mil = log(military_exp), 
         log_fat = log(best_fatality_estimate))
  # select(-c(aid_lag_2yrsum, refugees, best_fatality_estimate, military_exp))


# fig region -- afghanistan should be ME 
# split out africa into smaller regions

fig_df %>%
  # filter(country != 'Afghanistan') %>%
  group_by(year, region) %>%
  summarise(fatalities = sum(best_fatality_estimate), 
            refugees = sum(refugees), 
            pkeepers = sum(pk_total)) %>%
  ggplot(aes(x=year, y=pkeepers, color=region)) + 
  geom_line() 
  # theme(legend.position = "none")

fig_df %>%
  group_by(year, region) %>%
  summarise(fatalities = sum(best_fatality_estimate), 
            refugees = sum(refugees)) %>%
  ggplot(aes(x=year, y=refugees, fill=region)) + 
  geom_bar(position="stack", stat="identity") 

ggplot(fig_df, aes(color=region, group=region, y=refugees, x=year)) + 
  geom_line(stat="identity") + #(position="stack", stat="identity") + 
  # geom_line(data=all, aes(x=year, y=cost/18), color="red3", size=1, stat="identity", show.legend = FALSE) +
  # geom_errorbar(data=all, aes(ymin=lower95/18, ymax=upper95/18), color="red3", width=0.5, size=.8, show.legend = FALSE) +
  # geom_errorbar(aes(x=2021, ymin=20/18, ymax=125/18), color="red3", linetype="dashed", width=0.5, size=.8, show.legend = FALSE) +
  # annotate("text", x = 2003, y = 15, label = "Hurricane\n Katrina", size=3) + 
  # annotate("text", x = 2015, y = 23.5, label = "Hurricanes\n Harvey,\n Maria,\n and Irma", size=3) + 
  # annotate("text", x = 2022, y = 8, label = "Winter\n Storm", size=3) + 
  # scale_y_continuous(
  #   
  #   # Features of the first axis
  #   name = "Number of weather events",
  #   
  #   # Add a second axis and specify its features
  #   sec.axis = sec_axis(~.*18, name="Cost in billions (CPI-adjusted $)")
  # ) +
  theme_bw() 
  # scale_fill_manual(values=c("red3", "midnightblue", "steelblue", "steelblue1", "lightskyblue3", "aquamarine4", "gold2", "salmon2")) +  # "#56B4E9"))
  # labs(
  #   title = "The damage is done",
  #   subtitle = "United States, Billion dollar disaster events, 1980-2021",
  #   x ="Year", 
  #   fill = '', 
  #   caption = "Data source: NOAA; Center for Climate and Energy Solutions") +
  # theme(axis.line.y.right = element_line(color = "red3"), 
  #       axis.ticks.y.right = element_line(color = "red3"),
  #       axis.text.y.right = element_text(color = "red3"), 
  #       axis.title.y.right = element_text(color = "red3"),
  #       plot.caption = element_text(hjust = 0)
  # )



