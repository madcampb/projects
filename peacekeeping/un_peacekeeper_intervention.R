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

## Country variables



#############################################
##            reconfigure                  ##
#############################################


#############################################
##              modeling                   ##
#############################################


#############################################
##             make plots                  ##
#############################################
