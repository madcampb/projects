# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4)

## Load libraries

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(purrr)
library(tidyverse)
library(lubridate)
library(zoo)
library(tm)
library(topicmodels)
library(reshape2)
library(wordcloud)
library(pals)


#############################################
##            import data                  ##
#############################################

# AFP
afp <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/afp.csv')

afp <- afp %>%
  mutate(fc_source = 'afp', 
         source = '', 
         details = '', 
         rating = '') %>% 
  select(-c(X, time))

# AP
ap <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/ap.csv')

ap <- ap %>%
  mutate(fc_source = 'ap', 
         source = '', 
         details = '', 
         rating = '') %>% 
  select(-c(X))


# CheckYourFact
cyf <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/checkyourfact.csv')

cyf <- cyf %>%
  mutate(fc_source = 'checkyourfact', 
         source = '', 
         details = '', 
         rating = '') %>% 
  select(-c(X))

# ClimateFeedback
cf <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/climatefeedback.csv')

cf <- cf %>%
  mutate(fc_source = 'climatefeedback') %>% 
  select(-c(X))

# HealthFeedback
hf <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/healthfeedback.csv')

hf <- hf %>%
  mutate(fc_source = 'healthfeedback') %>% 
  select(-c(X))

# Dispatch
dis <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/dispatch.csv')

dis <- dis %>%
  mutate(fc_source = 'dispatch', 
         source = '', 
         rating = '', 
         details = review) %>% 
  select(-c(X, review))

# LeadStories
ls <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/leadstories.csv')

ls <- ls %>%
  mutate(fc_source = 'leadstories', 
         rating = '', 
         source = '') %>% 
  select(-c(X))

# PolitiFact
pf <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/politifact.csv')

pf <- pf %>%
  mutate(fc_source = 'politifact', 
         details = '', 
         source = '') %>% 
  select(-c(X))

# Reuters
rt <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/reuters.csv')

rt <- rt %>%
  mutate(fc_source = 'reuters', 
         rating = '', 
         source = '') %>% 
  select(-c(X))

# USAToday
usa <- read.csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/usatoday.csv')

usa <- usa %>%
  mutate(fc_source = 'usatoday', 
         rating = '', 
         source = '') %>% 
  select(-c(X))


# combine everything together
claims <- bind_rows(afp, ap, cf, cyf, dis, hf, ls, pf, rt, usa)

claims <- claims %>%
  mutate(newdate = lubridate::dmy(date))

# there are some duplicates
# keep headline at earliest associated date 

#############################################
##        explorartory analysis            ##
#############################################

claims %>% 
  filter(fc_source != 'ap', 
         fc_source != 'usatoday', 
         fc_source != 'healthfeedback', 
         fc_source != 'climatefeedback') %>%
  group_by(newdate) %>% 
  tally() %>% 
  mutate(rolling_avg=rollapply(n,7,mean,align='right',fill=NA)) %>%
  filter(newdate > "2020-10-31") %>%
  ggplot(aes(x=newdate, y=rolling_avg, group=1)) +
  geom_line()


claims %>% 
  filter(fc_source != 'healthfeedback',
         fc_source != 'climatefeedback') %>%
  group_by(newdate, fc_source) %>% 
  tally() %>% 
  group_by(fc_source) %>%
  mutate(rolling_avg=rollapply(n,7,mean,align='right',fill=NA)) %>%
  filter(newdate > "2020-10-31") %>%
  ggplot(aes(x=newdate, y=rolling_avg, group=fc_source, color=fc_source)) +
  geom_line()

# something is wrong with the health / climate feedback data
# lots of repeats... look back at the urls 

#############################################
##            preprocessing                ##
#############################################

# data

corpus_data <- claims %>%
  mutate(text = headline) %>%
  select(text, newdate, fc_source) %>%
  rename(date = newdate) %>% 
  group_by(text, fc_source) %>%
  summarise(date = min(date)) 

corpus_data <- tibble::rowid_to_column(corpus_data, "doc_id") %>%
  select(doc_id, text, date, fc_source)


# load stopwords
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
extra_stopwords <- c('facebook', 'post', 'twitter', 'tweet', 'youtube', 'video', 
                     'fact', 'check', 'factcheck', 'misleading', 'falsely', 'claim', 
                     'claims', 'mislead', 'conspiracy', 'theory', 'inacurate', 
                     'inacurately', 'tweets', 'posts', 'manipulated', 'doctored', 
                     'share', 'shared', 'spread', 'spreads', 'meme', 'fake', 'evidence', 
                     'misinformation', 'disinformation', 'misrepresent', 'misrepresents', 
                     'information', 'missing', 'context', 'fake', 'viral', 'flaw', 
                     'flawed', 'reason', 'reasoning', 'image', 'peddles', 'faslsehood', 
                     'falsehoods', 'checking', 'downplay', 'debunk', 'debunking', 
                     'videos', 'images', 'photos', 'photo', 'memes', 'quote', 'quotes')

all_stopwords <- c(english_stopwords, extra_stopwords)

# create corpus object
corpus <- Corpus(DataframeSource(corpus_data))

# Preprocessing chain
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, all_stopwords)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

#############################################
##          model calculation              ##
#############################################

# compute document term matrix with terms >= minimumFrequency
minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))

# have a look at the number of documents and terms in the matrix
dim(DTM)

# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]
