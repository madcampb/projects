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
  filter(fc_source != 'ap', 
         fc_source != 'usatoday', 
         fc_source != 'healthfeedback', 
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

corpus_data <- tibble::rowid_to_column(claims, "doc_id") %>%
  mutate(text = headline) %>%
  select(doc_id, text, newdate, fc_source) %>%
  rename(date = newdate) %>%
  filter(fc_source != 'healthfeedback', 
         fc_source != 'climatefeedback') %>% 
  mutate(text = gsub("[[:punct:]]+", "", text))

# duplicates included above, removed below
# something weird happening here, come back to this
# TODO 

corpus_data2 <- claims %>%
  mutate(text = headline) %>%
  select(text, newdate, fc_source) %>%
  rename(date = newdate) %>%
  group_by(text, fc_source) %>%
  summarise(date = min(date)) %>%
  ungroup()

corpus_data2 <- tibble::rowid_to_column(corpus_data2, "doc_id") %>%
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
                     'videos', 'images', 'photos', 'photo', 'memes', 'quote', 'quotes', 
                     "“", "”", "’", "‘", "“", "”", "“", 'show', 'shows', 'proof', 
                     'report', 'data', 'false', 'true', 'states', 'news', 'real', 
                     'biden', 'bidens', 'trump', 'trumps', 'joe', 'donald', 'president',
                     'nancy', 'pelosi', 'mike', 'pence', 'barack', 'obama', 'state', 
                     'man', 'people', 'democrats', 'republicans', 'democrat', 'republican', 
                     'day', 'million', 'billion', 'voter', 'voters', 'votes', 'cast', 
                     'new', 'york', 'texas', 'pennsylvania', 'michigan', 'wisconsin', 
                     'call', 'time', 'vote', 'days', 'year', 'found', 'years', 'give', 
                     'send', 'lives', 'white', 'house', 'live', 'photograph', 'kamala', 
                     'harris', 'amy', 'coney', 'barrett', 'mitt', 'romney', 'play', 
                     'make', 'pass', 'full', 'interview', 'hillary', 'clinton', 
                     'kayleigh', 'mcenany', 'hear')

extra_stopwords_lim <- c('facebook', 'post', 'twitter', 'tweet', 'youtube', 'video', 
                     'fact', 'check', 'factcheck', 'misleading', 'falsely', 'claim', 
                     'claims', 'mislead', 'conspiracy', 'theory', 'inacurate', 
                     'inacurately', 'tweets', 'posts', 'manipulated', 'doctored', 
                     'share', 'shared', 'spread', 'spreads', 'meme', 'fake', 'evidence', 
                     'misinformation', 'disinformation', 'misrepresent', 'misrepresents', 
                     'information', 'missing', 'context', 'fake', 'viral', 'flaw', 
                     'flawed', 'reason', 'reasoning', 'image', 'peddles', 'faslsehood', 
                     'falsehoods', 'checking', 'downplay', 'debunk', 'debunking', 
                     'videos', 'images', 'photos', 'photo', 'memes', 'quote', 'quotes', 
                     'show', 'shows', 'proof', 'ceo',
                     'report', 'data', 'false', 'true', 'news', 'real', 
                     'man', 'people', 
                     'day', 'million', 'billion',
                     'call', 'time', 'vote', 'days', 'year', 'found', 'years', 'give', 
                     'send', 'live', 'photograph',  
                     'make', 'pass', 'full')

all_stopwords <- c(english_stopwords, extra_stopwords)

# create corpus object
corpus <- Corpus(DataframeSource(corpus_data))

# Preprocessing chain
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, all_stopwords)
# this isn't removing all the punctuation 
# TODO
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
# processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

#############################################
##            topic modeling               ##
#############################################

################### BTM ######################

library(udpipe)
library(data.table)
library(stopwords)
library(BTM)
library(textplot)
library(ggraph)

all_stop <- c(stopwords("en"), extra_stopwords_lim, english_stopwords)
corpus_data2$text   <- tolower(corpus_data2$text)
corpus_data2$text   <- gsub("'", "", corpus_data2$text)
corpus_data2$text   <- gsub("<.+>", "", corpus_data2$text)

anno    <- udpipe(corpus_data2, "english", trace = 10)
biterms <- as.data.table(anno)
biterms <- biterms[, cooccurrence(x = lemma,
                                  relevant = upos %in% c("NOUN", "ADJ", "VERB") & 
                                    nchar(lemma) > 2 & !lemma %in% all_stop,
                                  skipgram = 3),
                   by = list(doc_id)]


## Build model 
set.seed(123456)
traindata <- subset(anno, upos %in% c("NOUN", "ADJ", "VERB") & !lemma %in% all_stop & nchar(lemma) > 2)
traindata <- traindata[, c("doc_id", "lemma")]
model     <- BTM(traindata, biterms = biterms, k = 10, iter = 2000, background = TRUE, trace = 100)

plot(model, top_n = 10)

## Inspect the model - topic frequency + conditional term probabilities
model$theta

topicterms <- terms(model, top_n = 10)
topicterms

################### LDA ######################

# # compute document term matrix with terms >= minimumFrequency
# minimumFrequency <- 5
# DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# 
# # have a look at the number of documents and terms in the matrix
# dim(DTM)
# 
# # due to vocabulary pruning, we have empty rows in our DTM
# # LDA does not like this. So we remove those docs from the
# # DTM and the metadata
# sel_idx <- slam::row_sums(DTM) > 0
# DTM <- DTM[sel_idx, ]
# textdata <- corpus_data[sel_idx, ]
# 
# # number of topics
# K <- 20
# 
# # set random number generator seed
# set.seed(9161)
# 
# # compute the LDA model, inference via 1000 iterations of Gibbs sampling
# topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))
# 
# # have a look a some of the results (posterior distributions)
# tmResult <- posterior(topicModel)
# 
# # format of the resulting object
# attributes(tmResult)
# 
# # lengthOfVocab
# nTerms(DTM)    
# 
# # topics are probability distribtions over the entire vocabulary
# beta <- tmResult$terms   # get beta from results
# dim(beta)                # K distributions over nTerms(DTM) terms
# rowSums(beta)            # rows in beta sum to 1
# nDocs(DTM)               # size of collection
# 
# # for every document we have a probaility distribution of its contained topics
# theta <- tmResult$topics 
# dim(theta)               # nDocs(DTM) distributions over K topics
# rowSums(theta)[1:10]     # rows in theta sum to 1
# 
# # take a look at the terms in the topics 
# terms(topicModel, 10)
# exampleTermData <- terms(topicModel, 10)
# exampleTermData[, 1:8]
# 
# top5termsPerTopic <- terms(topicModel, 5)
# topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
# 
# # visualize topics as word cloud
# topicToViz <- 19 # change to topic of interest
# # topicToViz <- grep('mexico', topicNames)[1] # Or select a topic by a term contained in its name
# 
# # select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
# top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# words <- names(top40terms)
# 
# # extract the probabilites of each of the 40 terms
# probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# 
# # visualize the terms as wordcloud
# mycolors <- brewer.pal(8, "Dark2")
# wordcloud(words, probabilities, random.order = FALSE, color = mycolors)
# 
# 
# exampleIds <- c(2, 100, 200)
# lapply(corpus[exampleIds], as.character)
# print(paste0(exampleIds[1], ": ", substr(content(corpus[[exampleIds[1]]]), 0, 400), '...'))
# print(paste0(exampleIds[2], ": ", substr(content(corpus[[exampleIds[2]]]), 0, 400), '...'))
# print(paste0(exampleIds[3], ": ", substr(content(corpus[[exampleIds[3]]]), 0, 400), '...'))
# 
# N <- length(exampleIds)
# # get topic proportions form example documents
# topicProportionExamples <- theta[exampleIds,]
# colnames(topicProportionExamples) <- topicNames
# vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")
# ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") +
#   geom_bar(stat="identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   coord_flip() +
#   facet_wrap(~ document, ncol = N)


#############################################
##          regular expressions            ##
#############################################

claims2 <- claims %>%
  filter(fc_source != 'ap', 
         fc_source != 'usatoday', 
         fc_source != 'climatefeedback', 
         fc_source != 'healthfeedback') %>%
  mutate(claim_gp = 'other', 
         text = gsub("<.+>", "", gsub("'", "", tolower(headline)))) %>% # lowercase, remove punctuation
  select(headline, claim_gp, text, fc_source, newdate)

# general politics

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('white house|trump|biden|pence|pelosi|harris|obama|aoc|ocasio cortez|sanders|romney|democrat|republican|senator|congress|schiff|hannity|clinton|warren|kayleigh|carlson|nikki haley|reagan|presidential debate|debate|maga|campaign events|us president|executive order|stimulous',text), 'politics', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()


# capitol riot antifa storm national guard trump supporters

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('capitol riot|rioter|insurrection|seige|storming|stormed|us capitol|capitol assault|military tribunals',text), 'capital_riot', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# stimmies

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('stimulus|american rescue plan',text), 'stimmies', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()


# covid / vaccines / masks / coronavirus / flu / mandates

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('covid|coronavirus|mask|vaccine|cdc|pandemic|ventilators|fauci|sars-cov-2|vaccinated|vaccinate|pfizer|take the shot|jab|major disease outbreaks',text), 'covid', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# voter fraud georgia ballot find county michigan

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl("more votes than people|rigged election|voting machines|voter suppression|poll workers|voting issue|voter turnout|election fraud|voter fraud|misprinted ballots|election audit|actual results|popular vote|ballots|‘rigged’ election|voting servers|dominion|certify vote|dead voter|deleted vote|fraud sting|election map|poll clerks|vote count|election reporting|ballot processing|vote surge|voter registration|fraud claims|election was rigged|ballot|vote tally|pennsylvania lead|counting votes|premature declaration of victory|polling|presidential election|fraud|more votes cast|registered voters|2020 election",text), 'voter_fraud', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# mail in ballots 

# dominion voting system failure seize server machine 

# gamestop / reddit 

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('gamestop|reddit|robinhood',text), 'gamestop', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# deep state / soros /qanon

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('deep state|soros|qanon',text), 'deep_state', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# misc healthcare

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('health care|prescription|benadryl',text), 'healthcare', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()


# migrants

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('border authorities|migrant|migrants|immigration|carravan|ice operation',text), 'immigration', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# BLM 

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl("blm|black lives matter|floyd's|george floyd|defunded police|defund the police|derek chauvin|white supremecy|white people|colin kaepernick|defunded the police",text), 'blm', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# texas power

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl("texas pastor|power output|storm-hit texas|texas power|texas wind|de-ice|increasing power|power grid|fema|texas freezes|national guard to texas|joel osteen|texas energy|emergency in texas|texas utilities|texas winter storm|wind and our solar|winter snow|fake texas snow|wind turbines|record cold snap|texas' blackout|lakewood",text), 'texas power', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# climate change 

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('climate|greta thunberg|global warming|al gore|environmentalist',text), 'climate', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# supreme court 

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('amy coney barrett|john roberts|supreme court|ruth bader ginsburg|ginsburg|barrett|roberts|rbg',text), 'supreme_court', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# epstein

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('epstein|ghislaine|maxwell',text), 'epstein', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# antifa

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('antifa',text), 'antifa', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# international

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('israeli|beirut|hong kong|uganda|french|berlin|scotland|boris johnson|egypt|myanmar|justin trudeau|nigerians|australians|france|algerian|rwanda|australia|poland|russia',text), 'international', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# child trafficking

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('child trafficking|pedophiles|pedophile|dozens of girls|children in|wayfaiar|child abuse',text), 'child_trafficking', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# USPS

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('usps|us postal service|postal service|dejoy|post office|postmaster general',text), 'usps', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()

# celebrities

claims2 <- claims2 %>%
  mutate(claim_gp = ifelse(grepl('oprah|kobe|serena|ron howard|taylor swift|elon musk|amy schumer|keanu reeves|jim carrey|shakira',text), 'usps', as.character(claim_gp)))

claims2 %>% group_by(claim_gp) %>% tally()


# tech misc 


# science misc


# social misc


#### VISUALIZE

claims2 %>% 
  filter(fc_source != 'ap', 
         fc_source != 'usatoday', 
         fc_source != 'healthfeedback', 
         fc_source != 'climatefeedback') %>%
  group_by(newdate, claim_gp) %>% 
  tally() %>% 
  group_by(claim_gp) %>%
  mutate(rolling_avg=rollapply(n,7,mean,align='right',fill=NA)) %>%
  filter(newdate > "2020-10-31") %>%
  ggplot(aes(x=newdate, y=rolling_avg, group=claim_gp, color=claim_gp)) +
  geom_line()


