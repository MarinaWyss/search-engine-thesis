library(tidyverse)
library(tidytext)
library(textdata)
library(sentimentr)
library(quanteda)

#beforeElection <- fullDataSet %>% 
#  filter(date <= "2018-11-07")

##################################
############# PREP ###############
##################################

# tokens
searchTokens <- fullDataSet %>% 
  unnest_tokens(word, search_term) 

# remove stopwords
'%ni%' <- Negate('%in%')

searchTokens <- searchTokens %>%
  anti_join(get_stopwords()) %>% 
  filter(word %ni% c("http", "https")) %>% 
  mutate(word = gsub("[[:punct:]]", " ", word),
         hasNum = str_extract_all(word, "[[:digit:]]+"),
         wordClean = ifelse(hasNum == "character(0)", word, NA)) %>% 
  filter(!is.na(wordClean)) %>% 
  select(-wordClean, -hasNum)

##################################
########### KEYWORDS #############
##################################

# most common queries
mostCommon <- searchTokens %>% 
  count(word, sort = TRUE)

# most common queries by partisanship
partisanQueries <- searchTokens %>% 
  filter(voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                TRUE ~ "Non-Voter")) %>% 
  group_by_(~ voteChoice) %>%
  count(word, sort = TRUE) %>% 
  slice(1:10) 

partisanQueriesPlotDem <- partisanQueries %>% 
  filter(voteChoice == "Democrat") %>% 
  ggplot(aes(x = reorder(word, -n), y = n)) + 
  geom_bar(stat = "identity", aes(fill = word)) +
  labs(x = "Query",
       y = "Number of Searches",
       title = "Top 10 Query Terms: Democrats")

partisanQueriesPlotRep <- partisanQueries %>% 
  filter(voteChoice == "Republican") %>% 
  ggplot(aes(x = reorder(word, -n), y = n)) + 
  geom_bar(stat = "identity", aes(fill = word)) +
  labs(x = "Query",
       y = "Number of Searches",
       title = "Top 10 Query Terms: Republicans")
  
partisanQueriesPlotNon <- partisanQueries %>% 
  filter(voteChoice == "Non-Voter") %>% 
  ggplot(aes(x = reorder(word, -n), y = n)) + 
  geom_bar(stat = "identity", aes(fill = word)) +
  labs(x = "Query",
       y = "Number of Searches",
       title = "Top 10 Query Terms: Non-Voters")

##################################
########## WORDSCORES ############
##################################

# turnout terms
wsTokensTurnout <- fullDataSet %>% 
  select(pmxid, turnout, date, search_term) %>% 
  select(-date) %>% 
  rename(text = search_term) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(text, collapse = " "),
            turnout = min(turnout))

wsCorpusTurnout <- corpus(wsTokensTurnout$text)
docnames(wsCorpusTurnout) <- wsTokensTurnout$pmxid

wsDFMTurnout <- dfm(wsCorpusTurnout, 
                    remove_punct = TRUE, 
                    remove_numbers = TRUE, 
                    remove_symbols = TRUE,
                    remove = c(stopwords("english"), 
                               "http", "https"))

wsDFMTurnout <- dfm_trim(wsDFMTurnout, min_docfreq = 2)

wsModelTurnout <- textmodel_wordscores(wsDFMTurnout, 
                                       wsTokensTurnout$turnout, 
                                       smooth = 0.5)

swTurnout <- sort(coef(wsModelTurnout))

wsNonTerms <- head(swTurnout, 10)
wsVoteTerms <- tail(swTurnout, 10)

# party choice
wsTokensParty <- fullDataSet %>% 
  select(pmxid, voteChoice, date, search_term) %>% 
  filter(voteChoice == 1 | voteChoice == 2) %>% 
  mutate(voteChoice = ifelse(voteChoice == 1, 1, -1)) %>% 
  select(-date) %>% 
  rename(text = search_term) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(text, collapse = " "),
            voteChoice = min(voteChoice))

wsCorpusParty <- corpus(wsTokensParty$text)
docnames(wsCorpusParty) <- wsTokensParty$pmxid

wsDFMParty <- dfm(wsCorpusParty, 
            remove_punct = TRUE, 
            remove_numbers = TRUE, 
            remove_symbols = TRUE,
            remove = c(stopwords("english"), 
                       "http", "https"))

wsDFMParty <- dfm_trim(wsDFMParty, min_docfreq = 2)

wsModelParty <- textmodel_wordscores(wsDFMParty, 
                                     wsTokensParty$voteChoice, 
                                     smooth = 0.5)

swParty <- sort(coef(wsModelParty))

wsDemTerms <- head(swParty, 10) # dems -1
wsRepTerms <- tail(swParty, 10) # reps 1


##################################
###########  KEYNESS  ############
##################################

# turnout
keynessTurnout <- fullDataSet %>% 
  select(turnout, date, search_term) %>% 
  select(-date) %>% 
  rename(text = search_term) %>% 
  group_by(turnout) %>% 
  summarise(text = paste(text, collapse = " ")) %>% 
  mutate(turnout = ifelse(turnout == 1, "Voters", "Non-Voters")) 

keynessCorpusTurnout <- corpus(keynessTurnout$text)
docnames(keynessCorpusTurnout) <- keynessTurnout$turnout
docvars(keynessCorpusTurnout, "turnout") <- keynessTurnout$turnout

keynessDFMTurnout <- dfm(keynessCorpusTurnout, 
                       groups = "turnout",
                       remove_punct = TRUE, 
                       remove_numbers = TRUE, 
                       remove_symbols = TRUE,
                       remove = c(stopwords("english"), 
                                  "http", "https"))

keynessModelTurnout <- textstat_keyness(keynessDFMTurnout, target = "Voters")

textplot_keyness(keynessModelTurnout) 

# party choice
keynessParty <- fullDataSet %>% 
  select(voteChoice, date, search_term) %>% 
  filter(voteChoice == 1 | voteChoice == 2) %>% 
  select(-date) %>% 
  rename(text = search_term) %>% 
  group_by(voteChoice) %>% 
  summarise(text = paste(text, collapse = " ")) %>% 
  mutate(party = ifelse(voteChoice == 1, "Republican", "Democrat")) 
  
keynessCorpusParty <- corpus(keynessParty$text)
docnames(keynessCorpusParty) <- keynessParty$party
docvars(keynessCorpusParty, "party") <- keynessParty$party

keynessDFMParty <- dfm(keynessCorpusParty, 
                       groups = "party",
                       remove_punct = TRUE, 
                       remove_numbers = TRUE, 
                       remove_symbols = TRUE,
                       remove = c(stopwords("english"), 
                             "http", "https"))

keynessModelParty <- textstat_keyness(keynessDFMParty, target = "Republican")

textplot_keyness(keynessModelParty) 


##################################
########### SENTIMENT ############
##################################

# sentiment binary
bing <- get_sentiments("bing")

sentimentBing <- searchTokens %>%
  inner_join(bing)

sentimentBingParty <- sentimentBing %>% 
  select(pmxid, voteChoice, word, sentiment) %>% 
  mutate(sentimentBinary = ifelse(sentiment == "negative", 0, 1)) %>% 
  group_by(voteChoice) %>% 
  summarize(meanSentiment = mean(sentimentBinary)) %>% 
  filter(!is.na(voteChoice) & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 4 ~ "Non Voter"))

sentimentPlotParty <- ggplot(data = sentimentBingParty, 
                             aes(x = voteChoice, y = meanSentiment)) +
  geom_bar(stat = "identity", aes(fill = voteChoice)) + 
  geom_text(aes(label = round(meanSentiment, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25)

# sentiment NRC
nrc <- get_sentiments("nrc")

sentimentNRC <- searchTokens %>%
  inner_join(nrc)

sentimentNRCParty <- sentimentNRC %>% 
  select(pmxid, voteChoice, word, sentiment) %>% 
  filter(!is.na(voteChoice) & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 4 ~ "Non Voter")) %>% 
  filter(sentiment != "positive" & sentiment != "negative")

sentimentNRCPlotDem <- sentimentNRCParty %>% 
  filter(voteChoice == "Democrat") %>% 
  group_by(sentiment) %>% 
  summarize(countSentiment = length(sentiment)) %>% 
  ggplot(aes(x = sentiment, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment)) +
  labs(x = "Sentiment",
       y = "Number of Searches",
       title = "Sentiment Distribution: Democrats")

sentimentNRCPlotRep <- sentimentNRCParty %>% 
  filter(voteChoice == "Republican") %>% 
  group_by(sentiment) %>% 
  summarize(countSentiment = length(sentiment)) %>% 
  ggplot(aes(x = sentiment, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment)) +
  labs(x = "Sentiment",
       y = "Number of Searches",
       title = "Sentiment Distribution: Republicans")

sentimentNRCPlotNon <- sentimentNRCParty %>% 
  filter(voteChoice == "Non Voter") %>% 
  group_by(sentiment) %>% 
  summarize(countSentiment = length(sentiment)) %>% 
  ggplot(aes(x = sentiment, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment))  +
  labs(x = "Sentiment",
       y = "Number of Searches",
       title = "Sentiment Distribution: Non Voters")

# sentiment sentence-level
sentenceLevel <- fullDataSet %>% 
  mutate(search_term = gsub("[[:punct:]]", " ", search_term))

sentences <- get_sentences(sentenceLevel$search_term)
sentiment <- sentiment(sentences)

summary(sentiment$sentiment)
#    Min.   1st Qu.   Median    Mean    3rd Qu.   Max. 
# -1.89835  0.00000  0.00000  0.01123  0.00000  1.50111 

sentimentPlot <- ggplot(data = sentiment, aes(x = sentiment)) +
  geom_histogram(bins = 50)

fullDataSet$sentiment <- sentiment$sentiment

sentenceSentiment <- fullDataSet %>% 
  mutate(sentiment_category = case_when(sentiment > 0 ~ "positive", 
                                        sentiment < 0 ~ "negative",
                                        TRUE ~ "neutral"))


sentimentSentenceParty <- sentenceSentiment %>% 
  select(pmxid, voteChoice, sentiment_category) %>% 
  filter(!is.na(voteChoice) & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 4 ~ "Non Voter")) 

sentimentSentencePlotDem <- sentimentSentenceParty %>% 
  filter(voteChoice == "Democrat") %>% 
  group_by(sentiment_category) %>% 
  summarize(countSentiment = length(sentiment_category)) %>% 
  ggplot(aes(x = sentiment_category, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment_category)) +
  labs(x = "Sentiment",
       y = "Number of Searches",
       title = "Sentiment Distribution: Democrats")

sentimentSentencePlotRep <- sentimentSentenceParty %>% 
  filter(voteChoice == "Republican") %>% 
  group_by(sentiment_category) %>% 
  summarize(countSentiment = length(sentiment_category)) %>% 
  ggplot(aes(x = sentiment_category, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment_category)) +
  labs(x = "Sentiment",
       y = "Number of Searches",
       title = "Sentiment Distribution: Republicans")

sentimentSentencePlotNon <- sentimentSentenceParty %>% 
  filter(voteChoice == "Non Voter") %>% 
  group_by(sentiment_category) %>% 
  summarize(countSentiment = length(sentiment_category)) %>% 
  ggplot(aes(x = sentiment_category, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment_category)) +
  labs(x = "Sentiment",
       y = "Number of Searches",
       title = "Sentiment Distribution: Non Voters")

# by whole block of text
wholeText <- fullDataSet %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(search_term, collapse =" ")) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

joinedText <- merge(wholeText, uniqueUsers)

sentimentWhole <- sentiment(joinedText$text)

summary(sentimentWhole$sentiment)
# Min.    1st Qu.  Median  Mean   3rd Qu. Max. 
#-4.6533  0.0000  0.1960  0.2828  0.5354  5.9522 

sentimentPlotWhole <- ggplot(data = sentimentWhole, aes(x = sentiment)) +
  geom_histogram(bins = 50)

uniqueUsers$sentimentWhole <- sentimentWhole$sentiment

sentimentWhole <- uniqueUsers %>% 
  mutate(sentiment_category = case_when(sentimentWhole > 0 ~ "positive", 
                                        sentimentWhole < 0 ~ "negative",
                                        TRUE ~ "neutral"))


sentenceWholeParty <- sentenceSentiment %>% 
  select(pmxid, voteChoice, sentiment_category) %>% 
  filter(!is.na(voteChoice) & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 4 ~ "Non Voter")) 

sentimentWholePlotDem <- sentenceWholeParty %>% 
  filter(voteChoice == "Democrat") %>% 
  group_by(sentiment_category) %>% 
  summarize(countSentiment = length(sentiment_category)) %>% 
  ggplot(aes(x = sentiment_category, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment_category)) +
  labs(x = "Sentiment of all Queries",
       y = "Number of Users",
       title = "Sentiment Distribution: Democrats")

sentimentWholePlotRep <- sentenceWholeParty %>% 
  filter(voteChoice == "Republican") %>% 
  group_by(sentiment_category) %>% 
  summarize(countSentiment = length(sentiment_category)) %>% 
  ggplot(aes(x = sentiment_category, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment_category)) +
  labs(x = "Sentiment of all Queries",
       y = "Number of Users",
       title = "Sentiment Distribution: Republicans")

sentimentWholePlotNon <- sentenceWholeParty %>% 
  filter(voteChoice == "Non Voter") %>% 
  group_by(sentiment_category) %>% 
  summarize(countSentiment = length(sentiment_category)) %>% 
  ggplot(aes(x = sentiment_category, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment_category)) +
  labs(x = "Sentiment of all Queries",
       y = "Number of Users",
       title = "Sentiment Distribution: Non Voters")

