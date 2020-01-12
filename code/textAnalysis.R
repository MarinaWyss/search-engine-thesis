library(tidyverse)
library(tidytext)
library(textdata)
library(sentimentr)

#beforeElection <- fullDataSet %>% 
#  filter(date <= "2018-11-07")

# tokens
searchTokens <- fullDataSet %>% 
  unnest_tokens(word, search_term) 

# remove stopwords
'%ni%' <- Negate('%in%')

searchTokens <- searchTokens %>%
  anti_join(get_stopwords()) %>% 
  filter(word %ni% c("http", "https"))

# most common queries
mostCommon <- searchTokens %>% 
  count(word, sort = TRUE)

mostCommonClean <- mostCommon %>% 
  mutate(word = gsub("[[:punct:]]", " ", word),
         hasNum = str_extract_all(word, "[[:digit:]]+"),
         wordClean = ifelse(hasNum == "character(0)", word, NA)) %>% 
  filter(!is.na(wordClean)) %>% 
  select(-wordClean, -hasNum)

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

# keywords
politicalWords <- c("vote", "voting", "register_to_vote", "voter_registration", "registration",
                    "election", "midterm", "house_of_representatives", "congressional_candidate",
                    "congress", "candidate", "campaign", "republican", "democrat", "democratic")

