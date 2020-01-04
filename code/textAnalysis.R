library(tidyverse)
library(tidytext)
library(textdata)


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

# most common queries by partisanship
partisanQueries <- searchTokens %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 3 ~ "Independent",
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

partisanQueriesPlotInd <- partisanQueries %>% 
  filter(voteChoice == "Independent") %>% 
  ggplot(aes(x = reorder(word, -n), y = n)) + 
  geom_bar(stat = "identity", aes(fill = word)) +
  labs(x = "Query",
       y = "Number of Searches",
       title = "Top 10 Query Terms: Independents")
  
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
  filter(!is.na(voteChoice)) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 3 ~ "Independent",
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
  filter(!is.na(voteChoice)) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 3 ~ "Independent",
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

sentimentNRCPlotInd <- sentimentNRCParty %>% 
  filter(voteChoice == "Independent") %>% 
  group_by(sentiment) %>% 
  summarize(countSentiment = length(sentiment)) %>% 
  ggplot(aes(x = sentiment, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment))  +
  labs(x = "Sentiment",
       y = "Number of Searches",
       title = "Sentiment Distribution: Independents")

sentimentNRCPlotNon <- sentimentNRCParty %>% 
  filter(voteChoice == "Non Voter") %>% 
  group_by(sentiment) %>% 
  summarize(countSentiment = length(sentiment)) %>% 
  ggplot(aes(x = sentiment, y = countSentiment)) + 
  geom_bar(stat = "identity", aes(fill = sentiment))  +
  labs(x = "Sentiment",
       y = "Number of Searches",
       title = "Sentiment Distribution: Non Voters")

