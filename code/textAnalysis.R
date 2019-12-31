library(tidyverse)
library(tidytext)

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




# sentiment test
bing <- get_sentiments("bing")

sentiment <- searchTokens %>%
  inner_join(bing)