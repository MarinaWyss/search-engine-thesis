library(tidyverse)
library(tidytext)
# library(textdata)
# library(quanteda)

# filter for time
dropDuplicates <- function(x){
  x <- x[!duplicated(x$pmxid), ]
}

# search data prep
searchTokens <- fullDataSet %>% 
  unnest_tokens(word, search_term) 

'%ni%' <- Negate('%in%')

searchTokens <- searchTokens %>%
  anti_join(get_stopwords()) %>% 
  filter(word %ni% c("http", "https")) %>% 
  mutate(word = gsub("[[:punct:]]", " ", word),
         hasNum = str_extract_all(word, "[[:digit:]]+"),
         wordClean = ifelse(hasNum == "character(0)", word, NA)) %>% 
  filter(!is.na(wordClean)) %>% 
  select(-wordClean, -hasNum)

# vote/register keywords
registerWords <- c("vote", "voting", "register", "voter", "registration",
                   "election", "midterm")

searchesFull <- searchTokens %>% 
  group_by(pmxid) %>% 
  mutate(register_word = ifelse(word %in% registerWords, 1, 0),
         num_register_searches = sum(register_word),
         searched_register = ifelse(num_register_searches >= 1, 1, 0))

searchesBefore <- searchTokens %>% 
  filter(date <= "2018-11-07") %>% 
  group_by(pmxid) %>% 
  mutate(register_word = ifelse(word %in% registerWords, 1, 0),
         num_register_searches = sum(register_word),
         searched_register = ifelse(num_register_searches >= 1, 1, 0)) 

searchesWeekBefore <- searchTokens %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  group_by(pmxid) %>% 
  mutate(register_word = ifelse(word %in% registerWords, 1, 0),
         num_register_searches = sum(register_word),
         searched_register = ifelse(num_register_searches >= 1, 1, 0)) 

searchesWeekAfter <- searchTokens %>% 
  filter(date >= "2018-11-07" & date <= "2018-11-14") %>% 
  group_by(pmxid) %>% 
  mutate(register_word = ifelse(word %in% registerWords, 1, 0),
         num_register_searches = sum(register_word),
         searched_register = ifelse(num_register_searches >= 1, 1, 0))

# candidates
candidates <- read.csv("candidateInfo.csv")
states <- read.csv("stateMapping.csv", header = FALSE)
colnames(states) <- c("state", "stateName")

fullDataSetT <- fullDataSet %>% 
  left_join(fullDataSet, states, by = "state")


# political keywords
politicalWords <- c("vote", "voting", "register_to_vote", "voter_registration", "registration",
                    "election", "midterm", "house_of_representatives", "congressional_candidate",
                    "congress", "candidate", "campaign", "republican", "democrat", "democratic")

