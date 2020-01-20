library(tidyverse)
library(tidytext)
# library(textdata)
# library(quanteda)
library(reshape2)

# prep dataset 
states <- read.csv("stateMapping.csv", 
                   header = FALSE, 
                   col.names = c("state", "stateName"))

fullDataSet <- merge(fullDataSet, states, 
                     by = "state", 
                     all.x = TRUE)

fullDataSet <- fullDataSet %>% 
  mutate(stateName = as.character(stateName),
         stateName = ifelse(is.na(stateName), "none", stateName))

candidates <- read.csv("candidateInfo.csv", 
                       header = FALSE, 
                       col.names = c("state", "name", "party"))

candidatesPrepped <- candidates %>% 
  group_by(state, party) %>% 
  summarise(candidates  = paste(name, collapse =", ")) %>% 
  dcast(state ~ party) %>% 
  select(state, Democratic, Republican, Other) %>% 
  rename(stateName = state) %>% 
  mutate(stateName = as.character(stateName),
         Democratic = tolower(Democratic),
         Republican = tolower(Republican),
         Other = tolower(Other))

fullDataSet <- left_join(fullDataSet, candidatesPrepped, 
                  by = "stateName", 
                  all.x = TRUE)

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
         searched_register = ifelse(num_register_searches >= 1, 1, 0)) %>% 
  mutate(rep_search = ifelse(word %in% Republican, 1, 0))

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


test3 <- fullDataSet %>% 
  filter(date >= "2018-11-07" & date <= "2018-11-08") %>% 
  unnest_tokens(word, search_term, token = "ngrams", n = 2) %>% 
  unnest_tokens(demCandidate, Democratic, token = "ngrams", n = 2) %>% 
  mutate(serachedDem = ifelse(word == demCandidate, 1, 0))

mean(test3$searchedDem, na.rm = TRUE)


  





# political keywords
politicalWords <- c("vote", "voting", "register_to_vote", "voter_registration", "registration",
                    "election", "midterm", "house_of_representatives", "congressional_candidate",
                    "congress", "candidate", "campaign", "republican", "democrat", "democratic")



# duplicates function
dropDuplicates <- function(x){
  x <- x[!duplicated(x$pmxid), ]
}

