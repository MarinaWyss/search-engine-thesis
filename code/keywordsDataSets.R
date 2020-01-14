library(tidyverse)
library(tidytext)
# library(textdata)
# library(quanteda)

# filter for time
selectDates <- function(x){
  x <- x[!duplicated(x$pmxid), ]
}

uniqueUsersFull <- fullDataSet[!duplicated(fullDataSet$pmxid), ]

beforeElection <- fullDataSet %>% 
  filter(date <= "2018-11-07")
uniqueUsersBeforeElection <- selectDates(beforeElection)

weekBeforeElection <- fullDataSet %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30")
uniqueUsersWeekBefore <- selectDates(weekBeforeElection)

weekAfterElection <- fullDataSet %>% 
  filter(date >= "2018-11-07" & date <= "2018-11-14")
uniqueUsersWeekAfter <- selectDates(weekAfterElection)

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

searchedRegister <- searchTokens %>% 
  group_by(pmxid) %>% 
  mutate(searched_register = ifelse(word %in% registerWords, 1, 0)) %>% 
  summarise(num_register_searches = sum(searched_register))

registerDataSetFull <- uniqueUsersFull %>% 
  merge(searchedRegister, by = "pmxid") %>% 
  mutate(searched_register = ifelse(num_register_searches >= 1, 1, 0)) %>% 
  select(pmxid, turnout, voteChoice, searched_register, num_register_searches)

registerDataSetBefore <- uniqueUsersBeforeElection %>% 
  merge(searchedRegister, by = "pmxid") %>% 
  mutate(searched_register = ifelse(num_register_searches >= 1, 1, 0)) %>% 
  select(pmxid, turnout, voteChoice, searched_register, num_register_searches)

registerDataSetWeekBefore <- uniqueUsersWeekBefore %>% 
  merge(searchedRegister, by = "pmxid") %>% 
  mutate(searched_register = ifelse(num_register_searches >= 1, 1, 0)) %>% 
  select(pmxid, turnout, voteChoice, searched_register, num_register_searches)

registerDataSetWeekAfter <- uniqueUsersWeekAfter %>% 
  merge(searchedRegister, by = "pmxid") %>% 
  mutate(searched_register = ifelse(num_register_searches >= 1, 1, 0)) %>% 
  select(pmxid, turnout, voteChoice, searched_register, num_register_searches)


# political keywords
politicalWords <- c("vote", "voting", "register_to_vote", "voter_registration", "registration",
                    "election", "midterm", "house_of_representatives", "congressional_candidate",
                    "congress", "candidate", "campaign", "republican", "democrat", "democratic")

