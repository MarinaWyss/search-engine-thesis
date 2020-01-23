library(tidyverse)
library(tidytext)
library(quanteda)
library(reshape2)

# duplicates function
dropDuplicates <- function(x){
  x <- x[!duplicated(x$pmxid), ]
}

# search data prep
searchTokensSingle <- fullDataSet %>% 
  select(pmxid, turnout, voteChoice, date, search_term) %>% 
  unnest_tokens(word, search_term) 

'%ni%' <- Negate('%in%')

searchTokensSingle <- searchTokensSingle %>%
  anti_join(get_stopwords()) %>% 
  filter(word %ni% c("http", "https")) %>% 
  mutate(word = gsub("[[:punct:]]", " ", word),
         hasNum = str_extract_all(word, "[[:digit:]]+"),
         wordClean = ifelse(hasNum == "character(0)", word, NA)) %>% 
  filter(!is.na(wordClean)) %>% 
  select(-wordClean, -hasNum)

# vote/register and political keywords
registerWords <- c("vote", "voting", "register", "voter", "registration",
                   "election", "midterm")

politicalWords <- c("representatives", "congress", "candidate", 
                    "campaign", "republican", "democrat", "democratic",
                    "kavanaugh", "bader", "sotomayor", "kagan", "gorsuch", 
                    "alito")

fullSearches <- searchTokensSingle %>% 
  group_by(pmxid) %>% 
  mutate(register_word = ifelse(word %in% registerWords, 1, 0),
         num_register_searches = sum(register_word),
         searched_register = ifelse(num_register_searches >= 1, 1, 0),
         political_word = ifelse(word %in% politicalWords, 1, 0),
         num_political_searches = sum(political_word),
         searched_politics = ifelse(num_political_searches >= 1, 1, 0)) %>% 
  select(-register_word, -political_word) %>% 
  dropDuplicates()

searchesBefore <- searchTokensSingle %>% 
  filter(date <= "2018-11-07") %>% 
  group_by(pmxid) %>% 
  mutate(register_word = ifelse(word %in% registerWords, 1, 0),
         num_register_searches = sum(register_word),
         searched_register = ifelse(num_register_searches >= 1, 1, 0),
         political_word = ifelse(word %in% politicalWords, 1, 0),
         num_political_searches = sum(political_word),
         searched_politics = ifelse(num_political_searches >= 1, 1, 0)) %>% 
  select(-register_word, -political_word) %>% 
  dropDuplicates()

searchesWeekBefore <- searchTokensSingle %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  group_by(pmxid) %>% 
  mutate(register_word = ifelse(word %in% registerWords, 1, 0),
         num_register_searches = sum(register_word),
         searched_register = ifelse(num_register_searches >= 1, 1, 0),
         political_word = ifelse(word %in% politicalWords, 1, 0),
         num_political_searches = sum(political_word),
         searched_politics = ifelse(num_political_searches >= 1, 1, 0)) %>% 
  select(-register_word, -political_word) %>% 
  dropDuplicates()

# candidates
states <- read.csv("stateMapping.csv", 
                   header = FALSE, 
                   col.names = c("state", "stateName"))

stateDataSet <- merge(fullDataSet, states, 
                     by = "state", 
                     all.x = TRUE)

stateDataSet <- stateDataSet %>% 
  select(pmxid, turnout, voteChoice, date, search_term, stateName) %>% 
  mutate(stateName = as.character(stateName),
         stateName = ifelse(is.na(stateName), "none", stateName))

candidates <- read.csv("candidateInfoBiTriGram.csv", 
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

candidateDataSet <- left_join(stateDataSet, candidatesPrepped, 
                              by = "stateName", 
                              all.x = TRUE)

candidateTokens <- candidateDataSet %>% 
  unnest_tokens(word, search_term, 
                token = "ngrams",
                n = 2) %>% 
  anti_join(get_stopwords()) %>% 
  mutate(word = str_replace(word, " ", "_")) %>% 
  filter(nchar(word) >= 8) 
  
candidateStateFull <- candidateTokens %>% 
  mutate(dem_candidate_search = ifelse(str_detect(Democratic, word), 1, 0),
         rep_candidate_search = ifelse(str_detect(Republican, word), 1, 0),
         other_candidate_search = ifelse(str_detect(Other, word), 1, 0)) %>% 
  group_by(pmxid) %>% 
  mutate(searched_dem_candidate = ifelse(sum(dem_candidate_search) >= 1, 1, 0),
         searched_rep_candidate = ifelse(sum(rep_candidate_search) >= 1, 1, 0),
         searched_other_candidate = ifelse(sum(other_candidate_search) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem_candidate, searched_rep_candidate, searched_other_candidate) %>% 
  dropDuplicates()

candidateStateBefore <- candidateTokens %>% 
  filter(date <= "2018-11-07") %>% 
  mutate(dem_candidate_search = ifelse(str_detect(Democratic, word), 1, 0),
         rep_candidate_search = ifelse(str_detect(Republican, word), 1, 0),
         other_candidate_search = ifelse(str_detect(Other, word), 1, 0)) %>% 
  group_by(pmxid) %>% 
  mutate(searched_dem_candidate = ifelse(sum(dem_candidate_search) >= 1, 1, 0),
         searched_rep_candidate = ifelse(sum(rep_candidate_search) >= 1, 1, 0),
         searched_other_candidate = ifelse(sum(other_candidate_search) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem_candidate, searched_rep_candidate, searched_other_candidate) %>% 
  dropDuplicates()

candidateStateWeekBefore <- candidateTokens %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  mutate(dem_candidate_search = ifelse(str_detect(Democratic, word), 1, 0),
         rep_candidate_search = ifelse(str_detect(Republican, word), 1, 0),
         other_candidate_search = ifelse(str_detect(Other, word), 1, 0)) %>% 
  group_by(pmxid) %>% 
  mutate(searched_dem_candidate = ifelse(sum(dem_candidate_search) >= 1, 1, 0),
         searched_rep_candidate = ifelse(sum(rep_candidate_search) >= 1, 1, 0),
         searched_other_candidate = ifelse(sum(other_candidate_search) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem_candidate, searched_rep_candidate, searched_other_candidate) %>% 
  dropDuplicates()

# partisan politicians
politicians <- read.csv("politicalFigures.csv", 
                        col.names = c("name", "party"))

politicians <- politicians %>% 
  filter(party != "A") %>% 
  mutate(name = tolower(name),
         name = str_replace(name, " ", "_"))

dems <- politicians %>% 
  filter(party == "D") %>% 
  select(name) %>% 
  pull() %>% 
  str_trim()

reps <- politicians %>% 
  filter(party == "R") %>% 
  select(name) %>% 
  pull() %>% 
  str_trim()

politicianTokens <- fullDataSet %>% 
  unnest_tokens(word, search_term, 
                token = "ngrams",
                n = 2) %>% 
  anti_join(get_stopwords()) %>% 
  mutate(word = str_replace(word, " ", "_"))

politiciansFull <- politicianTokens %>% 
  group_by(pmxid) %>% 
  mutate(dem_word = ifelse(word %in% dems, 1, 0),
         num_dem_searches = sum(dem_word),
         searched_dem = ifelse(sum(dem_word) >= 1, 1, 0),
         rep_word = ifelse(word %in% reps, 1, 0),
         num_rep_searches = sum(rep_word),
         searched_rep = ifelse(sum(rep_word) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem, num_dem_searches, 
         searched_rep, num_rep_searches) %>% 
  dropDuplicates()

politiciansBefore <- politicianTokens %>% 
  filter(date <= "2018-11-07") %>% 
  group_by(pmxid) %>% 
  mutate(dem_word = ifelse(word %in% dems, 1, 0),
         num_dem_searches = sum(dem_word),
         searched_dem = ifelse(sum(dem_word) >= 1, 1, 0),
         rep_word = ifelse(word %in% reps, 1, 0),
         num_rep_searches = sum(rep_word),
         searched_rep = ifelse(sum(rep_word) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem, num_dem_searches, 
         searched_rep, num_rep_searches) %>% 
  dropDuplicates()

politiciansWeekBefore <- politicianTokens %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  group_by(pmxid) %>% 
  mutate(dem_word = ifelse(word %in% dems, 1, 0),
         num_dem_searches = sum(dem_word),
         searched_dem = ifelse(sum(dem_word) >= 1, 1, 0),
         rep_word = ifelse(word %in% reps, 1, 0),
         num_rep_searches = sum(rep_word),
         searched_rep = ifelse(sum(rep_word) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem, num_dem_searches, 
         searched_rep, num_rep_searches) %>% 
  dropDuplicates()

# joining datasets
fullSearchesJoined <- merge(fullSearches, candidateStateFull, 
                            by = "pmxid",
                            all = TRUE)

fullSearchesJoined <- merge(fullSearchesJoined, politiciansFull, 
                            by = "pmxid",
                            all = TRUE)

beforeSearchesJoined <- merge(searchesBefore, candidateStateBefore, 
                              by = "pmxid",
                              all = TRUE)

beforeSearchesJoined <- merge(beforeSearchesJoined, politiciansBefore, 
                              by = "pmxid",
                              all = TRUE)

weekBeforeSearchesJoined <- merge(searchesWeekBefore, candidateStateWeekBefore, 
                                  by = "pmxid",
                                  all = TRUE)

weekBeforeSearchesJoined <- merge(weekBeforeSearchesJoined, politiciansWeekBefore, 
                                  by = "pmxid",
                                  all = TRUE)

# all search terms
fullText <- fullDataSet %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(search_term, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice)) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

fullCorpus <- corpus(fullText)
fullDFM <- dfm(fullCorpus)
fullDFM <- dfm_trim(fullDFM, min_termfreq = 3)

beforeText <- fullDataSet %>% 
  filter(date <= "2018-11-07") %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(search_term, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice)) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

beforeCorpus <- corpus(beforeText)
beforeDFM <- dfm(beforeCorpus)
beforeDFM <- dfm_trim(beforeDFM, min_termfreq = 3)

weekBeforeText <- fullDataSet %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(search_term, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice)) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

weekBeforeCorpus <- corpus(weekBeforeText)
weekBeforeDFM <- dfm(weekBeforeCorpus)
weekBeforeDFM <- dfm_trim(weekBeforeDFM, min_termfreq = 3)


