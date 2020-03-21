library(tidyverse)
library(tidytext)
library(quanteda)
library(reshape2)

# setwd()
source("./code/R/00_functions.R")
load("./data/preppedFullData.RData")

# search data prep
'%ni%' <- Negate('%in%')

fullDataSet <- fullDataSet %>% 
  mutate(search_id = row_number())

searchTokensSingle <- fullDataSet %>% 
  select(pmxid, turnout, voteChoice, date, search_term, search_id) %>% 
  unnest_tokens(word, search_term) %>% 
  anti_join(get_stopwords()) %>% 
  filter(word %ni% c("http", "https")) %>% 
  mutate(hasNum = str_extract_all(word, "[[:digit:]]+"),
         wordClean = ifelse(hasNum == "character(0)", word, NA)) %>% 
  filter(!is.na(wordClean)) %>% 
  select(-wordClean, -hasNum) 

############################
########## keywords ########
############################

# vote/register and political keywords
registerWords <- c("vote", "voting", "register", "voter", "registration",
                   "election", "midterm", "#electionday", "#registertovote",
                   "primary", "absentee", "polling", "ballot")

politicalWords <- c("representatives", "representative", "congress", "candidate", 
                    "campaign", "republican", "democrat", "democratic",
                    "kavanaugh", "bader", "sotomayor", "kagan", "gorsuch", 
                    "alito", "government", "bipartisan", "caucus", "convention",
                    "delegate", "filibuster", "gerrymander", "gerrymandering",
                    "gop", "incumbent", "politics", "political",  
                    "nominee", "nomination", "poll", "constituent", 
                    "consituency", "democracy", "electoral", "federal", 
                    "gubernatorial", "senate", "veto", "ratified",
                    "constitution", "constitutional", "amendment", "ballot", "mayor", 
                    "governor", "president", "senator", "congressman",
                    "congresswoman", "congressional", "gop", "inauguration",
                    "libertarian", "lobbyist", "legislation", "electorate", 
                    "referendum", "libertarianism", "communism", "communist",
                    "socialism", "socialist")

fullSearches <- searchTokensSingle %>% 
  group_by(pmxid) %>% 
  mutate(register_word = ifelse(word %in% registerWords, 1, 0),
         num_register_searches = sum(register_word, na.rm = TRUE),
         searched_register = ifelse(num_register_searches >= 1, 1, 0),
         political_word = ifelse(word %in% politicalWords, 1, 0),
         num_political_searches = sum(political_word, na.rm = TRUE),
         searched_politics = ifelse(num_political_searches >= 1, 1, 0)) %>% 
  select(-register_word, -political_word) %>% 
  dropDuplicates()

searchesBefore <- searchTokensSingle %>% 
  filter(date <= "2018-11-07") %>% 
  group_by(pmxid) %>% 
  mutate(register_word = ifelse(word %in% registerWords, 1, 0),
         num_register_searches = sum(register_word, na.rm = TRUE),
         searched_register = ifelse(num_register_searches >= 1, 1, 0),
         political_word = ifelse(word %in% politicalWords, 1, 0),
         num_political_searches = sum(political_word, na.rm = TRUE),
         searched_politics = ifelse(num_political_searches >= 1, 1, 0)) %>% 
  select(-register_word, -political_word) %>% 
  dropDuplicates()

searchesWeekBefore <- searchTokensSingle %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  group_by(pmxid) %>% 
  mutate(register_word = ifelse(word %in% registerWords, 1, 0),
         num_register_searches = sum(register_word, na.rm = TRUE),
         searched_register = ifelse(num_register_searches >= 1, 1, 0),
         political_word = ifelse(word %in% politicalWords, 1, 0),
         num_political_searches = sum(political_word, na.rm = TRUE),
         searched_politics = ifelse(num_political_searches >= 1, 1, 0)) %>% 
  select(-register_word, -political_word) %>% 
  dropDuplicates()

# candidates
states <- read.csv("./data/external/stateMapping.csv", 
                   header = FALSE, 
                   col.names = c("state", "stateName"))

stateDataSet <- merge(fullDataSet, states, 
                     by = "state", 
                     all.x = TRUE)

stateDataSet <- stateDataSet %>% 
  select(pmxid, turnout, voteChoice, date, search_term, stateName) %>% 
  mutate(stateName = as.character(stateName),
         stateName = ifelse(is.na(stateName), "none", stateName))

candidates <- read.csv("./data/external/candidateInfoBiTriGram.csv", 
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
  mutate(searched_dem_candidate = ifelse(sum(dem_candidate_search, na.rm = TRUE) >= 1, 1, 0),
         searched_rep_candidate = ifelse(sum(rep_candidate_search, na.rm = TRUE) >= 1, 1, 0),
         searched_other_candidate = ifelse(sum(other_candidate_search, na.rm = TRUE) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem_candidate, searched_rep_candidate, searched_other_candidate) %>% 
  dropDuplicates()

candidateStateBefore <- candidateTokens %>% 
  filter(date <= "2018-11-07") %>% 
  mutate(dem_candidate_search = ifelse(str_detect(Democratic, word), 1, 0),
         rep_candidate_search = ifelse(str_detect(Republican, word), 1, 0),
         other_candidate_search = ifelse(str_detect(Other, word), 1, 0)) %>% 
  group_by(pmxid) %>% 
  mutate(searched_dem_candidate = ifelse(sum(dem_candidate_search, na.rm = TRUE) >= 1, 1, 0),
         searched_rep_candidate = ifelse(sum(rep_candidate_search, na.rm = TRUE) >= 1, 1, 0),
         searched_other_candidate = ifelse(sum(other_candidate_search, na.rm = TRUE) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem_candidate, searched_rep_candidate, searched_other_candidate) %>% 
  dropDuplicates()

candidateStateWeekBefore <- candidateTokens %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  mutate(dem_candidate_search = ifelse(str_detect(Democratic, word), 1, 0),
         rep_candidate_search = ifelse(str_detect(Republican, word), 1, 0),
         other_candidate_search = ifelse(str_detect(Other, word), 1, 0)) %>% 
  group_by(pmxid) %>% 
  mutate(searched_dem_candidate = ifelse(sum(dem_candidate_search, na.rm = TRUE) >= 1, 1, 0),
         searched_rep_candidate = ifelse(sum(rep_candidate_search, na.rm = TRUE) >= 1, 1, 0),
         searched_other_candidate = ifelse(sum(other_candidate_search, na.rm = TRUE) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem_candidate, searched_rep_candidate, searched_other_candidate) %>% 
  dropDuplicates()

# partisan politicians
politicians <- read.csv("./data/external/politicalFigures.csv", 
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
         num_dem_searches = sum(dem_word, na.rm = TRUE),
         searched_dem = ifelse(sum(dem_word, na.rm = TRUE) >= 1, 1, 0),
         rep_word = ifelse(word %in% reps, 1, 0),
         num_rep_searches = sum(rep_word, na.rm = TRUE),
         searched_rep = ifelse(sum(rep_word, na.rm = TRUE) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem, num_dem_searches, 
         searched_rep, num_rep_searches) %>% 
  dropDuplicates()

politiciansBefore <- politicianTokens %>% 
  filter(date <= "2018-11-07") %>% 
  group_by(pmxid) %>% 
  mutate(dem_word = ifelse(word %in% dems, 1, 0),
         num_dem_searches = sum(dem_word, na.rm = TRUE),
         searched_dem = ifelse(sum(dem_word, na.rm = TRUE) >= 1, 1, 0),
         rep_word = ifelse(word %in% reps, 1, 0),
         num_rep_searches = sum(rep_word, na.rm = TRUE),
         searched_rep = ifelse(sum(rep_word, na.rm = TRUE) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem, num_dem_searches, 
         searched_rep, num_rep_searches) %>% 
  dropDuplicates()

politiciansWeekBefore <- politicianTokens %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  group_by(pmxid) %>% 
  mutate(dem_word = ifelse(word %in% dems, 1, 0),
         num_dem_searches = sum(dem_word, na.rm = TRUE),
         searched_dem = ifelse(sum(dem_word, na.rm = TRUE) >= 1, 1, 0),
         rep_word = ifelse(word %in% reps, 1, 0),
         num_rep_searches = sum(rep_word, na.rm = TRUE),
         searched_rep = ifelse(sum(rep_word, na.rm = TRUE) >= 1, 1, 0)) %>% 
  select(pmxid, searched_dem, num_dem_searches, 
         searched_rep, num_rep_searches) %>% 
  dropDuplicates()

# joining datasets
fullSearchesJoined <- merge(fullSearches, candidateStateFull, 
                            by = "pmxid",
                            all = TRUE)

fullSearchesJoined <- merge(fullSearchesJoined, politiciansFull, 
                            by = "pmxid",
                            all = TRUE)%>% 
  select(-date, -word)

beforeSearchesJoined <- merge(searchesBefore, candidateStateBefore, 
                              by = "pmxid",
                              all = TRUE)

beforeSearchesJoined <- merge(beforeSearchesJoined, politiciansBefore, 
                              by = "pmxid",
                              all = TRUE)  %>% 
  select(-date, -word)

weekBeforeSearchesJoined <- merge(searchesWeekBefore, candidateStateWeekBefore, 
                                  by = "pmxid",
                                  all = TRUE)

weekBeforeSearchesJoined <- merge(weekBeforeSearchesJoined, politiciansWeekBefore, 
                                  by = "pmxid",
                                  all = TRUE) %>% 
  select(-date, -word)

# saving
save(fullSearchesJoined, file = "data/forModels/fullSearchesJoined.RData")
save(beforeSearchesJoined, file = "data/forModels/beforeSearchesJoined.RData")
save(weekBeforeSearchesJoined, file = "data/forModels/weekBeforeSearchesJoined.RData")


############################
##### all search terms #####
############################

fullText <- searchTokensSingle %>% 
  mutate(stemmed = char_wordstem(word)) %>%
  filter(nchar(stemmed) >= 2) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(stemmed, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice)) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text)) 

fullCorpus <- corpus(fullText)
fullDFM <- dfm(fullCorpus)
fullDFM <- dfm_trim(fullDFM, min_termfreq = 3)

beforeText <- searchTokensSingle %>% 
  filter(date <= "2018-11-07") %>% 
  mutate(stemmed = char_wordstem(word)) %>% 
  filter(nchar(stemmed) >= 2) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(stemmed, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice)) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

beforeCorpus <- corpus(beforeText)
beforeDFM <- dfm(beforeCorpus)
beforeDFM <- dfm_trim(beforeDFM, min_termfreq = 3)

weekBeforeText <- searchTokensSingle %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  mutate(stemmed = char_wordstem(word)) %>% 
  filter(nchar(stemmed) >= 2) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(stemmed, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice)) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

weekBeforeCorpus <- corpus(weekBeforeText)
weekBeforeDFM <- dfm(weekBeforeCorpus)
weekBeforeDFM <- dfm_trim(weekBeforeDFM, min_termfreq = 3)

# saving
save(fullDFM, file = "data/forModels/fullDFM.RData")
save(beforeDFM, file = "data/forModels/beforeDFM.RData")
save(weekBeforeDFM, file = "data/forModels/weekBeforeDFM.RData")


# top 1000 search terms
top1000Before <- searchTokensSingle %>% 
  filter(date <= "2018-11-07") %>% 
  mutate(stemmed = char_wordstem(word)) %>% 
  filter(nchar(stemmed) >= 2) %>% 
  group_by(stemmed) %>% 
  mutate(num_searches = n()) %>% 
  filter(num_searches > 100) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(stemmed, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice)) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

top1000BeforeCorpus <- corpus(top1000Before)
top1000BeforeDFM <- dfm(top1000BeforeCorpus)

top1000WeekBefore <- searchTokensSingle %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  mutate(stemmed = char_wordstem(word)) %>% 
  filter(nchar(stemmed) >= 2) %>% 
  group_by(stemmed) %>% 
  mutate(num_searches = n()) %>% 
  filter(num_searches > 8) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(stemmed, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice)) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

top1000WeekBeforeCorpus <- corpus(top1000WeekBefore)
top1000WeekBeforeDFM <- dfm(top1000WeekBeforeCorpus)

# saving
save(top1000BeforeDFM, file = "data/forModels/top1000BeforeDFM.RData")
save(top1000WeekBeforeDFM, file = "data/forModels/top1000WeekBeforeDFM.RData")


# top 1000 bigrams
bigramsBefore <- fullDataSet %>% 
  filter(date <= "2018-11-07") %>% 
  unnest_tokens(word, search_term, 
                token = "ngrams",
                n = 2) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!is.na(word)) %>% 
  mutate(word = str_replace(word, " ", "_")) %>% 
  group_by(word) %>% 
  mutate(num_searches = n()) %>% 
  filter(num_searches >= 29) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(word, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice))

bigramsBeforeCorpus <- corpus(bigramsBefore)
bigramsBeforeDFM <- dfm(bigramsBeforeCorpus)


bigramsWeekBefore <- fullDataSet %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  unnest_tokens(word, search_term, 
                token = "ngrams",
                n = 2) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!is.na(word)) %>% 
  mutate(word = str_replace(word, " ", "_")) %>% 
  group_by(word) %>% 
  mutate(num_searches = n()) %>% 
  filter(num_searches >= 5) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(word, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice))

bigramsWeekBeforeCorpus <- corpus(bigramsWeekBefore)
bigramsWeekBeforeDFM <- dfm(bigramsWeekBeforeCorpus)

# saving
save(bigramsBeforeDFM, file = "data/forModels/bigramsBeforeDFM.RData")
save(bigramsWeekBeforeDFM, file = "data/forModels/bigramsWeekBeforeDFM.RData")


#############################
## full political searches ##
#############################
demCandidates <- candidates %>% 
  filter(party == "Democratic") %>% 
  mutate(name = tolower(name))
demCandidates <- as.vector(demCandidates[ ,2])

repCandidates <- candidates %>% 
  filter(party == "Republican") %>% 
  mutate(name = tolower(name))
repCandidates <- as.vector(repCandidates[ ,2])

politicalSearches <- searchTokensSingle %>% 
  mutate(
    register_word = ifelse(word %in% registerWords, 1, 0),
    political_word = ifelse(word %in% politicalWords, 1, 0)
  )

politicianSearches <- fullDataSet %>% 
  unnest_tokens(word, search_term, 
                token = "ngrams",
                n = 2) %>% 
  anti_join(get_stopwords()) %>% 
  mutate(word = str_replace(word, " ", "_")) %>% 
  mutate(
    dem = ifelse(word %in% dems, 1, 0),
    rep = ifelse(word %in% reps, 1, 0),
    dem_cand = ifelse(word %in% demCandidates, 1, 0),
    rep_cand = ifelse(word %in% repCandidates, 1, 0)
  )

searchIdsPolitical <- politicalSearches %>% 
  group_by(search_id) %>% 
  filter(sum(register_word, political_word) >= 1) %>% 
  select(search_id) %>% 
  unique()

searchIdsPoliticians <- politicianSearches %>% 
  group_by(search_id) %>% 
  filter(sum(dem, rep, dem_cand, rep_cand) >= 1) %>% 
  select(search_id) %>% 
  unique()

searchIds<- rbind(searchIdsPolitical, searchIdsPoliticians)
searchIds <- as.vector(unlist(searchIds))

beforeTextPolitical <- fullDataSet %>% 
  filter(date <= "2018-11-07") %>% 
  filter(search_id %in% searchIds) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(search_term, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice)) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text)) 

beforeCorpusPolitical <- corpus(beforeTextPolitical)
beforeDFMPolitical <- dfm(beforeCorpusPolitical)
beforeDFMPolitical <- dfm_trim(beforeDFMPolitical, min_termfreq = 3)

weekBeforeTextPolitical <- fullDataSet %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  filter(search_id %in% searchIds) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(search_term, collapse =" "),
            turnout = min(turnout),
            voteChoice = min(voteChoice)) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text)) 

weekBeforeCorpusPolitical <- corpus(weekBeforeTextPolitical)
weekBeforeDFMPolitical <- dfm(weekBeforeCorpusPolitical)
weekBeforeDFMPolitical <- dfm_trim(weekBeforeDFMPolitical, min_termfreq = 3)

save(beforeDFMPolitical, file = "data/forModels/beforeDFMPolitical.RData")
save(weekBeforeDFMPolitical, file = "data/forModels/weekBeforeDFMPolitical.RData")

