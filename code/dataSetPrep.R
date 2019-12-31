library(tidyverse)

# load data
searchData <- search_df
surveyData <- all_waves

#### SEARCH ENGINE DATA ####

# dropping rows without search term
searchData <- searchData %>% 
  filter(!is.na(search_term))

# dropping sequential duplicate search terms
searchData <- searchData %>% 
  group_by(pmxid, date) %>% 
  arrange(date) %>% 
  mutate(url_row = rep(cumsum(rle(search_term)$lengths), rle(search_term)$lengths),
         url_seq = 1:n()) %>% 
  ungroup() %>% 
  mutate(duplicate = ifelse(url_row != url_seq, 1, 0)) %>% 
  filter(duplicate == 0) %>% 
  select(-duplicate, -url_row, -url_seq)    

# keeping only necessary fields
searchData <- searchData %>% 
  select(pmxid, date, time, page_domain, 
         predecessor_url, succesor_url, search_term)

# search engine name vs URL
searchData <- searchData %>% 
  mutate(search_engine = case_when(grepl("google", page_domain) ~ "Google",
                                   grepl("yahoo", page_domain) ~ "Yahoo",
                                   grepl("bing", page_domain) ~ "Bing",
                                   grepl("duck", page_domain) ~ "DuckDuckGo",
                                   grepl("aol", page_domain) ~ "AOL",
                                   grepl("ask", page_domain) ~ "Ask",
                                   TRUE ~ "Other")) %>% 
  select(-page_domain)

# save
save(searchData, file = "preppedSearchData.RData")

#### SURVEY DATA ####

# select relevant variables
surveyData <- surveyData %>% 
  select(identity, W5_PATA506, W5_PATA507,
         birthyr, gender, race, educ,
         marstat, employ, faminc_new, pid3,
         pid7, inputstate, votereg, ideo5, 
         presvote16post, religpew) %>% 
  rename(pmxid = identity,
         turnout = W5_PATA506,
         voteChoice = W5_PATA507,
         familyIncome = faminc_new,
         state = inputstate,
         religion = religpew)

# drop NAs on target variables
surveyData <- surveyData %>% 
  filter(!is.na(turnout))

# turnout to binary
surveyData <- surveyData %>% 
  mutate(turnout = case_when(turnout == 5 ~ 1,
                             TRUE ~ 0))

# save
save(surveyData, file = "preppedSurveyData.RData")

#### EXTERNAL DATA ####
candidateInfo <- read.csv("candidateInfo.csv")

registrationDeadlines <- read.csv("registrationDeadlines.csv")
registrationDeadlines <- registrationDeadlines %>% 
  mutate(deadlineDate = as.Date("2018-11-06") - inPersonDeadline)

#### MERGE #### 
# doesn't yet include external data
fullDataSet <- inner_join(surveyData, searchData)
## length(unique(fullDataSet$pmxid)) - 708 unique users

# save
save(fullDataSet, file = "preppedFullData.RData")




