library(tidyverse)
library(kableExtra)

searchData <- search_df
surveyData <- all_waves

# dropping rows without search term
searchData <- searchData %>% 
  filter(!is.na(search_term))

# 913 unique users over entire timespan
length(unique(searchData$pmxid)) 


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


# three months before election
searchDataShort <- searchData %>% 
  filter(date >= "2018-08-01" & date <= "2018-11-07")

length(unique(searchDataShort$pmxid)) # 825 unique users for three months pre-election

searchesPerUser <- searchDataShort %>% # mean 278 searches per user, median 76
  group_by(pmxid) %>% 
  summarize(numberSearches = n())

mean(searchesPerUser$numberSearches)
median(searchesPerUser$numberSearches)

hist(searchesPerUser$numberSearches)

# just a week before election
searchDataShort <- searchData %>% 
  filter(date >= "2018-10-31" & date <= "2018-11-07")

length(unique(searchDataShort$pmxid)) # 375 unique users for one week pre-election

searchesPerUser <- searchDataShort %>% # mean 45 searches per user, median 20
  group_by(pmxid) %>% 
  summarize(numberSearches = n())

mean(searchesPerUser$numberSearches)
median(searchesPerUser$numberSearches)

hist(searchesPerUser$numberSearches) # less right skewed than full data set


# with vs. without full URLs
withList <- unique(searchData$pmxid)
fullList <- unique(search_df$pmxid)
withoutList <- fullList[!fullList %in% withList]

withData <- search_df[search_df$pmxid %in% withList, ]
withoutData <- search_df[search_df$pmxid %in% withoutList, ]

withData <- withData %>% 
  rename(identity = pmxid)

withoutData <- withoutData %>% 
  rename(identity = pmxid)

surveyData <- surveyData %>% 
  select(identity, W5_PATA506, W5_PATA507,
         votereg, birthyr, gender, educ,
         faminc_new, ideo5) %>% 
  rename(turnout = W5_PATA506,
         voteChoice = W5_PATA507,
         familyIncome = faminc_new) %>% 
  mutate(turnout = case_when(turnout == 5 ~ 1,
                             TRUE ~ 0),
         voteChoice = case_when(voteChoice == 1 ~ 0,
                                voteChoice == 2 ~ 1,
                                TRUE ~ NA_real_),
         gender = ifelse(gender == 1, 0, 1),
         votereg = ifelse(votereg == 1, 1, 0),
         familyIncome = ifelse(familyIncome == 97, NA, familyIncome)) %>% 
  type.convert()

withoutDataSurvey <- merge(surveyData, withoutData, by = "identity")
withDataSurvey <- merge(surveyData, withData, by = "identity")

withoutDataSurvey <- withoutDataSurvey %>% 
  select(-client_id, -client_key, -os_name,
         -os_version, -device_type, -session_start_time,
         -start_time_utc, -date, -time, - page_url,
         -page_domain, -predecessor_url, -succesor_url,
         -browser_vendor, -search_term, -Category, -page_duration, 
         -identity)

withDataSurvey <- withDataSurvey %>% 
  select(-client_id, -client_key, -os_name,
         -os_version, -device_type, -session_start_time,
         -start_time_utc, -date, -time, - page_url,
         -page_domain, -predecessor_url, -succesor_url,
         -browser_vendor, -search_term, -Category, -page_duration,
         -identity)

withoutDataSurveyTable <- sapply(withoutDataSurvey, mean, na.rm = TRUE)
withDataSurveyTable <- sapply(withDataSurvey, mean, na.rm = TRUE)

summaryTable <- data.frame(withDataSurveyTable, withoutDataSurveyTable)

colnames(summaryTable) <- c("With Full URLs", "Without Full URLs") 

summaryTable <- round(summaryTable, 2)

rownames(summaryTable) <- c("Turnout (0 no, 1 yes)", 
                            "Vote 2018 (0 Rep, 1 Dem)",
                            "Registered Voter (0 no, 1 yes)",
                            "Birth Year",
                            "Gender (0 male, 1 female)",
                            "Education (from 1-High School to 6-Postgrad)",
                            "Family income (from 1-under 10k to 16-over 500k)",
                            "Ideology (from 1-very Liberal - 5-very Conservative)")

summaryTableKable <- summaryTable %>% 
  kable() %>% 
  kable_styling()
