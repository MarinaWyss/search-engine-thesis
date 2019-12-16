library(tidyverse)

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
  select(identity, 
         W4_PATA403, 
         W4_Facebook_user,
         W4_PATA492a,
         race,
         birthyr,
         gender,
         educ,
         employ,
         faminc_new,
         pid3,
         votereg,
         newsint,
         presvote16post)

withoutDataSurvey <- merge(surveyData, withoutData, by = "identity")
withDataSurvey <- merge(surveyData, withData, by = "identity")

summary(withoutDataSurvey)
summary(withDataSurvey)



