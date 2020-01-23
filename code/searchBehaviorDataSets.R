library(tidyverse)
library(sentimentr)

# duplicates function
dropDuplicates <- function(x){
  x <- x[!duplicated(x$pmxid), ]
}

# sentiment all searches
usersTextFull <- fullDataSet %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(search_term, collapse =" ")) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

sentimentFull <- sentiment(usersTextFull$text)
usersTextFull$sentiment <- sentimentFull$sentiment

usersTextBefore <- fullDataSet %>% 
  filter(date <= "2018-11-07") %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(search_term, collapse =" ")) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

sentimentBefore <- sentiment(usersTextBefore$text)
usersTextBefore$sentiment <- sentimentBefore$sentiment

usersTextWeekBefore <- fullDataSet %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(search_term, collapse =" ")) %>% 
  mutate(text = gsub("[[:punct:]]", " ", text))

sentimentWeekBefore <- sentiment(usersTextWeekBefore$text)
usersTextWeekBefore$sentiment <- sentimentWeekBefore$sentiment

# search behavior
behaviorDataSetFull <- fullDataSet %>% 
  group_by(pmxid) %>% 
  mutate(mean_search_length = mean(nchar(search_term)),
         mean_searches_day = mean(
           length(search_term)
           /length(unique(fullDataSet$date))
             )) %>% 
  select(pmxid, turnout, voteChoice, search_engine, mean_search_length,
         mean_searches_day) %>% 
  dropDuplicates()

behaviorDataSetBefore <- fullDataSet %>% 
  filter(date <= "2018-11-07") %>% 
  group_by(pmxid) %>% 
  mutate(mean_search_length = mean(nchar(search_term)),
         mean_searches_day = mean(
           length(search_term)
           /length(unique(fullDataSet$date))
         )) %>% 
  select(pmxid, turnout, voteChoice, search_engine, mean_search_length,
         mean_searches_day) %>% 
  dropDuplicates()

behaviorDataSetWeekBefore <- fullDataSet %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  group_by(pmxid) %>% 
  mutate(mean_search_length = mean(nchar(search_term)),
         mean_searches_day = mean(
           length(search_term)
           /length(unique(fullDataSet$date))
         )) %>% 
  select(pmxid, turnout, voteChoice, search_engine, mean_search_length,
         mean_searches_day) %>% 
  dropDuplicates()

# merging
searchBehaviorFull <- merge(x = behaviorDataSetFull, 
                            y = usersTextFull[ , c("pmxid", "sentiment")], 
                            by = "pmxid",
                            all = TRUE)

searchBehaviorBefore <- merge(x = behaviorDataSetBefore, 
                            y = usersTextBefore[ , c("pmxid", "sentiment")], 
                            by = "pmxid",
                            all = TRUE)

searchBehaviorWeekBefore <- merge(x = behaviorDataSetWeekBefore, 
                            y = usersTextWeekBefore[ , c("pmxid", "sentiment")], 
                            by = "pmxid",
                            all = TRUE)
