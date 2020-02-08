library(tidyverse)
library(sentimentr)
library(chron)

# setwd()
load("./data/preppedFullData.RData")

############################
### 'longitudinal' style ###
############################

# all before
sentimentLong <- fullDataSet %>% 
  filter(date <= "2018-11-07") %>% 
  mutate(search_term = gsub("[[:punct:]]", "", search_term)) %>% 
  select(pmxid, date, search_term)

sentences <- get_sentences(sentimentLong$search_term)
sentiment <- sentiment(sentences)

behaviorLongBefore <- fullDataSet %>% 
  filter(date <= "2018-11-07") %>% 
  mutate(time_reg = as.numeric(
    (strptime(time, format = "%H:%M:%S") - 
       strptime("12:00:00", format = "%H:%M:%S")) 
    / 3600),
    search_length = nchar(search_term)) %>% 
  select(pmxid, date, turnout, voteChoice, time_reg, 
         search_engine, search_length)

behaviorLongBefore$sentiment <- sentiment$sentiment

save(behaviorLongBefore, file = "./data/forModels/behaviorLongBefore.RData")

# week before
sentimentLongWeekBefore <- fullDataSet %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  mutate(search_term = gsub("[[:punct:]]", "", search_term)) %>% 
  select(pmxid, date, search_term)

sentences <- get_sentences(sentimentLongWeekBefore$search_term)
sentiment <- sentiment(sentences)

behaviorLongWeekBefore <- fullDataSet %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  mutate(time_reg = as.numeric(
    (strptime(time, format = "%H:%M:%S") - 
       strptime("12:00:00", format = "%H:%M:%S")) 
    / 3600),
    search_length = nchar(search_term)) %>% 
  select(pmxid, date, turnout, voteChoice, time_reg, 
         search_engine, search_length)

behaviorLongWeekBefore$sentiment <- sentiment$sentiment

save(behaviorLongWeekBefore, file = "./data/forModels/behaviorLongWeekBefore.RData")

############################
######### by pmxid #########
############################

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
  summarize(turnout = min(turnout),
            voteChoice = min(voteChoice),
            search_engine = min(search_engine),
            time_of_day = mean(times(time)),
            mean_search_length = mean(nchar(search_term)),
            mean_searches_day = mean(
              length(search_term)
              /length(unique(fullDataSet$date))
             )) %>% 
  mutate(time_reg = as.numeric(
    (strptime(time_of_day, format = "%H:%M:%S") - 
      strptime("12:00:00", format = "%H:%M:%S")) 
      / 3600),
    google = ifelse(search_engine == "Google", 1, 0), 
    bing = ifelse(search_engine == "Bing", 1, 0),
    duck = ifelse(search_engine == "DuckDuckGo", 1, 0),
    yahoo = ifelse(search_engine == "Yahoo", 1, 0),
    other = ifelse(search_engine == "Other", 1, 0)) %>% 
  select(-time_of_day, -search_engine)

behaviorDataSetBefore <- fullDataSet %>% 
  filter(date <= "2018-11-07") %>% 
  group_by(pmxid) %>% 
  summarize(turnout = min(turnout),
            voteChoice = min(voteChoice),
            search_engine = min(search_engine),
            time_of_day = mean(times(time)),
            mean_search_length = mean(nchar(search_term)),
            mean_searches_day = mean(
              length(search_term)
              /length(unique(fullDataSet$date))
            )) %>% 
  mutate(time_reg = as.numeric(
    (strptime(time_of_day, format = "%H:%M:%S") - 
      strptime("12:00:00", format = "%H:%M:%S"))
    / 3600 ),
    google = ifelse(search_engine == "Google", 1, 0), 
    bing = ifelse(search_engine == "Bing", 1, 0),
    duck = ifelse(search_engine == "DuckDuckGo", 1, 0),
    yahoo = ifelse(search_engine == "Yahoo", 1, 0),
    other = ifelse(search_engine == "Other", 1, 0)) %>% 
  select(-time_of_day, -search_engine)

behaviorDataSetWeekBefore <- fullDataSet %>% 
  filter(date <= "2018-11-06" & date >= "2018-10-30") %>% 
  group_by(pmxid) %>% 
  summarize(turnout = min(turnout),
            voteChoice = min(voteChoice),
            search_engine = min(search_engine),
            time_of_day = mean(times(time)),
            mean_search_length = mean(nchar(search_term)),
            mean_searches_day = mean(
              length(search_term)
              /length(unique(fullDataSet$date))
            )) %>% 
  mutate(time_reg = as.numeric(
    (strptime(time_of_day, format = "%H:%M:%S") - 
           strptime("12:00:00", format = "%H:%M:%S")) / 60),
    google = ifelse(search_engine == "Google", 1, 0), 
    bing = ifelse(search_engine == "Bing", 1, 0),
    duck = ifelse(search_engine == "DuckDuckGo", 1, 0),
    yahoo = ifelse(search_engine == "Yahoo", 1, 0),
    other = ifelse(search_engine == "Other", 1, 0)) %>% 
  select(-time_of_day, -search_engine)

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

# saving
save(searchBehaviorFull, file = "data/forModels/searchBehaviorFull.RData")
save(searchBehaviorBefore, file = "data/forModels/searchBehaviorBefore.RData")
save(searchBehaviorWeekBefore, file = "data/forModels/searchBehaviorWeekBefore.RData")



