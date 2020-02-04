library(tidyverse)
library(kableExtra)
library(caret)
library(stargazer)

searchData <- search_df
surveyData <- all_waves

# dates of survey
date <- surveyData %>% select(W5_starttime) # 12/20/2018 - 01/07/2019

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
searchData <- searchData %>% 
  filter(!is.na(search_term))

withList <- unique(searchData$pmxid)
fullList <- unique(search_df$pmxid)
withoutList <- fullList[!fullList %in% withList]

allSearchData <- search_df
allSearchData$fullURL <- ifelse(search_df$pmxid %in% withList, 1, 0)

allSearchData <- allSearchData %>% 
  rename(identity = pmxid) %>% 
  select(identity, fullURL) %>% 
  mutate(fullURL = factor(fullURL))

surveyDataEdit <- surveyData %>% 
  select(identity, birthyr, gender, race, educ, marstat,
         employ, religpew_w5) %>% 
  mutate(age = factor(
           case_when(
             2018 - birthyr < 30 ~ "young",
             2018 - birthyr < 50 ~ "middle",
             2018 - birthyr >= 50 ~ "old",
             TRUE ~ NA_character_), 
           levels = c("young", "middle", "old")
         ),
         gender = factor(
           case_when(gender == 1 ~ "male", 
                     gender == 2 ~ "female", 
                     TRUE ~ NA_character_),
           levels = c("male", "female")
         ),
         race = factor(
           case_when(race == 1 ~ "white",
                     race == 2 ~ "black",
                     race == 3 ~ "hispanic",
                     race == 4 ~ "asian",
                     TRUE ~ "other"), 
           levels = c("white", "black", "hispanic", "asian", "other")
         ),
         educ = factor(
           case_when(educ %in% c(1, 2) ~ "highSchool",
                     educ %in% c(3, 4, 5) ~ "college",
                     educ == 6 ~ "advanced"), 
           levels = c("highSchool", "college", "advanced")
         ),
         marstat = factor(
           ifelse(marstat == 1, "married", "unmarried"), 
           levels = c("unmarried", "married")
         ),
         employ = factor(
           case_when(employ %in% c("Full-time", "Part-time") ~ "employed",
                     employ == "Student" ~ "student",
                     employ == "Retired" ~ "retired",
                     employ %in% c("Homemaker", "Permanently disabled",
                                   "Unemployed", "Temporarily laid off") ~ "unemployed",
                     TRUE ~ NA_character_), 
           levels = c("unemployed", "student", "retired", "employed")
         ),
         religion = factor(
           case_when(religpew_w5 %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ "religious",
                     religpew_w5 %in% c(9, 10, 11) ~ "nonReligious",
                     TRUE ~ NA_character_), 
           levels = c("nonReligious", "religious")
         )
  )

allMergedData <- merge(surveyDataEdit, allSearchData, by = "identity")
uniqueUsers <- allMergedData[!duplicated(allMergedData$identity), ]

mean(as.numeric(as.character(uniqueUsers$fullURL))) # 63% have full URLs

# logistic regression model
index <- createDataPartition(uniqueUsers$fullURL, p = 0.7, 
                             list = FALSE)
trainData <- uniqueUsers[index, ]
testData  <- uniqueUsers[-index, ]

model <- glm(fullURL ~ gender + race + educ + marstat + employ + religion + age,
                    family = binomial,
                    data = trainData)

summary(model)

preds <- model %>% predict(testData, type = "response")
predictedClasses <- ifelse(preds > 0.5, 1, 0)
y <- testData$fullURL

accuracy <- mean(predictedClasses == y, na.rm = TRUE)
precision <- posPredValue(factor(predictedClasses), y, positive = "1")
recall <- sensitivity(factor(predictedClasses), y, positive = "1")
F1 <- (2 * precision * recall) / (precision + recall)

resTable <- data.frame(accuracy, precision, recall, F1) %>% 
  round(2) %>% 
  kable() %>% 
  kable_styling() %>% 
  add_header_above(c("Logistic Regression - Full URLs" = 4))

modelOR <- exp(coef(model))
modelConf <- exp(confint(model))
modelP <- list(summary(model)$coefficients[ ,4])

stargazer(model,
          coef = list(modelOR),
          ci = T,
          ci.custom = list(modelConf),
          p = c(modelP),
          header = FALSE,
          title = "Odds-ratios Model for Full URLs", 
          type = "text")


# descriptive stats 
surveyDataDesc <- surveyData %>% 
  select(identity, birthyr, gender, race, 
         educ, marstat, employ, religpew_w5,
         ideo5)

allMergedDataDesc <- merge(surveyDataDesc, allSearchData, by = "identity")
uniqueUsersDesc <- allMergedDataDesc[!duplicated(allMergedDataDesc$identity), ]

table <- uniqueUsersDesc %>% 
  group_by(fullURL) %>% 
  add_count(fullURL) %>% 
  mutate(Share = n/length(uniqueUsers$fullURL)) %>% 
  select(fullURL, n, Share, birthyr, gender, race, 
         educ, marstat, employ, religpew_w5, ideo5) %>% 
  mutate(Age = 2018 - birthyr,
         white = ifelse(race == 1, 1, 0),
         hasDegree = ifelse(educ >= 4, 1, 0),
         married = ifelse(marstat == 1, 1, 0),
         fullTime = ifelse(employ == "Full-time", 1, 0),
         religious = case_when(religpew_w5 %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 1,
                               TRUE ~ 0)) %>%
  select(-birthyr, -educ, -race, -marstat, -employ, -religpew_w5) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  rename(Count = n, 
         PercentWomen = gender,
         PercentWhite = white,
         PercentMarried = married,
         PercentFullTime = fullTime,
         PercentReligious = religious,
         Ideology = ideo5) %>% 
  select(fullURL, Count, Share, Age, PercentWomen, PercentWhite, PercentMarried,
         PercentFullTime, hasDegree, PercentReligious, Ideology) %>% 
  kable() %>% 
  kable_styling()
