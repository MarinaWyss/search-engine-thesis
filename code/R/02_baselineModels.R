library(tidyverse)
library(stargazer)
library(caret)
library(kableExtra)

set.seed(12345)

# setwd()
load("./data/preppedFullData.RData")
source("./code/R/00_functions.R")

uniqueUsers <- fullDataSet[!duplicated(fullDataSet$pmxid), ]

# prep data
baselineUsers <- uniqueUsers %>% 
  select(turnout, voteChoice, birthyr, gender, race, educ, marstat,
         employ, religion) %>% 
  mutate(turnout = factor(turnout),
         age = factor(
           case_when(
             2018 - birthyr < 30 ~ "young",
             2018 - birthyr < 50 ~ "middle",
             2018 - birthyr >= 50 ~ "old",
             TRUE ~ NA_character_), 
           levels = c("young", "middle", "old")
           ),
         gender = factor(
           ifelse(gender == 0, "male", "female"),
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
           case_when(religion %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ "religious",
                     religion %in% c(9, 10, 11) ~ "nonReligious",
                     TRUE ~ NA_character_), 
           levels = c("nonReligious", "religious")
           )
         )

# turnout baseline
indexTurnout <- createDataPartition(baselineUsers$turnout, p = 0.7, 
                             list = FALSE)
turnoutTrainData <- baselineUsers[indexTurnout, ]
turnoutTestData  <- baselineUsers[-indexTurnout, ]

turnoutModel <- glm(turnout ~ gender + race + educ + marstat + employ + religion + age,
                    family = binomial,
                    data = turnoutTrainData)

summary(turnoutModel)

preds <- turnoutModel %>% predict(turnoutTestData, type = "response")
preds <- factor(ifelse(preds > 0.5, 1, 0))
testY <- turnoutTestData$turnout

accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)

resTableTurnout <- data.frame(Accuracy = accuracy,
                              Precision = precision, 
                              Recall = recall,
                              F1 = f1) %>% 
  round(2) %>% 
  kable() %>% 
  kable_styling() %>% 
  add_header_above(c("Logistic Regression Baseline: Turnout" = 4))

# vote choice baseline
voteChoiceData <- baselineUsers %>% 
  filter(voteChoice %in% c(1, 2)) %>% 
  mutate(voteChoice = factor(ifelse(voteChoice == 2, 0, voteChoice)))

indexParty <- createDataPartition(voteChoiceData$voteChoice, p = 0.7, list = FALSE)
partyTrainData <- voteChoiceData[indexParty, ]
partyTestData  <- voteChoiceData[-indexParty, ]

partyModel <- glm(voteChoice ~ gender + race + educ + marstat + employ + religion + age,
                    family = binomial,
                    data = partyTrainData)

summary(partyModel)

preds <- partyModel %>% predict(partyTestData, type = "response")
preds <- factor(ifelse(preds > 0.5, 1, 0))
testY <- partyTestData$voteChoice

accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)

resTableParty <- data.frame(Accuracy = accuracy,
                            Precision = precision, 
                            Recall = recall,
                            F1 = f1) %>% 
  round(2) %>% 
  kable() %>% 
  kable_styling() %>% 
  add_header_above(c("Logistic Regression Baseline: Vote Choice" = 4))

# tables
turnoutModelOR <- exp(coef(turnoutModel))
turnoutModelConf <- exp(confint(turnoutModel))
turnoutModelp <- list(summary(turnoutModel)$coefficients[ ,4])

partyModelOR <- exp(coef(partyModel))
partyModelConf <- exp(confint(partyModel))
partyModelp <- list(summary(partyModel)$coefficients[ ,4])

stargazer(turnoutModel,
          coef = list(turnoutModelOR),
          ci = T,
          ci.custom = list(turnoutModelConf),
          p = c(turnoutModelp),
          header = FALSE,
          no.space = TRUE,
          title = "Odds-ratios Model: Demographics and Turnout", 
          type = "html")

stargazer(partyModel,
          coef = list(partyModelOR),
          ci = T,
          ci.custom = list(partyModelConf),
          p = c(partyModelp),
          header = FALSE,
          title = "Odds-ratios Model: Demographics and Party Choice", 
          type = "latex")