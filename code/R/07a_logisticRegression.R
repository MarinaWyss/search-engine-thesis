library(tidyverse)
library(kableExtra)
library(DMwR)
library(caret)

set.seed(2342)

# setwd()
source("./code/R/00_functions.R")
load("./data/forModels/beforeSearchesJoined.RData")
load("./data/forModels/weekBeforeSearchesJoined.RData")
load("./data/forModels/searchBehaviorBefore.RData")
load("./data/forModels/searchBehaviorWeekBefore.RData")

# join data
allData <- merge(searchBehaviorBefore,
                 beforeSearchesJoined[c(-2, -3)],
                 by = "pmxid") %>% 
  select(-search_id)

# TURNOUT -----------------------------------------------------------------------

# prep data
turnoutData <- allData %>% 
  filter(!is.na(turnout)) %>% 
  select(-pmxid, -voteChoice) %>% 
  mutate(turnout = factor(turnout))

# split data into training and test 
indexAll <- createDataPartition(turnoutData$turnout, 
                                p = 0.7, 
                                list = FALSE)

trainDataAll <- turnoutData[indexAll, ]
testDataAll  <- turnoutData[-indexAll, ]

# resampling the data 
trainDataAllBalanced <- SMOTE(turnout ~., 
                              data = trainDataAll,
                              perc.over = 500, 
                              perc.under = 120)

# modeling
lrTurnout <- glm(turnout ~., 
                 data = trainDataAllBalanced, 
                 family = binomial(link = "logit"))

summary(lrTurnout)

# prediction 
preds = ifelse(predict(lrTurnout, 
                       testDataAll, 
                type = "response") > 0.5, 1, 0)

testY <- as.numeric(as.character(testDataAll$turnout))

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsTurnout <- metrics(accuracy, precision, recall, f1) 




# VOTE CHOICE ---------------------------------------------------------------------

# prep data
voteData <- allData %>% 
  filter(!is.na(voteChoice) & voteChoice %in% c(1, 2)) %>% 
  mutate(voteChoice = ifelse(voteChoice == 2, 0, 1),
         voteChoice = factor(voteChoice)) %>% 
  select(-pmxid, -turnout) 
  

# split data into training and test 
indexAll <- createDataPartition(voteData$voteChoice, 
                                p = 0.7, 
                                list = FALSE)

trainDataAll <- voteData[indexAll, ]
testDataAll  <- voteData[-indexAll, ]

# resampling the data 
trainDataAllBalanced <- SMOTE(voteChoice ~., 
                              data = trainDataAll,
                              perc.over = 200, 
                              perc.under = 150)

# modeling
lrVoteChoice <- glm(voteChoice ~., 
                    data = trainDataAllBalanced, 
                    family = binomial(link = "logit"))

summary(lrVoteChoice)

# prediction 
preds = ifelse(predict(lrVoteChoice, 
                       testDataAll, 
                       type = "response") > 0.5, 1, 0)

testY <- as.numeric(as.character(testDataAll$voteChoice))

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsVote <- metrics(accuracy, precision, recall, f1) 















