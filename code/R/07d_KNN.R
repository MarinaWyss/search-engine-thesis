library(tidyverse)
library(xgboost)
library(DMwR)
library(caret)
library(kableExtra)
library(vip)

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
                 by = "pmxid") 


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

# prep variables
trainKNNSearches <- trainDataAllBalanced %>%
  select(-searched_other_candidate) %>% 
  na.omit()

testKNNSearches <- testDataAll %>% 
  select(-searched_other_candidate) %>% 
  na.omit()

# hyperparameters
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 10)

knnFit <- train(turnout ~ ., 
                data = trainKNNSearches, 
                method = "knn", 
                trControl = ctrl, 
                preProcess = c("center","scale"),
                tuneLength = 20)

# prediction
preds <- predict(knnFit, newdata = testKNNSearches[, -1])
testY <- testKNNSearches$turnout

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsTurnout <- metrics(accuracy, precision, recall, f1) 

# feature importance
varImp(knnFit)


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

# prep variables
trainKNNSearches <- trainDataAllBalanced %>%
  select(-searched_other_candidate) %>% 
  na.omit()

testKNNSearches <- testDataAll %>% 
  select(-searched_other_candidate) %>% 
  na.omit()

# hyperparameters
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 10)

knnFit <- train(voteChoice ~ ., 
                data = trainKNNSearches, 
                method = "knn", 
                trControl = ctrl, 
                preProcess = c("center","scale"),
                tuneLength = 20)

# prediction
preds <- predict(knnFit, newdata = testKNNSearches[, -1])
testY <- testKNNSearches$voteChoice

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsVote <- metrics(accuracy, precision, recall, f1) 

# feature importance
varImp(knnFit)


