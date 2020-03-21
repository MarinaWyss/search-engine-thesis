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
load("./data/forModels/behaviorLongBefore.RData")
load("./data/forModels/top1000BeforeDFM.RData")
load("./data/forModels/top1000WeekBeforeDFM.RData")
load("./data/forModels/bigramsBeforeDFM.RData")
load("./data/forModels/bigramsWeekBeforeDFM.RData")
load("./data/forModels/beforeDFMPolitical.RData")
load("./data/forModels/weekBeforeDFMPolitical.RData")

# FEATURE ENGINEERED ------------------------------------------------------------

# join data
allData <- merge(searchBehaviorBefore,
                 beforeSearchesJoined[c(-2, -3)],
                 by = "pmxid") %>% 
  select(-search_id)

# TURNOUT --------------------------------

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


# variables
X <- as.matrix(trainDataAllBalanced[setdiff(names(trainDataAllBalanced), "turnout")])
Y <- as.numeric(as.character(trainDataAllBalanced$turnout))

# cross validation
allCV <- xgb.cv(
  data = X,
  label = Y,
  nrounds = 6000,
  objective = "binary:logistic",
  early_stopping_rounds = 50, 
  nfold = 10,
  params = list(
    eta = 0.1,
    max_depth = 3,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 1.0),
  verbose = 0
)  

# minimum test CV error
min(allCV$evaluation_log$test_error_mean)

# hyperparameters
hyper_grid <- expand.grid(
  eta = 0.01,
  max_depth = 3, 
  min_child_weight = 3,
  subsample = 0.5, 
  colsample_bytree = 0.5,
  gamma = c(0, 1, 10, 100, 1000),
  lambda = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  alpha = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  error = 0,          
  trees = 0          
)

for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123)
  m <- xgb.cv(
    data = X,
    label = Y,
    nrounds = 4000,
    objective = "binary:logistic",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = hyper_grid$eta[i], 
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i], 
      lambda = hyper_grid$lambda[i], 
      alpha = hyper_grid$alpha[i]
    ) 
  )
  hyper_grid$error[i] <- min(m$evaluation_log$test_error_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

hyper_grid %>%
  filter(error > 0) %>%
  arrange(error) %>%
  glimpse()

# before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, alpha 0.01, trees 121
# week before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, lambda 1, alpha 1, trees 18

params <- list(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5, 
  alpha = 0.01
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 121,
  objective = "binary:logistic",
  verbose = 0
)

# prediction 
testY <- testDataAll$turnout
testDataAll <- testDataAll %>%
  select(-turnout)

preds <- predict(xgbTrain, as.matrix(testDataAll))
preds <- as.integer(preds > 0.5)

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsTurnout <- metrics(accuracy, precision, recall, f1) 

# feature importance
vip(xgbTrain) 


# VOTE CHOICE -----------------------------

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

# variables
X <- as.matrix(trainDataAllBalanced[setdiff(names(trainDataAllBalanced), "voteChoice")])
Y <- as.numeric(as.character(trainDataAllBalanced$voteChoice))

# cross validation
allCV <- xgb.cv(
  data = X,
  label = Y,
  nrounds = 6000,
  objective = "binary:logistic",
  early_stopping_rounds = 50, 
  nfold = 10,
  params = list(
    eta = 0.1,
    max_depth = 3,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 1.0),
  verbose = 0
)  

# minimum test CV error
min(allCV$evaluation_log$test_error_mean)

# hyperparameters
hyper_grid <- expand.grid(
  eta = 0.01,
  max_depth = 3, 
  min_child_weight = 3,
  subsample = 0.5, 
  colsample_bytree = 0.5,
  gamma = c(0, 1, 10, 100, 1000),
  lambda = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  alpha = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  error = 0,          
  trees = 0          
)

for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123)
  m <- xgb.cv(
    data = X,
    label = Y,
    nrounds = 4000,
    objective = "binary:logistic",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = hyper_grid$eta[i], 
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i], 
      lambda = hyper_grid$lambda[i], 
      alpha = hyper_grid$alpha[i]
    ) 
  )
  hyper_grid$error[i] <- min(m$evaluation_log$test_error_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

hyper_grid %>%
  filter(error > 0) %>%
  arrange(error) %>%
  glimpse()

# before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, lambda 0.1, trees 198
# week before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, lambda 1, trees 20

params <- list(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5,
  lambda = 0.01
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 198,
  objective = "binary:logistic",
  verbose = 0
)

# prediction 
testY <- testDataAll$voteChoice
testDataAll <- testDataAll %>%
  select(-voteChoice)

preds <- predict(xgbTrain, as.matrix(testDataAll))
preds <- as.integer(preds > 0.5)

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsVote <- metrics(accuracy, precision, recall, f1) 

# feature importance
vip(xgbTrain) 



# DFMS  ----------------------------------------------------------------------

# TURNOUT --------------------------------

# prep data
pmxid <- docvars(beforeDFMPolitical)

top1000before <- convert(beforeDFMPolitical, to = "data.frame")
top1000before <- top1000before[, !duplicated(colnames(top1000before))]
top1000before <- cbind(pmxid$turnout, top1000before)

top1000before <- top1000before %>% 
  select(-document) %>% 
  rename(turnout = `pmxid$turnout`) %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = factor(turnout)) %>% 
  select_if(negate(function(col) is.numeric(col) && sum(col) < 1))

# split data into training and test 
indexTop1000Before <- createDataPartition(top1000before$turnout, 
                                          p = 0.7, 
                                          list = FALSE)

trainDataTop1000 <- top1000before[indexTop1000Before, ]
testDataTop1000  <- top1000before[-indexTop1000Before, ]

# resampling the data 
trainDataTop1000Balanced <- SMOTE(turnout ~., 
                                  data = trainDataTop1000,
                                  perc.over = 500, 
                                  perc.under = 120)

# variables
X <- as.matrix(trainDataTop1000Balanced[setdiff(names(trainDataTop1000Balanced), "turnout")])
Y <- as.numeric(as.character(trainDataTop1000Balanced$turnout))

# cross validation
allCV <- xgb.cv(
  data = X,
  label = Y,
  nrounds = 6000,
  objective = "binary:logistic",
  early_stopping_rounds = 50, 
  nfold = 10,
  params = list(
    eta = 0.1,
    max_depth = 3,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 1.0),
  verbose = 0
)  

# minimum test CV error
min(allCV$evaluation_log$test_error_mean)

# hyperparameters
hyper_grid <- expand.grid(
  eta = 0.01,
  max_depth = 3, 
  min_child_weight = 3,
  subsample = 0.5, 
  colsample_bytree = 0.5,
  gamma = c(0, 1, 10, 100, 1000),
  lambda = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  alpha = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  error = 0,          
  trees = 0          
)

for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123)
  m <- xgb.cv(
    data = X,
    label = Y,
    nrounds = 4000,
    objective = "binary:logistic",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = hyper_grid$eta[i], 
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i], 
      lambda = hyper_grid$lambda[i], 
      alpha = hyper_grid$alpha[i]
    ) 
  )
  hyper_grid$error[i] <- min(m$evaluation_log$test_error_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

hyper_grid %>%
  filter(error > 0) %>%
  arrange(error) %>%
  glimpse()

# top 1000 before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, gamma 1, trees 100
# top 1000 week before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, gamma 1, trees 318
# bigrams before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, gamma 1, lambda 0.01, trees 411
# bigrams week before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, trees 123

params <- list(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 47,
  objective = "binary:logistic",
  verbose = 0
)

# prediction 
testY <- testDataTop1000$turnout
testDataTop1000 <- testDataTop1000 %>%
  select(-turnout)

preds <- predict(xgbTrain, as.matrix(testDataTop1000))
preds <- as.integer(preds > 0.5)

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsTurnout <- metrics(accuracy, precision, recall, f1) 

# feature importance
vip(xgbTrain) 


# VOTE CHOICE ------------------------------------------------------------------

# prep data
pmxid <- docvars(beforeDFMPolitical)

top1000before <- convert(beforeDFMPolitical, to = "data.frame")
top1000before <- top1000before[, !duplicated(colnames(top1000before))]
top1000before <- cbind(pmxid$voteChoice, top1000before)

top1000before <- top1000before %>% 
  select(-document) %>% 
  rename(voteChoice = `pmxid$voteChoice`) %>% 
  filter(!is.na(voteChoice) & voteChoice %in% c(1, 2)) %>% 
  mutate(voteChoice = ifelse(voteChoice == 2, 0, 1),
         voteChoice = factor(voteChoice)) %>% 
  select_if(negate(function(col) is.numeric(col) && sum(col) < 1))

# split data into training and test 
indexTop1000Before <- createDataPartition(top1000before$voteChoice, 
                                          p = 0.7, 
                                          list = FALSE)

trainDataTop1000 <- top1000before[indexTop1000Before, ]
testDataTop1000  <- top1000before[-indexTop1000Before, ]

# resampling the data 
trainDataTop1000Balanced <- SMOTE(voteChoice ~., 
                                  data = trainDataTop1000,
                                  perc.over = 200, 
                                  perc.under = 150)


# variables
X <- as.matrix(trainDataTop1000Balanced[setdiff(names(trainDataTop1000Balanced), "voteChoice")])
Y <- as.numeric(as.character(trainDataTop1000Balanced$voteChoice))

# cross validation
allCV <- xgb.cv(
  data = X,
  label = Y,
  nrounds = 6000,
  objective = "binary:logistic",
  early_stopping_rounds = 50, 
  nfold = 10,
  params = list(
    eta = 0.1,
    max_depth = 3,
    min_child_weight = 3,
    subsample = 0.8,
    colsample_bytree = 1.0),
  verbose = 0
)  

# minimum test CV error
min(allCV$evaluation_log$test_error_mean)

# hyperparameters
hyper_grid <- expand.grid(
  eta = 0.01,
  max_depth = 3, 
  min_child_weight = 3,
  subsample = 0.5, 
  colsample_bytree = 0.5,
  gamma = c(0, 1, 10, 100, 1000),
  lambda = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  alpha = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  error = 0,          
  trees = 0          
)

for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123)
  m <- xgb.cv(
    data = X,
    label = Y,
    nrounds = 4000,
    objective = "binary:logistic",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = hyper_grid$eta[i], 
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i], 
      lambda = hyper_grid$lambda[i], 
      alpha = hyper_grid$alpha[i]
    ) 
  )
  hyper_grid$error[i] <- min(m$evaluation_log$test_error_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

hyper_grid %>%
  filter(error > 0) %>%
  arrange(error) %>%
  glimpse()

# top 1000 before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, trees 83
# top 1000 week before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, trees 236
# bigrams before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, gamma 1, lambda 0.01, trees 230
# bigrams week before: eta 0.01, max_depth 3, min_child 3, subsample 0.5, colsample 0.5, gamma 1, trees 79

params <- list(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5,
  gamma = 1
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 79,
  objective = "binary:logistic",
  verbose = 0
)

# prediction 
testY <- testDataTop1000$voteChoice
testDataTop1000 <- testDataTop1000 %>%
  select(-voteChoice)

preds <- predict(xgbTrain, as.matrix(testDataTop1000))
preds <- as.integer(preds > 0.5)

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsVote <- metrics(accuracy, precision, recall, f1) 

# feature importance
vip(xgbTrain) 


