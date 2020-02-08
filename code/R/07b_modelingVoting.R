library(tidyverse)
library(xgboost)
library(lme4)
library(DMwR)
library(caret)
library(kableExtra)

set.seed(2342)

# setwd()
load("./data/forModels/beforeSearchesJoined.RData")
load("./data/forModels/weekBeforeSearchesJoined.RData")
load("./data/forModels/searchBehaviorBefore.RData")
load("./data/forModels/searchBehaviorWeekBefore.RData")

# functions
acc <- function(x, y){
  1 - (as.numeric(sum(x != y)) / length(y))
}

prec <- function(x, y){
  posPredValue(as.factor(x), 
               as.factor(y), 
               positive = "1")
}

rec <- function(x, y){
  sensitivity(as.factor(x), 
              as.factor(y), 
              positive = "1")
}


#########################
########## DFM ##########
#########################
# pmxid <- docvars(top1000BeforeDFM$pm)
# 
# df <- data.frame(top1000BeforeDFM)
# df <- cbind(pmxid$turnout, df)
# df <- df %>% 
#   select(-document) %>% 
#   rename(turnout = `pmxid$turnout`)
# 
# dfUp <- upSample(x = df,
#                  y = as.factor(df$turnout)) %>% 
#   select(-Class)
# 
# indexDFM <- createDataPartition(dfUp$turnout, p = 0.7, 
#                                      list = FALSE)
# 
# trainDataDFM <- dfUp[indexDFM, ]
# testDataDFM  <- dfUp[-indexDFM, ]


#########################
######## searches #######
#########################
searches <- beforeSearchesJoined %>% 
  select(-turnout, -pmxid) %>% 
  filter(!is.na(voteChoice) & voteChoice %in% c(1, 2)) %>% 
  mutate(voteChoice = ifelse(voteChoice == 2, 0, 1),
         voteChoice = factor(voteChoice))

# resampling the data 
searchesBalanced <- SMOTE(voteChoice ~., 
                          data = searches,
                          perc.over = 500, 
                          perc.under = 120)

# split data into training and test and prep
indexSearches <- createDataPartition(searchesBalanced$voteChoice, 
                                     p = 0.7, 
                                     list = FALSE)

trainDataSearches <- searchesBalanced[indexSearches, ]
testDataSearches  <- searchesBalanced[-indexSearches, ]

X <- as.matrix(trainDataSearches[setdiff(names(trainDataSearches), "voteChoice")])
Y <- as.numeric(as.character(trainDataSearches$voteChoice))

# cross validation
searchesCV <- xgb.cv(
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
min(searchesCV$evaluation_log$test_error_mean)

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
  nrounds = 238,
  objective = "binary:logistic",
  verbose = 0
)

# vip
vip::vip(xgbTrain) 

# prediction 
preds <- predict(xgbTrain, as.matrix(testDataSearches[-1]))
preds <- as.integer(preds > 0.5)
testY <- testDataSearches$voteChoice

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
F1 <- (2 * precision * recall) / (precision + recall)

metricsSearches <- data.frame(accuracy, precision, recall, F1) %>% 
  kable() %>% 
  kable_styling()



#########################
######## behavior #######
#########################
behavior <- searchBehaviorBefore %>% 
  select(-pmxid, -turnout) %>% 
  filter(!is.na(voteChoice) & voteChoice %in% c(1, 2)) %>% 
  mutate(voteChoice = ifelse(voteChoice == 2, 0, 1),
         voteChoice = factor(voteChoice))

# resampling the data 
behaviorBalanced <- SMOTE(voteChoice ~., 
                          data = behavior,
                          perc.over = 500, 
                          perc.under = 120)

# split data into training and test and prep
indexBehavior <- createDataPartition(behaviorBalanced$voteChoice, 
                                     p = 0.7, 
                                     list = FALSE)

trainDataBehavior <- behaviorBalanced[indexBehavior, ]
testDataBehavior  <- behaviorBalanced[-indexBehavior, ]

X <- as.matrix(trainDataBehavior[setdiff(names(trainDataBehavior), "voteChoice")])
Y <- as.numeric(as.character(trainDataBehavior$voteChoice))

# cross validation
behaviorCV <- xgb.cv(
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
min(behaviorCV$evaluation_log$test_error_mean)

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
  nrounds = 795,
  objective = "binary:logistic",
  verbose = 0
)

# vip
vip::vip(xgbTrain) 

# prediction 
preds <- predict(xgbTrain, as.matrix(testDataBehavior[-1]))
preds <- as.integer(preds > 0.5)
testY <- testDataBehavior$voteChoice

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
F1 <- (2 * precision * recall) / (precision + recall)

metricsBehavior <- data.frame(accuracy, precision, recall, F1) %>% 
  kable() %>% 
  kable_styling()


#########################
# searches and behavior #
#########################
allData <- merge(searchBehaviorBefore,
                 beforeSearchesJoined[c(-2, -3)],
                 by = "pmxid")

allData <- allData %>% 
  select(-pmxid, -turnout) %>% 
  filter(!is.na(voteChoice) & voteChoice %in% c(1, 2)) %>% 
  mutate(voteChoice = ifelse(voteChoice == 2, 0, 1),
         voteChoice = factor(voteChoice))

# resampling the data 
allDataBalanced <- SMOTE(voteChoice ~., 
                         data = allData,
                         perc.over = 500, 
                         perc.under = 120)

# split data into training and test and prep
indexAll <- createDataPartition(allDataBalanced$voteChoice, 
                                p = 0.7, 
                                list = FALSE)

trainDataAll <- allDataBalanced[indexAll, ]
testDataAll  <- allDataBalanced[-indexAll, ]

X <- as.matrix(trainDataAll[setdiff(names(trainDataAll), "voteChoice")])
Y <- as.numeric(as.character(trainDataAll$voteChoice))

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

params <- list(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5,
  alpha = 1.0
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 721,
  objective = "binary:logistic",
  verbose = 0
)

# vip
vip::vip(xgbTrain) 

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
F1 <- (2 * precision * recall) / (precision + recall)

metricsAll <- data.frame(accuracy, precision, recall, F1) %>% 
  kable() %>% 
  kable_styling()


