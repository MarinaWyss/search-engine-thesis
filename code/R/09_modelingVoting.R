library(tidyverse)
library(xgboost)
library(caret)

set.seed(2342)


#########################
########## DFM ##########
#########################
pmxid <- docvars(beforeDFM)

df <- data.frame(beforeDFM)
df <- cbind(pmxid$turnout, df)
df <- df %>% 
  select(-document) %>% 
  rename(turnout = `pmxid$turnout`)

dfUp <- upSample(x = df,
                 y = as.factor(df$turnout)) %>% 
  select(-Class)

indexDFM <- createDataPartition(dfUp$turnout, p = 0.7, 
                                     list = FALSE)

trainDataDFM <- dfUp[indexDFM, ]
testDataDFM  <- dfUp[-indexDFM, ]


#########################
######## searches #######
#########################

searches <- beforeSearchesJoined %>% 
  select(-date, -word, -pmxid, -turnout) %>% 
  filter(!is.na(voteChoice) & voteChoice %in% c(1, 2)) %>% 
  mutate(voteChoice = ifelse(voteChoice == 2, 0, 1))

# upsampling the data 
searchesUp <- upSample(x = searches,
                           y = as.factor(searches$voteChoice)) %>% 
  select(-Class)

# split data into training and test and prep
indexSearches <- createDataPartition(searchesUp$voteChoice, p = 0.7, 
                                         list = FALSE)

trainDataSearches <- searchesUp[indexSearches, ]
testDataSearches  <- searchesUp[-indexSearches, ]

X <- as.matrix(trainDataSearches[setdiff(names(trainDataSearches), 
                                             "voteChoice")])
Y <- trainDataSearches$voteChoice

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
  rmse = 0,          
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
  hyper_grid$rmse[i] <- min(m$evaluation_log$test_rmse_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

hyper_grid %>%
  filter(rmse > 0) %>%
  arrange(rmse) %>%
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
  nrounds = 44,
  objective = "binary:logistic",
  verbose = 0
)

# vip
vip::vip(xgbTrain) 

# prediction 
preds <- predict(xgbTrain, as.matrix(testDataSearches[-1]))
preds <- as.integer(preds > 0.5)

# metrics - real bad lol
testY <- testDataSearches$voteChoice

accuracy <- 1 - (as.numeric(sum(preds != testY))/
                   length(testY))
precision <- posPredValue(as.factor(preds), as.factor(testY), 
                          positive = "1")
recall <- sensitivity(as.factor(preds), as.factor(testY), 
                      positive = "1")
F1 <- (2 * precision * recall) / (precision + recall)


#########################
######## behavior #######
#########################
behavior <- searchBehaviorBefore %>% 
  select(-pmxid, -turnout) %>% 
  filter(!is.na(voteChoice) & voteChoice %in% c(1, 2)) %>% 
  mutate(google = ifelse(search_engine == "Google", 1, 0),
         bing = ifelse(search_engine == "Bing", 1, 0),
         duck = ifelse(search_engine == "DuckDuckGo", 1, 0),
         yahoo = ifelse(search_engine == "Yahoo", 1, 0),
         other = ifelse(search_engine == "Other", 1, 0),
         voteChoice = ifelse(voteChoice == 2, 0, 1)) %>% 
  select(-search_engine)

# upsampling the data 
behaviorUp <- upSample(x = behavior,
                           y = as.factor(behavior$voteChoice)) %>% 
  select(-Class)

# split data into training and test and prep
indexBehavior <- createDataPartition(behaviorUp$voteChoice, 
                                     p = 0.7, 
                                     list = FALSE)

trainDataBehavior <- behaviorUp[indexBehavior, ]
testDataBehavior  <- behaviorUp[-indexBehavior, ]

X <- as.matrix(trainDataBehavior[setdiff(names(trainDataBehavior), 
                                             "voteChoice")])
Y <- trainDataBehavior$voteChoice

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
  rmse = 0,          
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
  hyper_grid$rmse[i] <- min(m$evaluation_log$test_rmse_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

hyper_grid %>%
  filter(rmse > 0) %>%
  arrange(rmse) %>%
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
  nrounds = 56,
  objective = "binary:logistic",
  verbose = 0
)

# vip
vip::vip(xgbTrain) 

# prediction 
preds <- predict(xgbTrain, as.matrix(testDataBehavior[-1]))
preds <- as.integer(preds > 0.5)

# metrics - medium bad
testY <- testDataBehavior$voteChoice

accuracy <- 1 - (as.numeric(sum(preds != testY))/
                   length(testY))
precision <- posPredValue(as.factor(preds), as.factor(testY), 
                          positive = "1")
recall <- sensitivity(as.factor(preds), as.factor(testY), 
                      positive = "1")
F1 <- (2 * precision * recall) / (precision + recall)


#########################
# searches and behavior #
#########################
allData <- merge(beforeSearchesJoined[c(-2, -3)], searchBehaviorBefore,
                 by = "pmxid")

allData <- allData %>% 
  filter(!is.na(voteChoice) & voteChoice %in% c(1, 2)) %>% 
  mutate(google = ifelse(search_engine == "Google", 1, 0),
         bing = ifelse(search_engine == "Bing", 1, 0),
         duck = ifelse(search_engine == "DuckDuckGo", 1, 0),
         yahoo = ifelse(search_engine == "Yahoo", 1, 0),
         other = ifelse(search_engine == "Other", 1, 0),
         voteChoice = ifelse(voteChoice == 2, 0, 1)) %>% 
  select(-search_engine, -date, -word, -pmxid, -turnout) 

allDataUp <- upSample(x = allData,
                       y = as.factor(allData$voteChoice)) %>% 
  select(-Class)

# split data into training and test and prep
indexAll <- createDataPartition(allDataUp$voteChoice, 
                                p = 0.7, 
                                list = FALSE)

trainDataAll <- allDataUp[indexAll, ]
testDataAll  <- allDataUp[-indexAll, ]

X <- as.matrix(trainDataAll[setdiff(names(trainDataAll), 
                                         "voteChoice")])
Y <- trainDataAll$voteChoice

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
  rmse = 0,          
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
  hyper_grid$rmse[i] <- min(m$evaluation_log$test_rmse_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

hyper_grid %>%
  filter(rmse > 0) %>%
  arrange(rmse) %>%
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
  nrounds = 172,
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

# metrics - 
accuracy <- 1 - (as.numeric(sum(preds != testY))/
                   length(testY))
precision <- posPredValue(as.factor(preds), as.factor(testY), 
                          positive = "1")
recall <- sensitivity(as.factor(preds), as.factor(testY), 
                      positive = "1")
F1 <- (2 * precision * recall) / (precision + recall)



