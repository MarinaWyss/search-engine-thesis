library(tidyverse)
library(xgboost)
library(lme4)
library(DMwR)
library(caret)
library(kableExtra)
library(e1071)
library(vip)
library(glmnet)

set.seed(2342)

# setwd()
load("./data/forModels/beforeSearchesJoined.RData")
load("./data/forModels/weekBeforeSearchesJoined.RData")
load("./data/forModels/searchBehaviorBefore.RData")
load("./data/forModels/searchBehaviorWeekBefore.RData")
load("./data/forModels/behaviorLongBefore.RData")
load("./data/forModels/top1000BeforeDFM.RData")
load("./data/forModels/top1000WeekBeforeDFM.RData")
load("./data/forModels/bigramsBeforeDFM.RData")
load("./data/forModels/bigramsWeekBeforeDFM.RData")

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

# prep data
pmxid <- docvars(top1000BeforeDFM)

top1000before <- convert(top1000BeforeDFM, to = "data.frame")
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

### SUPPORT VECTOR MACHINE ###
# hyperparameters
bestTune <- best.tune(svm, 
                train.x = trainDataTop1000Balanced[ ,-1],
                train.y = trainDataTop1000Balanced[ ,1],
                kernel = "linear",
                ranges = list(cost = 10^(-1:2),
                              gamma = c(0.5, 1, 2)))

# modeling
svmTop1000 <- svm(turnout ~ ., 
                  data = trainDataTop1000Balanced,
                  cost = 1,
                  gamma  = 0.5,
                  kernel = "linear")

# prediction
preds <- predict(svmTop1000, testDataTop1000[,-1])
testY <- as.numeric(as.character(testDataTop1000$turnout))

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
F1 <- (2 * precision * recall) / (precision + recall)

metricsSVM <- data.frame(accuracy, precision, recall, F1) %>% 
  kable() %>% 
  kable_styling()

# feature importance
w <- t(svmTop1000$coefs) %*% svmTop1000$SV                
w <- apply(w, 2, function(v){sqrt(sum(v^2))}) 
w <- sort(w, decreasing = T)
topWeights <- data.frame(head(w, 20))
topWeights <- topWeights %>% 
  rownames_to_column("search_term") %>% 
  rename("weight" = `head.w..20.`)

svmBeforePlot <- ggplot(data = topWeights, 
                        aes(x = reorder(search_term, weight), y = weight)) + 
  geom_bar(stat = "identity", aes(fill = search_term)) +
  coord_flip() + 
  labs(x = "Query",
       y = "Weight",
       title = "SVM FI")

### REGULARIZED LOGISTIC REGRESSION ###
X <- model.matrix(turnout ~ ., trainDataTop1000Balanced)[, -1]
Y <- as.numeric(as.character(trainDataTop1000Balanced$turnout))

cv <- cv.glmnet(X, Y, 
                alpha = 1,
                family = "binomial",
                type.measure = "class")

lambda_1se <- cv$lambda.1se

# prediction
testX <- model.matrix(turnout ~ ., testDataTop1000)[, -1]
testY <- as.numeric(as.character(testDataTop1000$turnout))
preds <- ifelse(predict(cv, 
                        newx = testX, 
                        s = lambda_1se, 
                        type = "response") > 0.5, 1, 0)

accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
F1 <- (2 * precision * recall) / (precision + recall)

metricsLogReg <- data.frame(accuracy, precision, recall, F1) %>% 
  kable() %>% 
  kable_styling()

# feature importance
vip(cv, num_features = 20, geom = "point")


### XGBOOST ###
# prep variables
X <- as.matrix(trainDataTop1000Balanced[setdiff(names(trainDataTop1000Balanced), "turnout")])
Y <- as.numeric(as.character(trainDataTop1000Balanced$turnout))

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
  colsample_bytree = 0.5,
  lambda = 0.01
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 332,
  objective = "binary:logistic",
  verbose = 0
)

# vip
vip(xgbTrain) 

# prediction 
preds <- predict(xgbTrain, as.matrix(testDataTop1000[-1]))
preds <- as.integer(preds > 0.5)
testY <- testDataTop1000$turnout

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
F1 <- (2 * precision * recall) / (precision + recall)

metricsXGBDFM <- data.frame(accuracy, precision, recall, F1) %>% 
  kable() %>% 
  kable_styling()



#########################
######## searches #######
#########################
searches <- beforeSearchesJoined %>% 
  select(-voteChoice, -pmxid) %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = factor(turnout))

# split data into training and test and prep
indexSearches <- createDataPartition(searches$turnout, 
                                     p = 0.7, 
                                     list = FALSE)

trainDataSearches <- searches[indexSearches, ]
testDataSearches  <- searches[-indexSearches, ]

# resampling the data 
trainDataSearchesBalanced <- SMOTE(turnout ~., 
                                   data = trainDataSearches,
                                   perc.over = 500, 
                                   perc.under = 120)

# prep variables
X <- as.matrix(trainDataSearchesBalanced[setdiff(names(trainDataSearchesBalanced), "turnout")])
Y <- as.numeric(as.character(trainDataSearchesBalanced$turnout))

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
  nrounds = 8,
  objective = "binary:logistic",
  verbose = 0
)

# vip
vip(xgbTrain) 

# prediction 
preds <- predict(xgbTrain, as.matrix(testDataSearches[-1]))
preds <- as.integer(preds > 0.5)
testY <- testDataSearches$turnout

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
  select(-pmxid, -voteChoice) %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = factor(turnout))

# split data into training and test and prep
indexBehavior <- createDataPartition(behavior$turnout, 
                                     p = 0.7, 
                                     list = FALSE)

trainDataBehavior <- behavior[indexBehavior, ]
testDataBehavior  <- behavior[-indexBehavior, ]

# resampling the data 
trainDataBehaviorBalanced <- SMOTE(turnout ~., 
                                   data = trainDataBehavior,
                                   perc.over = 500, 
                                   perc.under = 120)

# scaling
trainDataBehaviorBalanced <- trainDataBehaviorBalanced %>% 
  mutate_at(vars(time_reg, mean_search_length, sentiment), 
            scale) %>% 
  mutate(turnout = as.numeric(as.character(turnout)))

testDataBehavior <- testDataBehavior %>% 
  mutate_at(vars(time_reg, mean_search_length, sentiment), 
            scale) %>% 
  mutate(turnout = as.numeric(as.character(turnout)))

# variables 
X <- as.matrix(trainDataBehaviorBalanced[setdiff(names(trainDataBehaviorBalanced), "turnout")])
Y <- trainDataBehaviorBalanced$turnout

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
  colsample_bytree = 0.5,
  lambda = 1, 
  alpha = 1
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 140,
  objective = "binary:logistic",
  verbose = 0
)

# vip
vip(xgbTrain) 

# prediction 
preds <- predict(xgbTrain, as.matrix(testDataBehavior[-1]))
preds <- as.integer(preds > 0.5)
testY <- testDataBehavior$turnout

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
  filter(!is.na(turnout)) %>% 
  select(-pmxid, -voteChoice) %>% 
  mutate(turnout = factor(turnout))

# split data into training and test and prep
indexAll <- createDataPartition(allData$turnout, 
                                p = 0.7, 
                                list = FALSE)

trainDataAll <- allData[indexAll, ]
testDataAll  <- allData[-indexAll, ]

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

params <- list(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5, 
  lamda = 0.1,
  alpha = 1
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 370,
  objective = "binary:logistic",
  verbose = 0
)

# vip
vip(xgbTrain) 

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
F1 <- (2 * precision * recall) / (precision + recall)

metricsAll <- data.frame(accuracy, precision, recall, F1) %>% 
  kable() %>% 
  kable_styling()



#########################
##### behavior long #####
#########################
behaviorLongBefore <- behaviorLongBefore %>% 
  mutate(time_reg = scale(time_reg),
         search_length = scale(search_length),
         sentiment = scale(sentiment),
         turnout = factor(turnout),
         search_engine = factor(search_engine)) %>% 
  select(-date, -voteChoice) %>% 
  as.data.frame()
  
# resampling the data
behaviorLongBeforeBalanced <- SMOTE(turnout ~., 
                                    data = behaviorLongBefore,
                                    perc.over = 500, 
                                    perc.under = 120)

behaviorLongModel <- glmer(
  turnout ~ 
    time_reg + search_engine + search_length + sentiment + (1 | pmxid),
  data = behaviorLongBeforeBalanced,
  family = binomial(link = "logit")) 

summary(behaviorLongModel)


