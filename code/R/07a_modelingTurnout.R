library(tidyverse)
library(xgboost)
library(lme4)
library(DMwR)
library(caret)
library(kableExtra)
library(e1071)
library(vip)

set.seed(2342)

# setwd()
load("./data/forModels/beforeSearchesJoined.RData")
load("./data/forModels/weekBeforeSearchesJoined.RData")
load("./data/forModels/searchBehaviorBefore.RData")
load("./data/forModels/searchBehaviorWeekBefore.RData")
load("./data/forModels/behaviorLongBefore.RData")
load("./data/forModels/top1000BeforeDFM.RData")
load("./data/forModels/top1000WeekBeforeDFM.RData")


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

# resampling the data 
top1000BeforeBalanced <- SMOTE(turnout ~., 
                               data = top1000before,
                               perc.over = 500, 
                               perc.under = 120)


# split data into training and test 
indexTop100Before <- createDataPartition(top1000BeforeBalanced$turnout, 
                                         p = 0.7, 
                                         list = FALSE)

trainDataTop1000 <- top1000BeforeBalanced[indexTop100Before, ]
testDataTop1000  <- top1000BeforeBalanced[-indexTop100Before, ]

### SUPPORT VECTOR MACHINE ###
# hyperparameters
bestTune <- best.tune(svm, 
                train.x = trainDataTop1000[ ,-1],
                train.y = trainDataTop1000[ ,1],
                kernel = "linear",
                ranges = list(cost = 10^(-1:2),
                              gamma = c(0.5, 1, 2)))

# modeling
svmTop1000 <- svm(turnout ~ ., 
                  data = trainDataTop1000,
                  cost = 0.1,
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


#########################
######## searches #######
#########################
searches <- beforeSearchesJoined %>% 
  select(-voteChoice, -pmxid) %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = factor(turnout))

# resampling the data 
searchesBalanced <- SMOTE(turnout ~., 
                          data = searches,
                          perc.over = 500, 
                          perc.under = 120)

# split data into training and test and prep
indexSearches <- createDataPartition(searchesBalanced$turnout, 
                                     p = 0.7, 
                                     list = FALSE)

trainDataSearches <- searchesBalanced[indexSearches, ]
testDataSearches  <- searchesBalanced[-indexSearches, ]

X <- as.matrix(trainDataSearches[setdiff(names(trainDataSearches), "turnout")])
Y <- as.numeric(as.character(trainDataSearches$turnout))

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
  nrounds = 79,
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

# resampling the data 
behaviorBalanced <- SMOTE(turnout ~., 
                          data = behavior,
                          perc.over = 500, 
                          perc.under = 120)

# split data into training and test and prep
indexBehavior <- createDataPartition(behaviorBalanced$turnout, 
                                     p = 0.7, 
                                     list = FALSE)

trainDataBehavior <- behaviorBalanced[indexBehavior, ]
trainDataBehavior <- trainDataBehavior %>% 
  mutate_at(vars(time_reg, mean_search_length, sentiment), 
            scale) %>% 
  mutate(turnout = as.numeric(as.character(turnout)))

testDataBehavior  <- behaviorBalanced[-indexBehavior, ]
testDataBehavior <- testDataBehavior %>% 
  mutate_at(vars(time_reg, mean_search_length, sentiment), 
            scale) %>% 
  mutate(turnout = as.numeric(as.character(turnout)))

X <- as.matrix(trainDataBehavior[setdiff(names(trainDataBehavior), "turnout")])
Y <- trainDataBehavior$turnout

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
  nrounds = 26,
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

# resampling the data 
allDataBalanced <- SMOTE(turnout ~., 
                          data = allData,
                          perc.over = 500, 
                          perc.under = 120)

# split data into training and test and prep
indexAll <- createDataPartition(allDataBalanced$turnout, 
                                p = 0.7, 
                                list = FALSE)

trainDataAll <- allDataBalanced[indexAll, ]
testDataAll  <- allDataBalanced[-indexAll, ]

X <- as.matrix(trainDataAll[setdiff(names(trainDataAll), "turnout")])
Y <- as.numeric(as.character(trainDataAll$turnout))

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
  colsample_bytree = 0.5
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 50,
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


