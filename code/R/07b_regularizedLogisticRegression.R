library(tidyverse)
library(DMwR)
library(caret)
library(kableExtra)
library(vip)
library(glmnet)

set.seed(2342)

# setwd()
source("./code/R/00_functions.R")
load("./data/forModels/top1000BeforeDFM.RData")
load("./data/forModels/top1000WeekBeforeDFM.RData")
load("./data/forModels/bigramsBeforeDFM.RData")
load("./data/forModels/bigramsWeekBeforeDFM.RData")


# TURNOUT -----------------------------------------------------------------------

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

# variables
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

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsTurnout <- metrics(accuracy, precision, recall, f1) 

# feature importance
vip(cv, num_features = 20, geom = "point")



# VOTE CHOICE ------------------------------------------------------------------

# prep data
pmxid <- docvars(top1000BeforeDFM)

top1000before <- convert(top1000BeforeDFM, to = "data.frame")
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
X <- model.matrix(voteChoice ~ ., trainDataTop1000Balanced)[, -1]
Y <- as.numeric(as.character(trainDataTop1000Balanced$voteChoice))

cv <- cv.glmnet(X, Y, 
                alpha = 1,
                family = "binomial",
                type.measure = "class")

lambda_1se <- cv$lambda.1se

# prediction
testX <- model.matrix(voteChoice ~ ., testDataTop1000)[, -1]
testY <- as.numeric(as.character(testDataTop1000$voteChoice))
preds <- ifelse(predict(cv, 
                        newx = testX, 
                        s = lambda_1se, 
                        type = "response") > 0.5, 1, 0)

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsVote <- metrics(accuracy, precision, recall, f1) 

# feature importance
vip(cv, num_features = 20, geom = "point")

