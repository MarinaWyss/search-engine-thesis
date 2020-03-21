library(tidyverse)
library(xgboost)
library(DMwR)
library(caret)
library(kableExtra)
library(e1071)
library(vip)

set.seed(2342)

# setwd()
source("./code/R/00_functions.R")
load("./data/forModels/top1000BeforeDFM.RData")
load("./data/forModels/top1000WeekBeforeDFM.RData")
load("./data/forModels/bigramsBeforeDFM.RData")
load("./data/forModels/bigramsWeekBeforeDFM.RData")
load("./data/forModels/beforeDFMPolitical.RData")
load("./data/forModels/weekBeforeDFMPolitical.RData")


# TURNOUT -----------------------------------------------------------------------

# prep data
pmxid <- docvars(weekBeforeDFMPolitical)

top1000before <- convert(weekBeforeDFMPolitical, to = "data.frame")
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

# hyperparameters
bestTune <- best.tune(svm, 
                train.x = trainDataTop1000Balanced[ ,-1],
                train.y = trainDataTop1000Balanced[ ,1],
                kernel = "linear",
                ranges = list(cost = 10^(-1:2),
                              gamma = c(0.5, 1, 2)))

## top 1000 before: cost 10, gamma 0.5
## top 1000 week before: cost 10, gamma 0.5
## bigrams before: cost 10, gamma 0.5
## bigrams week before: cost 10, gamma 0.5
## political before: cost 1, gamma 0.5
## political week before: cost 1, gamma 0.5

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
f1 <- F1(precision, recall)
metricsTurnout <- metrics(accuracy, precision, recall, f1) 

# feature importance
w <- t(svmTop1000$coefs) %*% svmTop1000$SV                
w <- apply(w, 2, function(v){sqrt(sum(v^2))}) 
w <- sort(w, decreasing = T)
topWeights <- data.frame(head(w, 20))
topWeights <- topWeights %>% 
  rownames_to_column("search_term") %>% 
  rename("weight" = `head.w..20.`)

svmPlot <- ggplot(data = topWeights, 
                        aes(x = reorder(search_term, weight), y = weight)) + 
  geom_bar(stat = "identity", aes(fill = search_term)) +
  coord_flip() + 
  labs(x = "Query",
       y = "Weight",
       title = "SVM FI")


# VOTE CHOICE ------------------------------------------------------------------

# prep data
pmxid <- docvars(weekBeforeDFMPolitical)

top1000before <- convert(weekBeforeDFMPolitical, to = "data.frame")
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

# hyperparameters
bestTune <- best.tune(svm, 
                      train.x = trainDataTop1000Balanced[ ,-1],
                      train.y = trainDataTop1000Balanced[ ,1],
                      kernel = "linear",
                      ranges = list(cost = 10^(-1:2),
                                    gamma = c(0.5, 1, 2)))

## top 1000 before: cost 10, gamma 0.5
## top 1000 week before: cost 10, gamma 0.5
## bigrams before: cost 10, gamma 0.5
## bigrams week before: cost 10, gamma 0.5
## political before: cost 10, gamma 0.5
## political week before: cost 100, gamma 0.5

# modeling
svmTop1000 <- svm(voteChoice ~ ., 
                  data = trainDataTop1000Balanced,
                  cost = 100,
                  gamma  = 0.5,
                  kernel = "linear")

# prediction
preds <- predict(svmTop1000, testDataTop1000[,-1])
testY <- as.numeric(as.character(testDataTop1000$voteChoice))

# metrics
accuracy <- acc(preds, testY)
precision <- prec(preds, testY)
recall <- rec(preds, testY)
f1 <- F1(precision, recall)
metricsVote <- metrics(accuracy, precision, recall, f1)

# feature importance
w <- t(svmTop1000$coefs) %*% svmTop1000$SV                
w <- apply(w, 2, function(v){sqrt(sum(v^2))}) 
w <- sort(w, decreasing = T)
topWeights <- data.frame(head(w, 20))
topWeights <- topWeights %>% 
  rownames_to_column("search_term") %>% 
  rename("weight" = `head.w..20.`)

svmPlot <- ggplot(data = topWeights, 
                        aes(x = reorder(search_term, weight), y = weight)) + 
  geom_bar(stat = "identity", aes(fill = search_term)) +
  coord_flip() + 
  labs(x = "Query",
       y = "Weight",
       title = "SVM FI")