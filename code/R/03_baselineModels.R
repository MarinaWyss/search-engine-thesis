library(tidyverse)
library(stargazer)
library(caret)
library(kableExtra)

set.seed(12345)

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

predsTurnout <- turnoutModel %>% predict(turnoutTestData, type = "response")
predictedClassesTurnout <- factor(ifelse(predsTurnout > 0.5, 1, 0))
yTurnout <- turnoutTestData$turnout

accuracyTurnout <- mean(predictedClassesTurnout == yTurnout, na.rm = TRUE)
precisionTurnout <- posPredValue(predictedClassesTurnout, yTurnout, positive = "1")
recallTurnout <- sensitivity(predictedClassesTurnout, yTurnout, positive = "1")
F1Turnout <- (2 * precisionTurnout * recallTurnout) / (precisionTurnout + recallTurnout)

resTableTurnout <- data.frame(accuracy = accuracyTurnout,
                              precision = precisionTurnout, 
                              recall = recallTurnout,
                              F1 = F1Turnout) %>% 
  round(2) %>% 
  kable() %>% 
  kable_styling() %>% 
  add_header_above(c("Logistic Regression - Turnout" = 4))

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

predsParty <- partyModel %>% predict(partyTestData, type = "response")
predictedClassesParty <- factor(ifelse(predsParty > 0.5, 1, 0))
yParty <- partyTestData$voteChoice

accuracyParty <- mean(predictedClassesParty == yParty, na.rm = TRUE)
precisionParty <- posPredValue(predictedClassesParty, yParty, positive = "1")
recallParty <- sensitivity(predictedClassesParty, yParty, positive = "1")
F1Party <- (2 * precisionParty * recallParty) / (precisionParty + recallParty)

resTableParty <- data.frame(accuracy = accuracyParty,
                            precision = precisionParty, 
                            recall = recallParty,
                            F1 = F1Party) %>% 
  round(2) %>% 
  kable() %>% 
  kable_styling() %>% 
  add_header_above(c("Logistic Regression - Vote Choice" = 4))

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
          title = "Odds-ratios Model for Turnout", 
          type = "text")

stargazer(partyModel,
          coef = list(partyModelOR),
          ci = T,
          ci.custom = list(partyModelConf),
          p = c(partyModelp),
          header = FALSE,
          title = "Odds-ratio Model Party Choice", 
          type = "text")