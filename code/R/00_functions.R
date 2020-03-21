library(tidyverse)
library(kableExtra)

# drop duplicates
dropDuplicates <- function(x){
  x <- x[!duplicated(x$pmxid), ]
}

# metrics
acc <- function(x, y){
  1 - (as.numeric(sum(x != y, na.rm = TRUE)) / length(y))
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

F1 <- function(precision, recall){
  (2 * precision * recall) / (precision + recall)
}

metrics <- function(accuracy, precision, recall, F1){
  data.frame(accuracy, precision, recall, F1) %>% 
    kable() %>% 
    kable_styling()
}

# negate
'%ni%' <- Negate('%in%')
