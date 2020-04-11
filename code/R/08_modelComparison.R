library(tidyverse)
library(kableExtra)
library(wesanderson)

modelResults <- read.csv("./data/modelResults.csv")

modelResults <- modelResults %>% 
  mutate(accuracy = accuracy / 100,
         f1score = f1score / 100)

tableData <- read.csv("./data/modelResultsTable.csv")

# TURNOUT ----------------------------------------------------------

# accuracy
baseAccuracyTurnout <- 0.93

modelResults %>% 
  filter(researchQ == "turnout") %>% 
  ggplot(aes(x = model,
             y = accuracy)) +
  geom_bar(stat = "identity",
           aes(fill = model)) +
  geom_hline(yintercept = baseAccuracyTurnout, 
             linetype = "dashed") +
  scale_fill_manual(values = wes_palette("IsleofDogs1", 
                                         12, 
                                         type = "continuous")) +
  geom_text(aes(label = round(accuracy, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = 2,
            size = 5) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0)) + 
  labs(title = "Model Comparison: Turnout",
       y = "Accuracy")

# f1
baseF1Turnout <- 0.95

modelResults %>% 
  filter(researchQ == "turnout") %>% 
  ggplot(aes(x = model,
             y = f1score)) +
  geom_bar(stat = "identity",
           aes(fill = model)) +
  geom_hline(yintercept = baseF1Turnout, 
             linetype = "dashed") +
  scale_fill_manual(values = wes_palette("IsleofDogs1", 
                                         12, 
                                         type = "continuous")) +
  geom_text(aes(label = round(f1score, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = 2,
            size = 5) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0)) + 
  labs(title = "Model Comparison: Turnout",
       y = "F1 Score")

tableData %>% 
  filter(researchQ == "turnout") %>% 
  select(-researchQ) %>% 
  rename(`F1 Score` = F1.score) %>% 
  kable() %>% 
  kable_styling() %>% 
  add_header_above(c("Model Comparison: Turnout" = 4))

# VOTE CHOICE ----------------------------------------------------------

# accuracy
baseAccuracyVote <- 0.74

modelResults %>% 
  filter(researchQ == "voteChoice") %>% 
  ggplot(aes(x = model,
             y = accuracy)) +
  geom_bar(stat = "identity",
           aes(fill = model)) +
  geom_hline(yintercept = baseAccuracyVote, 
             linetype = "dashed") +
  scale_fill_manual(values = wes_palette("IsleofDogs1", 
                                         12, 
                                         type = "continuous")) +
  geom_text(aes(label = round(accuracy, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = 2,
            size = 5) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0)) + 
  labs(title = "Model Comparison: Party Choice",
       y = "Accuracy")

# f1
baseF1Vote <- 0.65

modelResults %>% 
  filter(researchQ == "voteChoice") %>% 
  ggplot(aes(x = model,
             y = f1score)) +
  geom_bar(stat = "identity",
           aes(fill = model)) +
  geom_hline(yintercept = baseF1Vote, 
             linetype = "dashed") +
  scale_fill_manual(values = wes_palette("IsleofDogs1", 
                                         12, 
                                         type = "continuous")) +
  geom_text(aes(label = round(f1score, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = 2,
            size = 5) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0)) + 
  labs(title = "Model Comparison: Party Choice ",
       y = "F1 Score")

tableData %>% 
  filter(researchQ == "voteChoice") %>% 
  select(-researchQ) %>% 
  rename(`F1 Score` = F1.score) %>% 
  kable() %>% 
  kable_styling() %>% 
  add_header_above(c("Model Comparison: Party Choice" = 4))


  