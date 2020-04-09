library(tidyverse)
library(wesanderson)

modelResults <- read.csv("./data/modelResults.csv")

modelResults <- modelResults %>% 
  mutate(accuracy = accuracy / 100,
         f1score = f1score / 100)

# TURNOUT ----------------------------------------------------------
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
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 30, vjust = 1.0, hjust = 1.0)) + 
  labs(title = "Model Comparison: Turnout",
       y = "Accuracy")

# VOTE CHOICE ----------------------------------------------------------
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
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 30, vjust = 1.0, hjust = 1.0)) + 
  labs(title = "Model Comparison: Party Choice",
       y = "Accuracy")
  