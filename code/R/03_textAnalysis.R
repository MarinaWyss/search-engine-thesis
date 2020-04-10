library(tidyverse)
library(tidytext)
library(textdata)
library(sentimentr)
library(quanteda)
library(wesanderson)

# setwd()
load("./data/preppedFullData.RData")

##################################
############# PREP ###############
##################################

# tokens
searchTokens <- fullDataSet %>% 
  unnest_tokens(word, search_term) 

# remove stopwords
'%ni%' <- Negate('%in%')

searchTokens <- searchTokens %>%
  anti_join(get_stopwords()) %>% 
  filter(word %ni% c("http", "https")) %>% 
  mutate(word = gsub("[[:punct:]]", " ", word),
         hasNum = str_extract_all(word, "[[:digit:]]+"),
         wordClean = ifelse(hasNum == "character(0)", word, NA)) %>% 
  filter(!is.na(wordClean)) %>% 
  select(-wordClean, -hasNum)

##################################
########### KEYWORDS #############
##################################

# most common queries
mostCommon <- searchTokens %>% 
  count(word, sort = TRUE)

# most common queries by partisanship
partisanQueries <- searchTokens %>% 
  filter(voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                TRUE ~ "Non-Voter")) %>% 
  group_by_(~ voteChoice) %>%
  count(word, sort = TRUE) %>% 
  slice(1:10) 

pal <- c("#9986a5", "#875f62", "#824c34", "#b0915b",
         "#a3935b", "#393324", "#514d4f", "#c2babd",
         "#b7afad", "#8d8680")

partisanQueries %>% 
  filter(voteChoice == "Democrat") %>% 
  ggplot(aes(x = reorder(word, -n), 
           y = n)) + 
    geom_bar(stat = "identity", 
             aes(fill = forcats::fct_inorder(word))) +
    geom_text(aes(label = n, 
              vjust = 2,
              size = 7)) +
    scale_fill_manual(values = pal) +
    theme_bw() +
    theme(title = element_text(size = 15),
          legend.position = "none",
          axis.title.x = element_blank(),
          text = element_text(size = 20)) + 
    labs(x = "Query",
         y = "Number of Searches",
         title = "Top 10 Query Terms: Democrats")

##################################
########## WORDSCORES ############
##################################

# turnout terms
wsTokensTurnout <- fullDataSet %>% 
  select(pmxid, turnout, date, search_term) %>% 
  select(-date) %>% 
  rename(text = search_term) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(text, collapse = " "),
            turnout = min(turnout))

wsCorpusTurnout <- corpus(wsTokensTurnout$text)
docnames(wsCorpusTurnout) <- wsTokensTurnout$pmxid

wsDFMTurnout <- dfm(wsCorpusTurnout, 
                    remove_punct = TRUE, 
                    remove_numbers = TRUE, 
                    remove_symbols = TRUE,
                    remove = c(stopwords("english"), 
                               "http", "https"))

wsDFMTurnout <- dfm_trim(wsDFMTurnout, min_docfreq = 2)

wsModelTurnout <- textmodel_wordscores(wsDFMTurnout, 
                                       wsTokensTurnout$turnout, 
                                       smooth = 0.5)

swTurnout <- sort(coef(wsModelTurnout))

wsNonTerms <- head(swTurnout, 10)
wsVoteTerms <- tail(swTurnout, 10)

# party choice
wsTokensParty <- fullDataSet %>% 
  select(pmxid, voteChoice, date, search_term) %>% 
  filter(voteChoice == 1 | voteChoice == 2) %>% 
  mutate(voteChoice = ifelse(voteChoice == 1, 1, -1)) %>% 
  select(-date) %>% 
  rename(text = search_term) %>% 
  group_by(pmxid) %>% 
  summarise(text = paste(text, collapse = " "),
            voteChoice = min(voteChoice))

wsCorpusParty <- corpus(wsTokensParty$text)
docnames(wsCorpusParty) <- wsTokensParty$pmxid

wsDFMParty <- dfm(wsCorpusParty, 
            remove_punct = TRUE, 
            remove_numbers = TRUE, 
            remove_symbols = TRUE,
            remove = c(stopwords("english"), 
                       "http", "https"))

wsDFMParty <- dfm_trim(wsDFMParty, min_docfreq = 2)

wsModelParty <- textmodel_wordscores(wsDFMParty, 
                                     wsTokensParty$voteChoice, 
                                     smooth = 0.5)

swParty <- sort(coef(wsModelParty))

wsDemTerms <- head(swParty, 10) # dems -1
wsRepTerms <- tail(swParty, 10) # reps 1


##################################
###########  KEYNESS  ############
##################################

# turnout
keynessTurnout <- fullDataSet %>% 
  select(turnout, date, search_term) %>% 
  select(-date) %>% 
  rename(text = search_term) %>% 
  group_by(turnout) %>% 
  summarise(text = paste(text, collapse = " ")) %>% 
  mutate(turnout = ifelse(turnout == 1, "Voters", "Non-Voters")) 

keynessCorpusTurnout <- corpus(keynessTurnout$text)
docnames(keynessCorpusTurnout) <- keynessTurnout$turnout
docvars(keynessCorpusTurnout, "turnout") <- keynessTurnout$turnout

keynessDFMTurnout <- dfm(keynessCorpusTurnout, 
                       groups = "turnout",
                       remove_punct = TRUE, 
                       remove_numbers = TRUE, 
                       remove_symbols = TRUE,
                       remove = c(stopwords("english"), 
                                  "http", "https"))

keynessModelTurnout <- textstat_keyness(keynessDFMTurnout, target = "Voters")

keynessTurnoutData <- textplot_keyness(keynessModelTurnout)$data

ggplot(keynessTurnoutData, 
       aes(x = reorder(feature, keyness),
           y = keyness)) +
  geom_bar(stat = "identity", 
           aes(fill = right)) +
  scale_fill_manual(values = c("#a3935b", "#9986a5"),
                    labels = c("Non-Voters", "Voters")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        text = element_text(size = 12)) +
  labs(x = "Chi-Squared",
       title = "Top Terms Associated with Turnout",
       fill = "")
  

# party choice
keynessParty <- fullDataSet %>% 
  select(voteChoice, date, search_term) %>% 
  filter(voteChoice == 1 | voteChoice == 2) %>% 
  select(-date) %>% 
  rename(text = search_term) %>% 
  group_by(voteChoice) %>% 
  summarise(text = paste(text, collapse = " ")) %>% 
  mutate(party = ifelse(voteChoice == 1, "Republican", "Democrat")) 
  
keynessCorpusParty <- corpus(keynessParty$text)
docnames(keynessCorpusParty) <- keynessParty$party
docvars(keynessCorpusParty, "party") <- keynessParty$party

keynessDFMParty <- dfm(keynessCorpusParty, 
                       groups = "party",
                       remove_punct = TRUE, 
                       remove_numbers = TRUE, 
                       remove_symbols = TRUE,
                       remove = c(stopwords("english"), 
                             "http", "https"))

keynessModelParty <- textstat_keyness(keynessDFMParty, target = "Republican")

keynessVoteData <- textplot_keyness(keynessModelParty)$data 

ggplot(keynessVoteData, 
       aes(x = reorder(feature, keyness),
           y = keyness)) +
  geom_bar(stat = "identity", 
           aes(fill = right)) +
  scale_fill_manual(values = c("#a3935b", "#9986a5"),
                    labels = c("Democrats", "Republicans")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        text = element_text(size = 12)) +
  labs(x = "Chi-Squared",
       title = "Top Terms Associated with Party Choice",
       fill = "")

##################################
########### SENTIMENT ############
##################################

# sentiment binary
bing <- get_sentiments("bing")

sentimentBing <- searchTokens %>%
  inner_join(bing)

sentimentBingParty <- sentimentBing %>% 
  select(pmxid, voteChoice, word, sentiment) %>% 
  mutate(sentimentBinary = ifelse(sentiment == "negative", 0, 1)) %>% 
  group_by(voteChoice) %>% 
  summarize(meanSentiment = mean(sentimentBinary)) %>% 
  filter(!is.na(voteChoice) & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 4 ~ "Non Voter"))

sentimentPlotParty <- ggplot(data = sentimentBingParty, 
                             aes(x = voteChoice, y = meanSentiment)) +
  geom_bar(stat = "identity", 
           aes(fill = voteChoice)) + 
  geom_text(aes(label = round(meanSentiment, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = 2,
            size = 7) +
  scale_fill_manual(values = c("#a3935b", "#875f62", "#9986a5")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 22)) + 
  labs(title = "Mean Bing Sentiment Score By Partisanship",
       y = "Mean Bing Sentiment Score")

# sentiment NRC
nrc <- get_sentiments("nrc")

sentimentNRC <- searchTokens %>%
  inner_join(nrc)

sentimentNRCParty <- sentimentNRC %>% 
  select(pmxid, voteChoice, word, sentiment) %>% 
  filter(!is.na(voteChoice) & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 4 ~ "Non Voter")) %>% 
  filter(sentiment != "positive" & sentiment != "negative")

pal <- wes_palette("IsleofDogs1", 8, type = "continuous")

sentimentNRCPlotDem <- sentimentNRCParty %>% 
  filter(voteChoice == "Democrat") %>% 
  group_by(sentiment) %>% 
  summarize(countSentiment = length(sentiment)) %>% 
  ggplot(aes(x = sentiment, 
             y = countSentiment)) + 
  geom_bar(stat = "identity", 
           aes(fill = sentiment)) +
  geom_text(aes(label = countSentiment), 
            position = position_dodge(width = 0.9), 
            vjust = 2,
            size = 6) +
  scale_fill_manual(values = pal) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 22)) + 
  labs(y = "Number of Searches",
       title = "NRC Sentiment Distribution: Democrats")

sentimentNRCPlotRep <- sentimentNRCParty %>% 
  filter(voteChoice == "Republican") %>% 
  group_by(sentiment) %>% 
  summarize(countSentiment = length(sentiment)) %>% 
  ggplot(aes(x = sentiment, 
             y = countSentiment)) + 
  geom_bar(stat = "identity", 
           aes(fill = sentiment)) +
  geom_text(aes(label = countSentiment), 
            position = position_dodge(width = 0.9), 
            vjust = 2,
            size = 6) +
  scale_fill_manual(values = pal) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 22)) + 
  labs(y = "Number of Searches",
       title = "NRC Sentiment Distribution: Republicans")

sentimentNRCPlotNon <- sentimentNRCParty %>% 
  filter(voteChoice == "Non Voter") %>% 
  group_by(sentiment) %>% 
  summarize(countSentiment = length(sentiment)) %>% 
  ggplot(aes(x = sentiment, 
             y = countSentiment)) + 
  geom_bar(stat = "identity", 
           aes(fill = sentiment)) +
  geom_text(aes(label = countSentiment), 
            position = position_dodge(width = 0.9), 
            vjust = 2,
            size = 6) +
  scale_fill_manual(values = pal) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 22)) + 
  labs(y = "Number of Searches",
       title = "NRC Sentiment Distribution: Non-voters")

# sentiment sentence-level
sentenceLevel <- fullDataSet %>% 
  mutate(search_term = gsub("[[:punct:]]", " ", search_term))

sentences <- get_sentences(sentenceLevel$search_term)
sentiment <- sentiment(sentences)

summary(sentiment$sentiment)
#    Min.   1st Qu.   Median    Mean    3rd Qu.   Max. 
# -1.89835  0.00000  0.00000  0.01123  0.00000  1.50111 

sentimentPlot <- ggplot(data = sentiment, aes(x = sentiment)) +
  geom_histogram(bins = 50)

fullDataSet$sentiment <- sentiment$sentiment

sentenceSentiment <- fullDataSet %>% 
  mutate(sentiment_category = case_when(sentiment > 0 ~ "Positive", 
                                        sentiment < 0 ~ "Negative",
                                        TRUE ~ "Neutral"))


sentimentSentenceParty <- sentenceSentiment %>% 
  select(pmxid, voteChoice, sentiment_category) %>% 
  filter(!is.na(voteChoice) & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 4 ~ "Non Voter")) %>% 
  group_by(voteChoice, sentiment_category) %>% 
  summarize(countSentiment = length(sentiment_category)) 


sentimentSentencePlot <- sentimentSentenceParty %>% 
  ggplot(aes(x = sentiment_category, 
             y = countSentiment)) + 
  geom_bar(stat = "identity", 
           aes(fill = sentiment_category)) +
  geom_text(aes(label = countSentiment), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5,
            size = 4) +
  scale_fill_manual(values = c("#a3935b", "#875f62", "#9986a5")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 1.0, hjust = 1.0),  
        text = element_text(size = 18)) + 
  facet_wrap( ~ voteChoice,
              scales = "free_y") +
  labs(x = "Sentiment",
       y = "Number of Searches",
       title = "Sentiment Distribution By Partisanship")

