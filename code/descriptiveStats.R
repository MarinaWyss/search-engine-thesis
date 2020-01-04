library(tidyverse)
library(kableExtra)

length(unique(fullDataSet$pmxid)) # 708
uniqueUsers <- fullDataSet[!duplicated(fullDataSet$pmxid), ]

# voters vs. non-voters
turnoutTable <- uniqueUsers %>% 
  group_by(turnout) %>% 
  add_count(turnout) %>% 
  mutate(Share = n/length(uniqueUsers$turnout)) %>% 
  select(turnout, n, Share, birthyr, gender, educ, familyIncome, ideo5) %>% 
  type.convert() %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  mutate(Age = 2018 - birthyr) %>% 
  select(-birthyr) %>% 
  rename(Turnout = turnout,
         Count = n, 
         PercentWomen = gender,
         Education = educ,
         Ideology = ideo5) %>% 
  round(2) %>% 
  kable() %>% 
  kable_styling()
  
# party choice
voteChoiceTable <- uniqueUsers %>% 
  filter(!is.na(voteChoice) & voteChoice != 4) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                         voteChoice == 2 ~ "Democrat",
                         voteChoice == 3 ~ "Independent")) %>% 
  group_by(voteChoice) %>% 
  add_count(voteChoice) %>% 
  mutate(Share = n/length(uniqueUsers$voteChoice)) %>% 
  select(voteChoice, n, Share, birthyr, gender, educ, familyIncome, ideo5) %>% 
  mutate(Age = 2018 - birthyr) %>% 
  select(-birthyr) %>% 
  rename(VoteChoice = voteChoice,
         Count = n, 
         PercentWomen = gender,
         Education = educ,
         Ideology = ideo5) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>% 
  kable_styling()

# search engine used
engineTable <- fullDataSet %>% 
  group_by(search_engine) %>% 
  add_count(search_engine) %>% 
  mutate(Share = n/length(fullDataSet$search_engine)) %>% 
  select(search_engine, n, Share, birthyr, gender, educ, familyIncome, ideo5) %>% 
  type.convert() %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  mutate(Age = 2018 - birthyr) %>% 
  select(-birthyr) %>% 
  rename(SearchEngine = search_engine,
         Count = n, 
         PercentWomen = gender,
         Education = educ,
         Ideology = ideo5) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>% 
  kable_styling()

# number of searches
searchesUser <- fullDataSet %>% 
  group_by(pmxid) %>% 
  summarize(searches = length(search_term),
            voteChoice = unique(voteChoice)) %>% 
  filter(!is.na(voteChoice)) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 3 ~ "Independent",
                                voteChoice == 4 ~ "Non Voter"))


summary(searchesUser$searches)
# Min. 1st Qu.  Median  Mean   3rd Qu.    Max. 
# 1.0    16.0    93.5   408.6   411.5  8139.0 

searchesPlot <- ggplot(data = searchesUser, 
                       aes(x = searches)) +
  geom_histogram(aes(fill = voteChoice))

searchesParty <- searchesUser %>% 
  group_by(voteChoice) %>% 
  summarize(mean = mean(searches)) 

searchesPlotParty <- ggplot(data = searchesParty, 
                            aes(x = voteChoice, y = mean)) +
  geom_bar(stat = "identity", aes(fill = voteChoice)) 

# length of searches
searchLengthUser <- fullDataSet %>% 
  mutate(searchLength = nchar(search_term)) %>% 
  group_by(pmxid) %>% 
  summarize(searchLength = mean(searchLength),
            voteChoice = unique(voteChoice)) %>% 
  filter(!is.na(voteChoice)) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
                                voteChoice == 3 ~ "Independent",
                                voteChoice == 4 ~ "Non Voter"))

summary(searchLengthUser$searchLength)
# Min. 1st Qu.  Median   Mean   3rd Qu.  Max. 
# 1.00   19.10   22.12   25.70   25.70  322.12 

searchLengthPlot <- ggplot(data = searchLengthUser, 
                       aes(x = searchLength)) +
  geom_histogram(aes(fill = voteChoice))

searchLengthParty <- searchLengthUser %>% 
  group_by(voteChoice) %>% 
  summarize(mean = mean(searchLength)) 

searchLengthPlotParty <- ggplot(data = searchLengthParty, 
                                aes(x = voteChoice, y = mean)) +
  geom_bar(stat = "identity", aes(fill = voteChoice)) 
