library(tidyverse)
library(kableExtra)

length(unique(fullDataSet$pmxid)) # 708
uniqueUsers <- fullDataSet[!duplicated(fullDataSet$pmxid), ]

# voters vs. non-voters
turnoutTable <- uniqueUsers %>% 
  group_by(turnout) %>% 
  add_count(turnout) %>% 
  mutate(Share = n/length(uniqueUsers$turnout)) %>% 
  select(turnout, n, Share, birthyr, gender, race, 
         educ, marstat, employ, religion,
         ideo5) %>% 
  mutate(Age = 2018 - birthyr,
         white = ifelse(race == 1, 1, 0),
         hasDegree = ifelse(educ >= 4, 1, 0),
         married = ifelse(marstat == 1, 1, 0),
         fullTime = ifelse(employ == "Full-time", 1, 0),
         religious = case_when(religion %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 1,
                               TRUE ~ 0)) %>%
  select(-birthyr, -educ, -race, -marstat, -employ, -religion) %>% 
  type.convert() %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  rename(Turnout = turnout,
         Count = n, 
         PercentWomen = gender,
         PercentWhite = white,
         PercentMarried = married,
         PercentFullTime = fullTime,
         PercentReligious = religious,
         Ideology = ideo5) %>% 
  select(Turnout, Count, Share, Age, PercentWomen, PercentWhite, PercentMarried,
         PercentFullTime, hasDegree, PercentReligious, Ideology) %>% 
  round(2) %>% 
  kable() %>% 
  kable_styling()
  
# party choice
voteChoiceFilter <- uniqueUsers %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                         voteChoice == 2 ~ "Democrat")) 

voteChoiceTable <- voteChoiceFilter %>% 
  group_by(voteChoice) %>% 
  add_count(voteChoice) %>% 
  mutate(Share = n/length(voteChoiceFilter$voteChoice)) %>% 
  select(voteChoice, n, Share, birthyr, gender, race, 
         educ, marstat, employ, religion,
         ideo5) %>% 
  mutate(Age = 2018 - birthyr,
         white = ifelse(race == 1, 1, 0),
         hasDegree = ifelse(educ >= 4, 1, 0),
         married = ifelse(marstat == 1, 1, 0),
         fullTime = ifelse(employ == "Full-time", 1, 0),
         religious = case_when(religion %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 1,
                               TRUE ~ 0)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  rename(VoteChoice = voteChoice,
         Count = n, 
         PercentWomen = gender,
         PercentWhite = white,
         PercentMarried = married,
         PercentFullTime = fullTime,
         PercentReligious = religious,
         Ideology = ideo5) %>% 
  select(VoteChoice, Count, Share, Age, PercentWomen, PercentWhite, PercentMarried,
         PercentFullTime, hasDegree, PercentReligious, Ideology) %>%
  kable() %>% 
  kable_styling()

# search engine by user
engineUsers <- fullDataSet %>% 
  group_by(pmxid) %>% 
  mutate(googleUser = ifelse(search_engine == "Google", 1, 0),
         bingUser = ifelse(search_engine == "Bing", 1, 0),
         duckUser = ifelse(search_engine == "DuckDuckGo", 1, 0),
         yahooUser = ifelse(search_engine == "Yahoo", 1, 0),
         otherUser = ifelse(search_engine == "Other", 1, 0),
         multiUser = ifelse(googleUser + bingUser + duckUser + yahooUser + otherUser > 1, 1, 0))

sum(engineUsers$multiUser) # no users who use multiple search engines

# search engine user demographics
engineTable <- uniqueUsers %>% 
  group_by(search_engine) %>% 
  add_count(search_engine) %>% 
  mutate(Share = n/length(uniqueUsers$search_engine)) %>% 
  select(search_engine, n, Share, birthyr, gender, hasDegree, ideo5) %>% 
  type.convert() %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  mutate(Age = 2018 - birthyr) %>% 
  select(-birthyr) %>% 
  rename(SearchEngine = search_engine,
         Count = n, 
         PercentWomen = gender,
         Ideology = ideo5) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>% 
  kable_styling()

# number of searches
searchesUser <- fullDataSet %>% 
  group_by(pmxid) %>% 
  summarize(searches = length(search_term),
            voteChoice = unique(voteChoice)) %>% 
  filter(!is.na(voteChoice) & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
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
  filter(!is.na(voteChoice) & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat",
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
