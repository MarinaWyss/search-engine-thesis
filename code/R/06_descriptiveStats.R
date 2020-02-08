library(tidyverse)
library(kableExtra)
library(chron)

# setwd()
load("./data/preppedFullData.RData")
load("./data/forModels/fullSearchesJoined.RData")

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


# time of day
timeVote <- fullDataSet %>% 
  filter(!is.na(voteChoice) & voteChoice %in% c(1, 2)) %>% 
  mutate(voteChoice = ifelse(voteChoice == 2, "Democrat", "Republican")) %>% 
  group_by(voteChoice) %>% 
  summarize(timeOfDay = mean(times(time))) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Average time of day for queries" = 2))

timeTurnout <- fullDataSet %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = ifelse(turnout == 1, "voter", "non-voter")) %>% 
  group_by(turnout) %>% 
  summarize(timeOfDay = mean(times(time))) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Average time of day for queries" = 2))


# searched for register keywords - turnout
fullSearchesJoined %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = ifelse(turnout == 1, "voter", "non-voter")) %>% 
  group_by(turnout) %>% 
  summarize(mean_searches = mean(num_register_searches, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Mean Number of Registration-Related Searches" = 2))

turnoutRegister <- fullSearchesJoined %>% 
  mutate(turnout = ifelse(turnout == 1, "voter", "non-voter"),
         searched_register = ifelse(searched_register == 1, "searched_for_term", "no_searches"))

table(turnoutRegister$searched_register, turnoutRegister$turnout) %>% 
  prop.table(2) %>% 
  round(3) %>%
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Turnout and Registration-Related Keywords" = 3)) %>% 
  footnote("Column Percent")

# searched for register keywords - vote choice
fullSearchesJoined %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat")) %>% 
  group_by(voteChoice) %>% 
  summarize(mean_searches = mean(num_register_searches)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Mean Number of Registration-Related Searches" = 2))

voteChoiceRegister <- fullSearchesJoined %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat"),
         searched_register = ifelse(searched_register == 1, 
                                    "searched_for_term", "no_searches"))
  
table(voteChoiceRegister$searched_register, voteChoiceRegister$voteChoice) %>% 
  prop.table(2) %>% 
  round(3) %>%
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Vote Choice and Registration-Related Keywords" = 3)) %>% 
  footnote("Column Percent")


# searched for candidate - turnout
fullSearchesJoined %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = ifelse(turnout == 1, "voter", "non-voter")) %>% 
  group_by(turnout) %>% 
  summarize(dem_searches = mean(searched_dem_candidate, na.rm = TRUE),
            rep_searches = mean(searched_rep_candidate, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Proportion Who Searched For Candidates" = 3))

sum(fullSearchesJoined$searched_dem_candidate, na.rm = TRUE) # 30
sum(fullSearchesJoined$searched_rep_candidate, na.rm = TRUE) # 30


# searched for candidate - vote choice
fullSearchesJoined %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat")) %>% 
  group_by(voteChoice) %>% 
  summarize(dem_searches = mean(searched_dem_candidate, na.rm = TRUE),
            rep_searches = mean(searched_rep_candidate, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Proportion Who Searched For Candidates" = 3))


# searched for politician Rep/Dem - turnout
fullSearchesJoined %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = ifelse(turnout == 1, "voter", "non-voter")) %>% 
  group_by(turnout) %>% 
  summarize(mean_dem_searches = mean(num_dem_searches, na.rm = TRUE),
            prop_searched_dem = mean(searched_dem, na.rm = TRUE),
            mean_rep_searches = mean(num_rep_searches, na.rm = TRUE),
            prop_searched_rep = mean(searched_rep, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Dem/Rep Searches" = 5)) 

sum(fullSearchesJoined$searched_dem, na.rm = TRUE) # 135
sum(fullSearchesJoined$searched_rep, na.rm = TRUE) # 147

# searched for politician Rep/Dem - vote choice
fullSearchesJoined %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat")) %>% 
  group_by(voteChoice) %>% 
  summarize(mean_dem_searches = mean(num_dem_searches, na.rm = TRUE),
            prop_searched_dem = mean(searched_dem, na.rm = TRUE),
            mean_rep_searches = mean(num_rep_searches, na.rm = TRUE),
            prop_searched_rep = mean(searched_rep, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Dem/Rep Searches" = 5)) 


# searched for political keywords - turnout
fullSearchesJoined %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = ifelse(turnout == 1, "voter", "non-voter")) %>% 
  group_by(turnout) %>% 
  summarize(mean_searches = mean(num_political_searches, 
                                 na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Mean Number of Non-Partisan Political Searches" = 2))

turnoutPolitical <- fullSearchesJoined %>% 
  mutate(turnout = ifelse(turnout == 1, "voter", "non-voter"),
         searched_politics = ifelse(searched_politics == 1, "searched_for_term", "no_searches"))

table(turnoutPolitical$searched_politics, turnoutPolitical$turnout) %>% 
  prop.table(2) %>% 
  round(3) %>%
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Turnout and Non-Partisan Political Searches" = 3)) %>% 
  footnote("Column Percent")

# searched for political keywords - vote choice
fullSearchesJoined %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat")) %>% 
  group_by(voteChoice) %>% 
  summarize(mean_searches = mean(num_political_searches, 
                                 na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Mean Number of Non-Partisan Political Searches" = 2))

voteChoicePolitical <- fullSearchesJoined %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat"),
         searched_politics = ifelse(searched_politics == 1, 
                                    "searched_for_term", "no_searches"))

table(voteChoicePolitical$searched_politics, voteChoicePolitical$voteChoice) %>% 
  prop.table(2) %>% 
  round(3) %>%
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Search Behavior: Vote Choice and Non-Partisan Political Searches" = 3)) %>% 
  footnote("Column Percent")