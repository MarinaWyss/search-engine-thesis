library(tidyverse)
library(kableExtra)
library(chron)

# setwd()
load("./data/preppedFullData.RData")
load("./data/forModels/fullSearchesJoined.RData")

length(unique(fullDataSet$pmxid)) # 708
uniqueUsers <- fullDataSet[!duplicated(fullDataSet$pmxid), ]

# voters vs. non-voters
uniqueUsers %>% 
  group_by(turnout) %>% 
  add_count(turnout) %>% 
  mutate(Share = n/length(uniqueUsers$turnout)) %>% 
  select(turnout, n, Share, birthyr, gender, race, 
         educ, marstat, employ, religion,
         ideo5) %>% 
  mutate(`Mean Age` = 2018 - birthyr,
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
         `Percent Women` = gender,
         `Percent White` = white,
         `Percent Married` = married,
         `Percent Full-time` = fullTime,
         `Percent Religious` = religious,
         `Percent With Degree` = hasDegree,
         Ideology = ideo5) %>% 
  select(Turnout, Count, Share, `Mean Age`, `Percent Women`, `Percent White`, `Percent Married`,
         `Percent Full-time`, `Percent With Degree`, `Percent Religious`, Ideology) %>% 
  round(2) %>%
  ungroup() %>% 
  mutate(Turnout = ifelse(Turnout == 1, "Voter", "Non-voter")) %>% 
  kable() %>% 
  kable_styling() %>% 
  add_header_above(c("Descriptive Statistics: Voter vs. Non-voters" = 11))
  
# party choice
voteChoiceFilter <- uniqueUsers %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                         voteChoice == 2 ~ "Democrat")) 
voteChoiceFilter %>% 
  group_by(voteChoice) %>% 
  add_count(voteChoice) %>% 
  mutate(Share = n/length(voteChoiceFilter$voteChoice)) %>% 
  select(voteChoice, n, Share, birthyr, gender, race, 
         educ, marstat, employ, religion,
         ideo5) %>% 
  mutate(`Mean Age` = 2018 - birthyr,
         white = ifelse(race == 1, 1, 0),
         hasDegree = ifelse(educ >= 4, 1, 0),
         married = ifelse(marstat == 1, 1, 0),
         fullTime = ifelse(employ == "Full-time", 1, 0),
         religious = case_when(religion %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 1,
                               TRUE ~ 0)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  rename(Party = voteChoice,
         Count = n, 
         `Percent Women` = gender,
         `Percent White` = white,
         `Percent Married` = married,
         `Percent Full-time` = fullTime,
         `Percent Religious` = religious,
         `Percent With Degree` = hasDegree,
         Ideology = ideo5) %>% 
  select(Party, Count, Share, `Mean Age`, `Percent Women`, `Percent White`, `Percent Married`,
         `Percent Full-time`, `Percent With Degree`, `Percent Religious`, Ideology) %>%
  kable() %>% 
  kable_styling() %>% 
  add_header_above(c("Descriptive Statistics: Republicans vs. Democrats" = 11))


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
uniqueUsers %>% 
  group_by(search_engine) %>% 
  add_count(search_engine) %>% 
  mutate(Share = n/length(uniqueUsers$search_engine)) %>% 
  select(search_engine, n, Share, birthyr, gender, race, 
         educ, marstat, employ, religion,
         ideo5) %>% 
  mutate(`Mean Age` = 2018 - birthyr,
         white = ifelse(race == 1, 1, 0),
         hasDegree = ifelse(educ >= 4, 1, 0),
         married = ifelse(marstat == 1, 1, 0),
         fullTime = ifelse(employ == "Full-time", 1, 0),
         religious = case_when(religion %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ 1,
                               TRUE ~ 0)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  rename(`Search Engine` = search_engine,
         Count = n, 
         `Percent Women` = gender,
         `Percent White` = white,
         `Percent Married` = married,
         `Percent Full-time` = fullTime,
         `Percent Religious` = religious,
         `Percent With Degree` = hasDegree,
         Ideology = ideo5) %>% 
  select(`Search Engine`, Count, Share, `Mean Age`, `Percent Women`, `Percent White`, `Percent Married`,
         `Percent Full-time`, `Percent With Degree`, `Percent Religious`, Ideology) %>%
  kable() %>% 
  kable_styling() %>% 
  add_header_above(c("Descriptive Statistics: By Search Engine Used" = 11))


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
  geom_histogram(aes(fill = voteChoice)) +

searchesParty <- searchesUser %>% 
  group_by(voteChoice) %>% 
  summarize(mean = mean(searches)) 

searchesPlotParty <- ggplot(data = searchesParty, 
                            aes(x = voteChoice, 
                                y = mean)) +
  geom_bar(stat = "identity", 
           aes(fill = voteChoice)) +
  geom_text(aes(label = round(mean, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = 2,
            size = 7) +
  scale_fill_manual(values = c("#a3935b", "#875f62", "#9986a5")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 22)) + 
  labs(title = "Mean Number of Queries By Partisanship",
       y = "Mean Number of Queries")
  

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
  geom_bar(stat = "identity", 
           aes(fill = voteChoice)) +
  geom_text(aes(label = round(mean, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = 2,
            size = 7) +
  scale_fill_manual(values = c("#a3935b", "#875f62", "#9986a5")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(size = 22)) + 
  labs(title = "Mean Query Length By Partisanship",
       y = "Mean Query Length (Characters)")
  


# time of day
fullDataSet %>% 
  filter(!is.na(voteChoice) & voteChoice %in% c(1, 2)) %>% 
  mutate(voteChoice = ifelse(voteChoice == 2, "Democrat", "Republican")) %>% 
  group_by(voteChoice) %>% 
  summarize(`Time of Day` = mean(times(time))) %>% 
  rename(Party = voteChoice) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Average Query Time" = 2))

fullDataSet %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = ifelse(turnout == 1, "Voter", "Non-voter")) %>% 
  group_by(turnout) %>% 
  summarize(`Time of Day` = mean(times(time))) %>% 
  rename(Turnout = turnout) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Average Query Time" = 2))


# searched for register keywords - turnout
fullSearchesJoined %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = ifelse(turnout == 1, "Voter", "Non-voter")) %>% 
  group_by(turnout) %>% 
  summarize(mean_searches = mean(num_register_searches, na.rm = TRUE),
            made_search = mean(searched_register, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  rename(Turnout = turnout,
         `Mean Number of Searches` = mean_searches,
         `Proportion Making Search` = made_search) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Registration-Related Searches" = 3)) 


# searched for register keywords - vote choice
fullSearchesJoined %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat")) %>% 
  group_by(voteChoice) %>% 
  summarize(mean_searches = mean(num_register_searches, na.rm = TRUE),
            made_search = mean(searched_register, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  rename(Party = voteChoice,
         `Mean Number of Searches` = mean_searches,
         `Proportion Making Search` = made_search) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Registration-Related Searches" = 3)) 

# searched for candidate - turnout
fullSearchesJoined %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = ifelse(turnout == 1, "Voter", "Non-voter")) %>% 
  group_by(turnout) %>% 
  summarize(dem_searches = mean(searched_dem_candidate, na.rm = TRUE),
            rep_searches = mean(searched_rep_candidate, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  rename(Turnout = turnout,
         `Mean Dem. Candidate Searches` = dem_searches,
         `Mean Rep. Candidate Searches` = rep_searches) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Candidate Searches" = 3))


# searched for candidate - vote choice
fullSearchesJoined %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat")) %>% 
  group_by(voteChoice) %>% 
  summarize(dem_searches = mean(searched_dem_candidate, na.rm = TRUE),
            rep_searches = mean(searched_rep_candidate, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  rename(Party = voteChoice,
         `Mean Dem. Candidate Searches` = dem_searches,
         `Mean Rep. Candidate Searches` = rep_searches) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Candidate Searches" = 3))


# searched for politician Rep/Dem - turnout
fullSearchesJoined %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = ifelse(turnout == 1, "Voter", "Non-voter")) %>% 
  group_by(turnout) %>% 
  summarize(mean_dem_searches = mean(num_dem_searches, na.rm = TRUE),
            prop_searched_dem = mean(searched_dem, na.rm = TRUE),
            mean_rep_searches = mean(num_rep_searches, na.rm = TRUE),
            prop_searched_rep = mean(searched_rep, na.rm = TRUE)) %>% 
  rename(Turnout = turnout,
         `Mean Dem. Politician Searches` = mean_dem_searches,
         `Proportion Searched Dem. Politician` = prop_searched_dem,
         `Mean Rep. Politician Searches` = mean_rep_searches,
         `Proportion Searched Rep. Politician` = prop_searched_rep) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Politician Searches" = 5)) 

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
  rename(Party = voteChoice,
         `Mean Dem. Politician Searches` = mean_dem_searches,
         `Proportion Searched Dem. Politician` = prop_searched_dem,
         `Mean Rep. Politician Searches` = mean_rep_searches,
         `Proportion Searched Rep. Politician` = prop_searched_rep) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("Politician Searches" = 5)) 


# searched for political keywords - turnout
fullSearchesJoined %>% 
  filter(!is.na(turnout)) %>% 
  mutate(turnout = ifelse(turnout == 1, "Voter", "Non-voter")) %>% 
  group_by(turnout) %>% 
  summarize(mean_searches = mean(num_political_searches, 
                                 na.rm = TRUE),
            mean_searched = mean(searched_politics)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  rename(Turnout = turnout,
         `Mean Political Searches` = mean_searches,
         `Proportion Making Search` = mean_searched) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("General Political Searches" = 3))


# searched for political keywords - vote choice
fullSearchesJoined %>% 
  filter(!is.na(voteChoice) & voteChoice != 4 & voteChoice != 3) %>% 
  mutate(voteChoice = case_when(voteChoice == 1 ~ "Republican",
                                voteChoice == 2 ~ "Democrat")) %>% 
  group_by(voteChoice) %>% 
  summarize(mean_searches = mean(num_political_searches, 
                                 na.rm = TRUE),
            mean_searched = mean(searched_politics)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
    rename(Party = voteChoice,
           `Mean Political Searches` = mean_searches,
           `Proportion Making Search` = mean_searched) %>% 
  kable() %>%  
  kable_styling() %>% 
  add_header_above(c("General Political Searches" = 3))
