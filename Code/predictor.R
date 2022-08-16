library(ggplot2)
library(dplyr)
library(tidyr)
library(melt)

teamsForSeason <- c(
  "1. FC Köln",
  "1. FC Union Berlin",
  "1. FSV Mainz 05",
  "TSG 1899 Hoffenheim",
  "Bayer Leverkusen",
  "Bayern München",
  "Bor. Möchengladbach",
  "Borussia Dortmund",
  "Eintracht Frankfurt",
  "FC Augsburg",
  "FC Schalke 04",
  "Hertha BSC",
  "RB Leipzig",
  "SC Freiburg",
  "VfB Stuttgart",
  "VfL Bochum",
  "VfL Wolfsburg",
  "Werder Bremen"
)


matches <- read.csv("../data/games.csv", sep=";")

filteredMatchesHome <- matches %>% filter(Team.1 %in% teamsForSeason)
filteredMatchesAway <- matches %>% filter(Team.2 %in% teamsForSeason)

goalsShotHome <- aggregate(matches$Home, by=list(Club=matches$Team.1,matches$Startjahr), FUN=sum)
goalsShotAway <- aggregate(matches$Guest, by=list(Club=matches$Team.2,matches$Startjahr), FUN=sum)
goalsConcededHome <- aggregate(matches$Home, by=list(Club=matches$Team.2,matches$Startjahr), FUN=sum)
goalsConcededAway <- aggregate(matches$Guest, by=list(Club=matches$Team.1,matches$Startjahr), FUN=sum)

matchesOfCurrentTeams <- rbind(filteredMatchesHome,filteredMatchesAway)
matchesOfCurrentTeams <- unique(matchesOfCurrentTeams)



season2018 <- matchesOfCurrentTeams %>% filter(Startjahr == 2018)
season2019 <- matchesOfCurrentTeams %>% filter(Startjahr == 2019)
season2020 <- matchesOfCurrentTeams %>% filter(Startjahr == 2020)


melt_results <- function(results_df) {
  results_df %>%
    # select only relevant columns
    select(Team.1, Team.2,Home, Guest) %>%
    gather(location, team,-Home, -Guest) %>%
    # calculate goals for/against the team
    mutate(g_for = case_when(
      location == "home" ~ Home,
      location == "away" ~ Guest
    )) %>%
    mutate(g_ag = case_when(
      location == "home" ~ Guest,
      location == "away" ~ Home
    )) 
}

goalData <- matches %>%
  select(Team.1, Team.2 = visitor, Home, Away = vgoal) %>%
  # munge
  melt_results() %>%
  select(-hgoal, -agoal) %>%
  mutate(data = "real")

goalData <- matches %>%
  select(Team.1,Team.2,Guest, Home) %>% 
  melt_results() %>%
  mutate(data = "real")

goalsOverview <- ggplot(matches) +
  geom_density(adjust = 8, alpha = 0.5,aes(x = Home))+
  geom_density(adjust = 8, alpha = 0.5,aes(x = Guest))+
  scale_fill_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = 0:8)
goalsOverview




