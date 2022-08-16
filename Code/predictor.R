library(ggplot2)
library(dplyr)
library(tidyr)
#library(melt)


teamsForSeason <- c(
  "1. FC Köln",
  "1. FC Union Berlin",
  "1. FSV Mainz 05",
  "TSG 1899 Hoffenheim",
  "Bayer 04 Leverkusen",
  "Bayern München",
  "Bor. Mönchengladbach",
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

goalsShotHome <- aggregate(matches$Home, by=list(Club=matches$Team.1), FUN=sum)
avgGoalsShotHome <- aggregate(matches$Home, by=list(Club=matches$Team.1), FUN=mean)
goalsShotAway <- aggregate(matches$Guest, by=list(Club=matches$Team.2), FUN=sum)
avgGoalsShotAway <- aggregate(matches$Guest, by=list(Club=matches$Team.2), FUN=mean)
goalsConcededHome <- aggregate(matches$Home, by=list(Club=matches$Team.2), FUN=sum)
avgGoalsConcededHome <- aggregate(matches$Home, by=list(Club=matches$Team.2), FUN=mean)
goalsConcededAway <- aggregate(matches$Guest, by=list(Club=matches$Team.1), FUN=sum)
avgGoalsConcededAway <- aggregate(matches$Guest, by=list(Club=matches$Team.1), FUN=mean)

matchesOfCurrentTeams <- rbind(filteredMatchesHome,filteredMatchesAway)
matchesOfCurrentTeams <- unique(matchesOfCurrentTeams)

matches <- matches %>% mutate(homeVic =
                                      case_when(Home > Guest ~ 1, 
                                                  Home == Guest ~ 0,
                                                  Home < Guest ~ 0)) %>% mutate(AwayVic =
                                       case_when(Home > Guest ~ 0, 
                                                 Home == Guest ~ 0,
                                                 Home < Guest ~ 1)) %>% mutate(Equal =
                                       case_when(Home > Guest ~ 0, 
                                                 Home == Guest ~ 1,
                                                 Home < Guest ~ 0))

possibiltyOfHomeVictory <- sum(matches$homeVic)/nrow(matches)
possibiltyOfGuestVictory <- sum(matches$AwayVic)/nrow(matches)
possibiltyOfEqual <- sum(matches$Equal)/nrow(matches)

clubs <- data.frame(
  goalsShotHome$Club,
  goalsShotHome$x,
  avgGoalsShotHome$x,
  goalsShotAway$x,
  avgGoalsShotAway$x,
  goalsConcededHome$x,
  avgGoalsConcededHome$x,
  goalsConcededAway$x,
  avgGoalsConcededAway$x
)

clubs <- clubs %>% filter(goalsShotHome.Club %in% teamsForSeason)

meanShotGoals <- mean(matches$Home)
meanConcededGoals <- mean(matches$Guest)


season2018 <- matchesOfCurrentTeams %>% filter(Startjahr == 2018)
season2019 <- matchesOfCurrentTeams %>% filter(Startjahr == 2019)
season2020 <- matchesOfCurrentTeams %>% filter(Startjahr == 2020)


goalsOverview <- ggplot(matches) +
  geom_density(adjust = 8, alpha = 0.5,aes(x=Home)) + geom_density(adjust = 8, alpha = 0.5,aes(x=Guest))

goalsOverview


