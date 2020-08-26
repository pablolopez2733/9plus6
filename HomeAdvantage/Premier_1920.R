library(readr)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RCurl)
epl2020 <- read_csv("C:/Users/pablo/Desktop/GithubRepos/9plus6/HomeAdvantage/epl2020.csv")


#SETUP-----------------------------------------------------------------
#separamos por local y visita
home <- epl2020 %>%
  filter(h_a=="h") %>%
  group_by(teamId) %>%
  summarise(
    h_wins = sum(wins),
    h_draws = sum(draws),
    h_losses = sum(loses),
    h_points = sum(pts),
    h_goals = sum(tot_goal),
    h_xG = mean(xG),
    h_xGA = mean(xGA)
    
  )

away <- epl2020 %>%
  filter(h_a=="a") %>%
  group_by(teamId) %>%
  summarise(
    a_wins = sum(wins),
    a_draws = sum(draws),
    a_losses = sum(loses),
    a_points = sum(pts),
    a_goals = sum(tot_goal),
    a_xG = mean(xG),
    a_xGA = mean(xGA)
    
  )


  