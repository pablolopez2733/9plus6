library(readr)
library(purrr)
library(tidyr)
library(dplyr)
extrafont::loadfonts(device="win")
library(ggplot2)
library(RCurl)
library(stringr)
library(ggthemes)
library(extrafont)
library(ggdark)
library(ggimage)
library(here)

loadfonts(device = "win")
# SETUP de TEMPORADA COMPLETA
epl_full <- read_csv("C:/Users/pablo/Desktop/epl2020.txt")
epl_full <- epl_full %>%
  separate(Score, c("h_goals", "a_goals"), "–")
epl_full$h_Wins <- ifelse(epl_full$h_goals > epl_full$a_goals, 1, 0)
epl_full$a_wins <- ifelse(epl_full$h_goals < epl_full$a_goals, 1, 0)
epl_full$draw <- ifelse(epl_full$h_goals == epl_full$a_goals, 1, 0)

#Separamos por local y visita
home <- epl_full %>%
  group_by(Home) %>%
  summarise(
    h_xG = mean(xG),
    h_xGA = mean(xG_1),
    HGoals = sum(as.integer(h_goals)),
    HWins = sum(h_Wins),
    HDraws = sum(draw)
    
  ) %>%
  mutate(HPts = (HWins*3) + HDraws)%>%
  rename(Team = Home)


away <- epl_full %>%
  group_by(Away) %>%
  summarise(
    a_xG = mean(xG_1),
    a_xGA = mean(xG),
    AGoals = sum(as.integer(a_goals)),
    AWins = sum(a_wins),
    ADraws = sum(draw)
  ) %>%
  mutate(APts = (AWins*3)+ADraws) %>%
  rename(Team = Away)

#Quitamos NA's
home <- head(home,-1)
away <- head(away,-1)

#Lo colapsamos a una
season <- merge(home,away,by="Team")
season <- season %>% 
  mutate(HPR = HPts / (HPts + APts) ) %>%
  mutate(ppg_home = HPts / 19) %>%
  mutate(ppg_away = APts / 19) %>%
  mutate(Pts = APts + HPts) %>%
  arrange(desc(Pts))


season$position = NA
season$position[1:4] = "Champions"
season$position[5:6] = "Europa"
season$position[7:17] = "Middle"
season$position[18:20] = "Relegation"

#Grafica:
#png(file="homeawayEPL.png", width = 1200, height = 800, units = "px",res = 300)
gp9 <-
    ggplot(season, aes(x = ppg_home, y = ppg_away, colour=factor(position)) ) + 
    geom_point(size = 3) +
    geom_hline(yintercept = mean(season$ppg_away), color = "#adadad", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept =  mean(season$ppg_home), color = "#adadad", linetype = "dashed", alpha=0.5) +
    scale_colour_manual(breaks=c("Champions", "Europa","Middle", "Relegation"),
                        labels=c("Champions League", "Europa League","Middle", "Relegated"),
                        values=c("#6aff00", "#00ddff","#ffa200", "#ff0b03")) +
    guides(colour=guide_legend(title=NULL)) +
    xlim(0.4, 3.1)+
    ylim(0,2.5)+
    dark_theme_gray() +
    
    theme(
      aspect.ratio = 9 / 16,
      plot.title = element_text(size = 14, family = "Calibri",color = "white",hjust = 0),
      plot.subtitle = element_text(size = 8, family = "Calibri",color = "white",hjust = 0),
      axis.title = element_text(size = 9, family = "Calibri",color = "#adadad"),
      plot.caption = element_text(size=8, family = "Serif",color = "#adadad",hjust = 0),
      legend.text = element_text(size = 8, family = "Calibri",color = "#adadad"),
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.key.size =unit(.75,"line"),
      axis.text = element_text(size = 9, family = "Trebuchet MS",color = "#adadad"),
      panel.grid = element_line(size=.11))+
    ggrepel::geom_text_repel(aes(label = Team), size = 3,colour = 'white',family="Calibri")+
    labs(x='Home Points Per Game', y='Away Points Per Game',
         title = "Home PPG vs Away PPG",
         subtitle = 'Premier League 2019-2020 Season',
         caption = "Data: FB Reference | Plot by Pablo L. Landeros @Landeros_p33" )

gp9
ggsave("homevawayEPL.png", plot = gp9, width = 11, units = "cm")

#Con imagenes
crests <- read_csv("C:/Users/pablo/Desktop/GithubRepos/9plus6/HomeAdvantage/crests.csv")
season <- merge(season,crests,by="Team")



haepl <-
  ggplot(season, aes(x = ppg_home, y = ppg_away)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = mean(season$ppg_away), color = "#adadad", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(season$ppg_home), color = "#adadad", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = season$url),asp=16/9)+
  scale_colour_manual(breaks=c("Champions", "Europa","Middle", "Relegation"),
                      labels=c("Champions League", "Europa League","Middle", "Relegated"),
                      values=c("#6aff00", "#00ddff","#ffa200", "#ff0b03")) +
  guides(colour=guide_legend(title=NULL)) +
  xlim(0.4, 3.1)+
  ylim(0,2.5)+
  dark_theme_gray() +
  
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, family = "Calibri",color = "white",hjust = 0),
    plot.subtitle = element_text(size = 8, family = "Calibri",color = "white",hjust = 0),
    axis.title = element_text(size = 9, family = "Calibri",color = "#adadad"),
    plot.caption = element_text(size=8, family = "Serif",color = "#adadad",hjust = 0),
    legend.text = element_text(size = 8, family = "Calibri",color = "#adadad"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.key.size =unit(.75,"line"),
    axis.text = element_text(size = 9, family = "Trebuchet MS",color = "#adadad"),
    panel.grid = element_line(size=.11))+
    labs(x='Home Points Per Game', y='Away Points Per Game',
       title = "Home PPG vs Away PPG",
       subtitle = 'Premier League 2019-2020 Season',
       caption = "Data: FB Reference | Plot by Pablo L. Landeros @Landeros_p33" )+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
haepl
ggsave(filename = here::here("/Desktop/GithubRepos/9plus6/HomeAdvantage/logo_plot.png"), 
       height = 6, width = 8)



#dev.off()
#Ahora todo esto por cuarentena
quarantine <- subset(epl_full, Date > as.Date("2020-03-09") )


