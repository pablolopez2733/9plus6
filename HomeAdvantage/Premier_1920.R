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
library(ggrepel)
library(gganimate)
library(moments)   #install.packages("moments")
library(lubridate)
library(formattable)

loadfonts(device = "win")
# SETUP de TEMPORADA COMPLETA
epl_full <- read_csv("C:/Users/pablo/Desktop/epl2020.txt")
epl_full <- epl_full %>% filter(!is.na(Wk))
epl_full <- epl_full %>%
  separate(Score, c("h_goals", "a_goals"), "-")
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
  ggplot(season, aes(x = ppg_home, y = ppg_away,label = position)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = mean(season$ppg_away), color = "#adadad", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(season$ppg_home), color = "#adadad", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = season$url),asp=16/9)+
  geom_text_repel(data = subset(season,(position == "Champions" | position == "Europa")),
                  box.padding = unit(0.75, "lines"),
                  size = 4,colour = 'white',family="Trebuchet MS")+
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
people <- subset(epl_full, Date <= as.Date("2020-03-09"))

#QUARANTINE------------------------------------------------------------------
home <- quarantine %>%
  group_by(Home) %>%
  summarise(
    h_xG = mean(xG),
    h_xGA = mean(xG_1),
    HGoals = sum(as.integer(h_goals)),
    HWins = sum(h_Wins),
    HDraws = sum(draw),
    HGames = n()
    
  ) %>%
  mutate(HPts = (HWins*3) + HDraws)%>%
  rename(Team = Home)


away <- quarantine %>%
  group_by(Away) %>%
  summarise(
    a_xG = mean(xG_1),
    a_xGA = mean(xG),
    AGoals = sum(as.integer(a_goals)),
    AWins = sum(a_wins),
    ADraws = sum(draw),
    AGames = n()
  ) %>%
  mutate(APts = (AWins*3)+ADraws) %>%
  rename(Team = Away)

#Quitamos NA's
#home <- head(home,-1)
#away <- head(away,-1)

#Lo colapsamos a una
nopeople <- merge(home,away,by="Team")
nopeople <- nopeople %>% 
  mutate(HPR = HPts / (HPts + APts) ) %>%
  mutate(ppg_home = HPts / HGames) %>%
  mutate(ppg_away = APts / AGames) %>%
  mutate(Pts = APts + HPts) %>%
  arrange(desc(Pts))

#Ahora con gente------------------------------------------------------------
home <- people %>%
  group_by(Home) %>%
  summarise(
    h_xG = mean(xG),
    h_xGA = mean(xG_1),
    HGoals = sum(as.integer(h_goals)),
    HWins = sum(h_Wins),
    HDraws = sum(draw),
    HGames = n()
    
  ) %>%
  mutate(HPts = (HWins*3) + HDraws)%>%
  rename(Team = Home)


away <- people %>%
  group_by(Away) %>%
  summarise(
    a_xG = mean(xG_1),
    a_xGA = mean(xG),
    AGoals = sum(as.integer(a_goals)),
    AWins = sum(a_wins),
    ADraws = sum(draw),
    AGames = n()
  ) %>%
  mutate(APts = (AWins*3)+ADraws) %>%
  rename(Team = Away)

#Quitamos NA's
#home <- head(home,-1)
#away <- head(away,-1)

#Lo colapsamos a una
gente <- merge(home,away,by="Team")
gente <- gente %>% 
  mutate(HPR = HPts / (HPts + APts) ) %>%
  mutate(ppg_home = HPts / HGames) %>%
  mutate(ppg_away = APts / AGames) %>%
  mutate(Pts = APts + HPts) %>%
  arrange(desc(Pts))

nogente <- nopeople
nogente$HPR[which(nogente$Team == "Norwich City")] <- 0

#-------------------------------------------------------------
goles_sin_gente <- mean(as.numeric(quarantine$h_goals))
goles_con_gente <- mean(as.numeric(people$h_goals))

xg_sin_gente <- mean(as.numeric(quarantine$xG))
xg_con_gente <- mean(as.numeric(people$xG))

h_wins_gente <- mean(people$h_Wins)
h_wins_sgente <- mean(quarantine$h_Wins)

hpr_con_gente <- mean(nogente$HPR)
hpr_sin_gente <- mean(gente$HPR)

hppg_con_gente <- mean(gente$ppg_home)
hhpg_sin_gente <- mean(nogente$ppg_home)

hwins_con_gente <- mean(people$h_Wins)
hwins_sin_gente <- mean(quarantine$h_Wins)

#Plots-------------------------------------------------------------------
hva_ppg <-
  ggplot(data = nogente, aes(x = ppg_home, y = ppg_away)) + 
  geom_point(colour = "red")+
  geom_point(data = gente, aes(x = ppg_home, y = ppg_away),colour = "#1eddfa")+
  geom_hline(yintercept = mean(gente$ppg_away), color = "#1eddfa", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(gente$ppg_home), color = "#1eddfa", linetype = "dashed", alpha=0.5) +
  geom_hline(yintercept = mean(nogente$ppg_away), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(nogente$ppg_home), color = "red", linetype = "dashed", alpha=0.5) +
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
  labs(x='Puntos por partido de local', y='Puntos por partido de visita',
       title = "Puntos de local vs Puntos de visita",
       subtitle = 'Premier League 2019-2020',
       caption = "Data: FB Reference | Gráfica por Pablo L. Landeros @Landeros_p33" )+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
hva_ppg

ggsave(filename = here::here("hva_ppg.png"), 
       height = 6, width = 8)

#Intento de animación:
mean.hgoals <- epl_full %>%
  group_by(Wk) %>%
  summarise(Home_Goals = mean(as.numeric(h_goals)))

mean.hgoals


static <- ggplot(
  mean.hgoals,
  aes(Wk, Home_Goals)
) +
  geom_line(colour = "#1eddfa") +
  labs(x = "Semana", y = "Goles anotados en casa") +
  theme(legend.position = "top")+
  dark_theme_gray()+
  geom_vline(xintercept =  29, color = "orange", linetype = "dashed", alpha=0.5) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

static + 
  geom_point() +
  transition_reveal(Wk)

#Now by date:
mean.hgoals <- epl_full %>%
  group_by(Date) %>%
  summarise(Home_Goals = mean(as.numeric(h_goals)))

mean.hgoals


static <- ggplot(
  mean.hgoals,
  aes(as.Date(Date), Home_Goals)
) +
  geom_line(colour = "#1eddfa") +
  xlim(as.Date(c("2019-08-01", "2020-08-01")))+
  ylim(c(0,6))+
  labs(x = "Fecha", y = "Goles anotados en casa") +
  theme(legend.position = "top")+
  dark_theme_gray()+
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 16, family = "Calibri",color = "white",hjust = 0),
    plot.subtitle = element_text(size = 10, family = "Calibri",color = "white",hjust = 0),
    axis.title = element_text(size = 14, family = "Calibri",color = "#adadad"),
    plot.caption = element_text(size=12, family = "Serif",color = "#adadad",hjust = 0),
    legend.text = element_text(size = 8, family = "Calibri",color = "#adadad"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.key.size =unit(.75,"line"),
    axis.text = element_text(size = 14, family = "Trebuchet MS",color = "#adadad"),
    panel.grid = element_line(size=.11))+
  geom_vline(xintercept =  as.Date("2020-06-17"), color = "orange", linetype = "dashed", alpha=0.5)+
  geom_vline(xintercept =  as.Date("2020-03-09"), color = "orange", linetype = "dashed", alpha=0.5)+
  geom_text(aes(label = "SUSPENSIÓN DE PARTIDOS POR COVID-19"),x = as.Date("2020-03-15"),y = 5.5,vjust = 0,hjust= 0,colour="white",size = 3) +
  labs(x='Fecha', y='Goles por partido de los equipos locales',
       title = "La presencia de aficionados sí afectó el número de goles que anota el equipo local. ",
       subtitle = 'Premier League 2019-2020',
       caption = "Data: FB Reference | Gráfica por Pablo L. Landeros @Landeros_p33" )


a <- static + 
  geom_point(size = 3) +
  transition_reveal(as.Date(Date))
animate(a,
        fps = 10,
        duration = 15,
        height = 800, width = 1000)

anim_save(filename = here::here("Desktop/GithubRepos/9plus6/HomeAdvantage/hgoals.gif"), 
          height = 800, width = 1000,unit = "px")


#Tabla hppg---------------------------------------------------------------------
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"


prepandemia <- gente %>% 
  select(Team,ppg_home)
pandemia <- nogente %>% 
  select(Team,ppg_home)

tabla <- merge(prepandemia,pandemia,by="Team")
tabla$diferencia <- round(tabla$ppg_home.y - tabla$ppg_home.x,3) 


improvement_formatter <- formatter("span", 
                                   style = x ~ style(font.weight = "bold", 
                                                     color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
                                   x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
)
colnames(tabla) <- c("Equipo","Con Gente","Sin Gente","Diferencia")
tabla$`Con Gente` <- round(tabla$`Con Gente`,3)
tabla$`Sin Gente` <- round(tabla$`Sin Gente`,3)
t <- formattable(tabla, align=c("l","c","c","c"),list("Diferencia" = improvement_formatter))

#Exportar formattable--------------------------------------------------------
library("htmltools")
library("webshot")    

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}



export_formattable(t,here::here("Desktop/GithubRepos/9plus6/HomeAdvantage/table.png"))



