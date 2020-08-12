library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(formattable)
library(tidyr)
library(DT)
library(ggrepel)
library(ggdark)


df <- read_csv("C:/Users/pablo/Desktop/18_19NBA.txt")

pg <- df %>%
  filter(Pos == "PG" | Pos == "SG" & MP >= 2000)

hou <- df %>%
  filter(Tm == "HOU")

tpa <- df[1:20,]

tpa$Player <- sub("\\\\.*", "", tpa$Player)

#GRAFICA SIN NOMBRE-------------------------------------------------------------
tpa_plot1 <- ggplot(tpa, aes(x=MP, y=TPA)) + 
  geom_point(color = "#7df3ed", size=4.5)+
  #geom_text_repel(aes(label=tpa$Player)) +
  #geom_text(label=tpa$Player,vjust=1.5,hjust=.5,colour="black",size=5)+
  ggtitle("Intentos de Tres vs Minutos Jugados")+
  xlim(2100, 3050)+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Minutos Jugados",
    y = "3PA"
  )+
 geom_segment(mapping=aes(x=2700, y=900, xend=2850, yend=1010),
              arrow=arrow(type = "closed"), size=2, color="#e54069")

tpa_plot1+dark_theme_gray()+
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=17),
        axis.title = element_text(size=17),
        plot.title = element_text(size = 20),
        legend.position = "none"
  )


#GRAFICA CON NOMBRE----------------------------------------------------------

tpa_plot <- ggplot(tpa, aes(x=MP, y=TPA)) + 
  geom_point(aes(color=Player), size=4.5)+
  geom_text_repel(aes(label=tpa$Player)) +
  #geom_text(label=tpa$Player,vjust=1.5,hjust=.5,colour="black",size=5)+
  ggtitle("Intentos de Tres vs Minutos Jugados")+
  xlim(2100, 3050)+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Minutos Jugados",
    y = "3PA"
  )
  # geom_segment(mapping=aes(x=32.5, y=54, xend=31, yend=52.4),
  #              arrow=arrow(type = "closed"), size=2, color="red")
tpa_plot+dark_theme_gray()+
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=17),
        axis.title = element_text(size=17),
        plot.title = element_text(size = 20),
        legend.position = "none"
  )


#PUNTOS POR POSESION-----------------------------------------------------------
pointsp100 <- read_csv("C:/Users/pablo/Desktop/pointsp100.txt")
pointsp100 <- pointsp100[1:13,]
pointsp100$Player <- sub("\\\\.*", "", pointsp100$Player)

pointsp100$play_season <- paste(pointsp100$Player,pointsp100$Season)

ggplot(pointsp100, aes(x=play_season, y=PTS)) +
  geom_col(data = pointsp100,aes(x=play_season,y=PTS)) +
  coord_flip()

pointsp100 <- arrange(pointsp100, PTS)
pointsp100$play_season <- factor(pointsp100$play_season, levels = pointsp100$play_season)


ggplot(pointsp100, aes(play_season, PTS))+
  ggtitle("Puntos cada 100 posesiones por temporada")+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Temporada",
    y = "Puntos cada 100 posesiones"
  )+
  geom_col(fill = "#e54069")+
  coord_flip()+
  geom_text(aes(label = PTS), vjust = 0,hjust = -0.5)+
  dark_theme_gray()+
  theme(legend.position = "none")

#Scatter con eff--------------------------------------------------------------
tpa_plot3 <- ggplot(tpa, aes(x=MP, y=TPP)) + 
  geom_point(aes(color=Player), size=4.5)+
  geom_text_repel(aes(label=tpa$Player)) +
  #geom_text(label=tpa$Player,vjust=1.5,hjust=.5,colour="black",size=5)+
  ggtitle("% de tres vs Minutos Jugados")+
  xlim(2100, 3050)+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Minutos Jugados",
    y = "3P%"
  )+
 geom_segment(mapping=aes(x=2980, y=.4, xend=2867, yend=.368),
             arrow=arrow(type = "closed"), size=2, color="red")
tpa_plot3+dark_theme_gray()+
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=17),
        axis.title = element_text(size=17),
        plot.title = element_text(size = 20),
        legend.position = "none"
  )


