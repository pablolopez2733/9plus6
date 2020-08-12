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
png("no_name.png", units="in", width=11, height=7, res=300)
tpa_plot1 <- ggplot(tpa, aes(x=MP, y=TPA)) + 
  geom_point(color = "#7df3ed", size=3.2)+
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

dev.off()


#GRAFICA CON NOMBRE----------------------------------------------------------
png("w_name.png", units="in", width=11, height=7, res=300)
# insert ggplot code

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

dev.off()
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

png("bars.png", units="in", width=12, height=10, res=300)

ggplot(pointsp100, aes(play_season, PTS))+
  ggtitle("Puntos cada 100 posesiones por temporada")+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Temporada",
    y = "Puntos cada 100 posesiones"
  )+
  ylim(0,55.0)+
  geom_col(fill = "#e54069")+
  coord_flip()+
  geom_text(aes(label = PTS), vjust = 0,hjust = -0.5,size=5)+
  dark_theme_gray()+
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20, face = "bold"))
 dev.off()

#Scatter con eff--------------------------------------------------------------
png("eff.png", units="in", width=11, height=7, res=300)
 
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

dev.off()

#Grafica de tiros libres con harden----------------------------------------
fts <- df %>%
  filter(Pos == "PG" | Pos == "SG")

fts  <- arrange(fts,desc(FTA))

fts <- fts[1:20,]

fts$Player <- sub("\\\\.*", "", fts$Player)

png("fts_wHarden.png", units="in", width=12, height=7, res=300)

fts_plot <- ggplot(fts, aes(x=FTA, y=FT)) + 
  geom_point(color = "#e54069", size=3.2)+
  geom_text_repel(aes(label=fts$Player))+
  #geom_text_repel(aes(label=tpa$Player)) +
  #geom_text(label=tpa$Player,vjust=1.5,hjust=.5,colour="black",size=5)+
  ggtitle("Tiros libres intentados vs tiros libres encestados")+
  xlim(250, 900)+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Tiros Libres Intentados",
    y = "Tiros Libres Encestados"
  )

  #geom_segment(mapping=aes(x=2700, y=900, xend=2850, yend=1010),
   #            arrow=arrow(type = "closed"), size=2, color="#e54069")

fts_plot+dark_theme_gray()+
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=17),
        axis.title = element_text(size=17),
        plot.title = element_text(size = 20),
        legend.position = "none"
  )

dev.off()
#Grafica de tiros libres sin harden----------------------------------------
png("fts_nHarden.png", units="in", width=12, height=7, res=300)

fts_plot <- ggplot(fts, aes(x=FTA, y=FT)) + 
  geom_point(color = "#e54069", size=3.2)+
  geom_text_repel(aes(label=fts$Player))+
  #geom_text_repel(aes(label=tpa$Player)) +
  #geom_text(label=tpa$Player,vjust=1.5,hjust=.5,colour="black",size=5)+
  ggtitle("Tiros libres intentados vs tiros libres encestados")+
  xlim(250, 600)+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Tiros Libres Intentados",
    y = "Tiros Libres Encestados"
  )
#geom_segment(mapping=aes(x=2700, y=900, xend=2850, yend=1010),
#            arrow=arrow(type = "closed"), size=2, color="#e54069")

fts_plot+dark_theme_gray()+
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=17),
        axis.title = element_text(size=17),
        plot.title = element_text(size = 20),
        legend.position = "none"
  )
dev.off()
#Intentemos ver con FGA
fgas <- df %>%
  filter(Pos == "PG" | Pos == "SG")

fgas  <- arrange(fgas,desc(FGA))

fgas <- fgas[1:20,]

fgas$Player <- sub("\\\\.*", "", fgas$Player)


fgas_plot <- ggplot(fgas, aes(x=FGA, y=FG)) + 
  geom_point(color = "#e54069", size=3.2)+
  geom_text_repel(aes(label=fgas$Player))+
  #geom_text_repel(aes(label=tpa$Player)) +
  #geom_text(label=tpa$Player,vjust=1.5,hjust=.5,colour="black",size=5)+
  ggtitle("Intentos de campo vs tiros de campo encestado")+
  xlim(1100, 1950)+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Intentos de campo",
    y = "Tiros de campo encestados"
  )
#geom_segment(mapping=aes(x=2700, y=900, xend=2850, yend=1010),
#            arrow=arrow(type = "closed"), size=2, color="#e54069")

fgas_plot+dark_theme_gray()+
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=17),
        axis.title = element_text(size=17),
        plot.title = element_text(size = 20),
        legend.position = "none"
  )

