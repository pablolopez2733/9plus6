#------------------------------------------------
#Scripts para artículo de New Age Basketball
#------------------------------------------------
#Libraries:
library(readr)
library(tidyverse)
library(dplyr)
library(moments)   #install.packages("moments")
library(lubridate) #install.packages("lubridate")
library(ggplot2)
library(data.table)
library(formattable)
library(tidyr)
library(DT)


#Import Dataset:
GmScMp <- read_csv("https://github.com/pablolopez2733/9plus6/raw/master/NuevaEraNBA/GmScMp.csv")
gmsc_2 <- read_csv("https://github.com/pablolopez2733/9plus6/raw/master/NuevaEraNBA/gmsc_2.csv")
gmsc_3 <- read_csv("https://github.com/pablolopez2733/9plus6/raw/master/NuevaEraNBA/gmsc_3.csv")
View(GmScMp)

#Gráfica top 20 gmsc-------------------------------------------------
top10 <- GmScMp[1:20,]

top10_plot <- ggplot(top10, aes(x=MP, y=GmSc)) + 
  geom_point(aes(color=Player), size=6)+
  geom_text(label=top10$Player,vjust=1.5,hjust=.5,colour="black",size=5)+
  ggtitle("20 mejores actuaciones en un partido de NBA medidas con Gamescore")+
  xlim(29, 52.5)+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Minutos Jugados",
    y = "Gamescore"
  )+
  geom_segment(mapping=aes(x=32.5, y=54, xend=31, yend=52.4),
              arrow=arrow(type = "closed"), size=2, color="red")
top10_plot+theme_bw()+
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=17),
        axis.title = element_text(size=17),
        plot.title = element_text(size = 20)
        )
#Tabla de top 20 Gmsc-------------------------------------------------
options(DT.options = list(pageLength = 20))
datatable(top10,colnames = c('Jugador', 'Minutos Jugados', 'Gamescore')) %>% 
  formatStyle(
  'GmSc',
  target = 'row',
  backgroundColor = styleEqual(c(52.4), c('pink')),
)

#--------------------------------------------------------------------

#2as Graficas y tablas:

sec_plot <- ggplot(gmsc_2, aes(x=MP, y=GmSc)) + 
  geom_point(aes(color=Player), size=6)+
  geom_text(label=gmsc_2$Player,vjust=1.5,hjust=.5,colour="black",size=5)+
  ggtitle("20 mejores actuaciones en un partido de NBA medidas con Gamescore")+
  xlim(29, 52.5)+
  labs(
    caption = "Datos:BasketballReference.com",
    subtitle = "Si Harden hubiera jugado 37 mins",
    x = "Minutos Jugados",
    y = "Gamescore"
  )+
  geom_segment(mapping=aes(x=32, y=62, xend=35.8, yend=64),
               arrow=arrow(type = "closed"), size=2, color="red")
sec_plot+theme_bw()+
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=17),
        axis.title = element_text(size=17),
        plot.title = element_text(size = 20)
  )
#Tabla de top 20 Gmsc-------------------------------------------------
options(DT.options = list(pageLength = 20))
datatable(gmsc_2,colnames = c('Jugador', 'Minutos Jugados', 'Gamescore')) %>% 
  formatStyle(
    'GmSc',
    target = 'row',
    backgroundColor = styleEqual(c(64.62), c('pink')),
  )
#-----------------------------------------------------------------------------
#3A GRÁFICA
third_plot <- ggplot(gmsc_3, aes(x=MP, y=GmSc)) + 
  geom_point(aes(color=Player), size=6)+
  geom_text(label=gmsc_3$Player,vjust=1.5,hjust=.5,colour="black",size=5)+
  ggtitle("20 mejores actuaciones en un partido de NBA medidas con Gamescore")+
  xlim(29, 52.5)+
  labs(
    caption = "Datos:BasketballReference.com",
    subtitle = "Si Harden hubiera jugado 50 mins",
    x = "Minutos Jugados",
    y = "Gamescore"
  )+
  geom_segment(mapping=aes(x=44, y=77, xend=48.7, yend=84.5),
               arrow=arrow(type = "closed"), size=2, color="red")+
  geom_text(aes(label = "Gamescore: 87.3"),x = 50,y = 87.3,vjust = 3.1,hjust= .5,colour="red")
third_plot+theme_bw()+
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=17),
        axis.title = element_text(size=17),
        plot.title = element_text(size = 20)
  )
