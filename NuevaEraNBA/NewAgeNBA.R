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


#Import Dataset:
GmScMp <- read_csv("https://github.com/pablolopez2733/9plus6/raw/master/NuevaEraNBA/GmScMp.csv")
View(GmScMp)

#Gráfica top 10 gmsc
top10 <- GmScMp[1:20,]
#png(filename="top20gmsc.png", width=1700, height=1000)
#theme_set(theme_dark(base_size = 8))
top10_plot <- ggplot(top10, aes(x=MP, y=GmSc)) + 
  geom_point(aes(color=Player), size=6)+
  geom_text(label=top10$Player,vjust=1.5,hjust=.5,colour="black",size=5)+
  ggtitle("20 mejores actuaciones en un partido de NBA")+
  xlim(29, 52.5)+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Minutos Jugados",
    y = "Gamescore"
  )+
  # annotate(
  #   geom = "curve", x = 32.5, y = 55, xend = 31, yend = 52.4, colour="red",
  #   curvature = 0, arrow = arrow(length = unit(2, "mm"),type="closed")
  # )
  geom_segment( mapping=aes(x=32.5, y=54, xend=31, yend=52.4)
               , arrow=arrow(type = "closed"), size=2, color="red")
top10_plot+theme_bw()+
  theme(axis.text.x=element_text(size=17),
        axis.text.y=element_text(size=17),
        axis.title = element_text(size=17),
        plot.title = element_text(size = 20)
        )

#dev.off()

#Ahora agregando a Harden extrapolado:
#ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, col = Species)) + 
#  geom_point() +
#  geom_point(aes(x=5.6, y=3.9), colour="blue")