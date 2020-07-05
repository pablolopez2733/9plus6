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


#Import Dataset:
GmScMp <- read_csv("https://github.com/pablolopez2733/9plus6/raw/master/NuevaEraNBA/GmScMp.csv")
View(GmScMp)

#Gráfica top 10 gmsc
top10 <- GmScMp[1:20,]
png(filename="top20gmsc.png", width=1700, height=1000)
theme_set(theme_dark(base_size = 8))
top10_plot <- ggplot(top10, aes(x=MP, y=GmSc)) + 
  geom_point(aes(color=Player), size=8)+
  geom_text(label=top10$Player,vjust=1.5,hjust=.3,colour="white",size=8)+
  ggtitle("20 mejores actuaciones en un partido de NBA")+
  labs(
    caption = "Datos:BasketballReference.com",
    x = "Minutos Jugados",
    y = "Gamescore"
  )+theme_dark()



dev.off()

#Ahora agregando a Harden extrapolado:
#ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, col = Species)) + 
#  geom_point() +
#  geom_point(aes(x=5.6, y=3.9), colour="blue")