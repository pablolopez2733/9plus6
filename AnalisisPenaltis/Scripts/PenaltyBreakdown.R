#------------------------------------------------------------------------------
#Penalty Breakdown
#------------------------------------------------------------------------------
rm(list = ls())
library(tidyverse) 
library(dplyr) 
library(readr)
library(gplots)
library(RColorBrewer)
#Read Data:
penalties <- read_csv("https://github.com/pablolopez2733/9plus6/raw/master/AnalisisPenaltis/Data/WorldCupShootouts.csv")

#Desglose por zonas
zone_count <- table(penalties$Zone)
zone_count
#hacemos df de las zonas
zone.df <- data.frame(c(28,36,63),c(19,18,20),c(16,33,48))
names(zone.df)[1] <- "Izquierda"
names(zone.df)[2] <- "Centro"
names(zone.df)[3] <- "Derecha"
rownames(zone.df) <- c("Arriba","Centro","Abajo")
#PASAMOS A MATRIZ Y HEATMAP:
#Tiros totales---------------------------------------------------
mypalette<-brewer.pal(9,"YlOrRd")
matriz.zonas <- as.matrix(zone.df)
heatmap.2( matriz.zonas, scale = "none", Rowv=FALSE, Colv=FALSE, dendrogram='none', 
           cellnote=matriz.zonas, notecol="black", trace='none', 
           key=FALSE,lwid = c(.01,.99), col=mypalette,
           lhei = c(.01,.99),margins = c(5,15 ))
#Ahora en porcentajes:------------------------------------------

zonas.porc <- round(matriz.zonas * (100/279), digits = 2)
mypalette2<-rev(brewer.pal(9,"RdYlBu"))
heatmap.2( zonas.porc, scale = "none", Rowv=FALSE, Colv=FALSE, dendrogram='none', 
           cellnote=zonas.porc, notecol="black", trace='none', 
           key=FALSE,lwid = c(.01,.99), col=mypalette2,srtCol=360, 
           adjCol = c(0.5,1),lhei = c(.01,.99),margins = c(5,15 ))


