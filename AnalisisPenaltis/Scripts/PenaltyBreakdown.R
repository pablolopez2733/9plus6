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
#PASAMOS A MATRIZ Y HEATMAP
mypalette<-brewer.pal(9,"YlOrRd")
matriz.zonas <- as.matrix(zone.df)
heatmap.2( matriz.zonas, scale = "none", Rowv=FALSE, Colv=FALSE, dendrogram='none', 
           cellnote=matriz.zonas, notecol="black", trace='none', 
           key=FALSE,lwid = c(.01,.99), col=mypalette,
           lhei = c(.01,.99),margins = c(5,15 ))
#heatmap:
