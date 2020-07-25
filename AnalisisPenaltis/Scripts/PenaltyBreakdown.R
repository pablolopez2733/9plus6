#------------------------------------------------------------------------------
#Penalty Breakdown
#------------------------------------------------------------------------------
rm(list = ls())
library(tidyverse) 
library(dplyr) 
library(readr)
library(gplots)
library(RColorBrewer)
library(formattable)
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
#Goles----------------------------------------------------------------
goles <- penalties %>% filter(Goal == 1) %>% select(Zone)
goal_count <- table(goles$Zone)
goal.df <- data.frame(c(21,27,40),c(11,11,12),c(14,25,34))
names(goal.df)[1] <- "Izquierda"
names(goal.df)[2] <- "Centro"
names(goal.df)[3] <- "Derecha"
rownames(goal.df) <- c("Arriba","Centro","Abajo")
matriz.goles <- as.matrix(goal.df)
goles.porc <- round(matriz.goles * (100/195), digits = 2)

mypalette3<-(brewer.pal(9,"YlGn"))
heatmap.2( goles.porc, scale = "none", Rowv=FALSE, Colv=FALSE, dendrogram='none', 
           cellnote=goles.porc, notecol="black", trace='none', 
           key=FALSE,lwid = c(.01,.99), col=mypalette3,srtCol=360, 
           adjCol = c(0.5,1),lhei = c(.01,.99),margins = c(5,15 ))

table(goles)
table(penalties$Zone)
#Atajadas----------------------------------------------------------------------
atajadas <- penalties %>% 
  filter(Goal == 0 & OnTarget ==1) %>% 
  select(Zone,Keeper)

#lanzamientos----------------------------------------------------------------
table(penalties$Keeper)


#Atajadas--------------------------------------------------------------
atajadas <- penalties %>% 
  filter(Goal == 0 & OnTarget ==1) %>% 
  select(Zone,Keeper)

table(atajadas$Keeper)

tiros.izq <- penalties %>%
  filter(Zone==1 | Zone == 4 | Zone==7)

tiros.centro <- penalties %>%
  filter(Zone==2 | Zone == 5 | Zone==8)

tiros.der <- penalties %>%
  filter(Zone==3 | Zone == 6 | Zone==9)

atajadas.izq <- tiros.izq %>%
  filter(OnTarget ==1 & Goal == 0 )
atajadas.centro <- tiros.centro %>%
  filter(OnTarget ==1 & Goal == 0 )
atajadas.der <- tiros.der %>%
  filter(OnTarget ==1 & Goal == 0 )

#Zurdos o diesros--------------------------------------------
tiros.zurdos <- penalties %>% 
  filter(Foot == "L")

tiros.diestros <- penalties %>% 
  filter(Foot == "R")

table(tiros.zurdos$Zone)
table(tiros.diestros$Zone)

goles.zurdos <- tiros.zurdos %>%
  filter(Goal == 1)
goles.diestros <- tiros.diestros %>%
  filter(Goal == 1)

crunch.time <- penalties %>%
  filter(Elimination == 1)

crunch.goals <- crunch.time %>%
  filter(Goal == 1)

table(crunch.time$Zone)
table(crunch.goals$Zone)

tiros <- penalties %>% 
  filter(!is.na(Goal))

por.pais <- tiros %>% 
  group_by(Team) %>%
  summarize(
    tiros = n(), goles = sum(Goal),
  )

por.pais["Eficiencia"] = round((por.pais$goles/por.pais$tiros)*100,digits = 2)

names(por.pais)[1] <- "Equipo"
names(por.pais)[2] <- "Tiros"
names(por.pais)[3] <- "Goles"
names(por.pais)[4] <- "Eficiencia"

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

tb <- formattable(por.pais,align =c("c","c","c","c"),
            list(
  'Equipo' = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  'Eficiencia'= color_tile(customRed, customGreen)))


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

export_formattable(tb,"table.png")

#Maldicion de tirar primero-------------------------------------------------
penalesMundial <- read_csv("C:/Users/pablo/Desktop/GithubRepos/9plus6/AnalisisPenaltis/Data/penalesMundial.txt")
won.first <- penalesMundial[1:30,]

won.first 