rm(list=ls())
setwd("~")

##############################
#                            #
# Israel Piña #
# #EleccionesPuebla2019     #
##############################


require(plyr)  # el papá de dplyr
library(dplyr) # manipular data
library(tidyr) # "tidy data" o el paquete que SIEMPRE van a prender
library(ggplot2)
require(foreign)
require(readxl)
require(readstata13)
require(tidyverse)
library(stringr)
require(RColorBrewer)
library(scales)
require(doBy)
require(reshape2)
require(gridExtra)
require(Hmisc)
require(ggmap)
require(rgdal)
require(rgeos)
require(sp)
require(maptools)
require(DeducerSpatial)
require(mapproj)
library(raster)
library(sf)
library(rayshader)
library(gganimate)





dir1 <- "/media/hectorpina/yanipaperdisk/Proyectos R/elecciones_2019/data/in"  # poner ruta 
dir2 <- "/media/hectorpina/yanipaperdisk/Proyectos R/elecciones_2019/graphs"  # poner ruta
dir3 <- "/media/hectorpina/yanipaperdisk/Proyectos R/elecciones_2019//mapas"
dir4 <- "/media/hectorpina/yanipaperdisk/Proyectos R/elecciones_2019/data/out"



#LLAMAR DATOS

#PREP 2019
#data2017 <- read.csv(paste(dir1, "elecciones/gobernador_casilla_seccion_distrito_2017.csv", sep="/"), fileEncoding = "Windows-1252")


mpos <- read_excel(paste(dir1, "21_CATALOGO_SECCIONES.xlsx", sep="/"))
eleccion <- read_excel(paste(dir1, "PUE_GUB_ext_2019b.xlsx", sep="/"))

View(mpos)
View(eleccion)

############## UNIR DATAFRAMES ##############

tempo <- mpos[,c(5,6)]
#Poner ceros a la izquierda
tempo$MUNICIPIO <- str_pad(tempo$MUNICIPIO, width=3, side="left", pad="0")
View(tempo)

data2019 <- merge(eleccion, tempo, by="SECCION", all=T)
View(data2019)


############## ######################################################################

  #### Elecciones 2019

#Eliminar columnas
data2019$CLAVE_CASILLA <- NULL 
data2019$CLAVE_ACTA <- NULL
data2019$TIPO_ACTA <- NULL
data2019$REPRESENTANTES_PP_CI <- NULL
data2019$OBSERVACIONES <- NULL
data2019$CONTABILIZADA <- NULL
data2019$MECANISMOS_TRASLADO <- NULL
data2019$SHA <- NULL
data2019$FECHA_HORA_ACOPIO <- NULL
data2019$FECHA_HORA_CAPTURA <- NULL
data2019$FECHA_HORA_VERIFICACION <- NULL
data2019$ORIGEN <- NULL
data2019$DIGITALIZACION <- NULL
data2019$TIPO_DOCUMENTO <- NULL
data2019$COTEJADA <- NULL
data2019$TOTAL_VOTOS_ASENTADO <- NULL 
data2019$TOTAL_REP_PARTIDO_CI_VOTARON <- NULL


#CAMBIAR ANOTACIONES POR CEROS

data2019[!is.na(data2019$PAN) & data2019$PAN=="Sin dato",]$PAN <- 0
data2019[!is.na(data2019$PRI) & data2019$PRI=="Sin dato",]$PRI <- 0
data2019[!is.na(data2019$PRD) & data2019$PRD=="Sin dato",]$PRD <- 0
data2019[!is.na(data2019$PT) & data2019$PT=="Sin dato",]$PT <- 0
data2019[!is.na(data2019$PVEM) & data2019$PVEM=="Sin dato",]$PVEM <- 0
data2019[!is.na(data2019$MC) & data2019$MC=="Sin dato",]$MC <- 0
data2019[!is.na(data2019$MORENA) & data2019$MORENA=="Sin dato",]$MORENA <- 0
data2019[!is.na(data2019$C_PAN_PRD_MC) & data2019$C_PAN_PRD_MC=="Sin dato",]$C_PAN_PRD_MC <- 0
data2019[!is.na(data2019$C_PAN_PRD) & data2019$C_PAN_PRD=="Sin dato",]$C_PAN_PRD <- 0
data2019[!is.na(data2019$C_PAN_MC) & data2019$C_PAN_MC=="Sin dato",]$C_PAN_MC <- 0
data2019[!is.na(data2019$C_PRD_MC) & data2019$C_PRD_MC=="Sin dato",]$C_PRD_MC <- 0
data2019[!is.na(data2019$C_PT_PVEM_MORENA) & data2019$C_PT_PVEM_MORENA=="Sin dato",]$C_PT_PVEM_MORENA <- 0
data2019[!is.na(data2019$C_PT_PVEM) & data2019$C_PT_PVEM=="Sin dato",]$C_PT_PVEM <- 0
data2019[!is.na(data2019$C_PT_MORENA) & data2019$C_PT_MORENA=="Sin dato",]$C_PT_MORENA <- 0
data2019[!is.na(data2019$C_PVEM_MORENA) & data2019$C_PVEM_MORENA=="Sin dato",]$C_PVEM_MORENA <- 0
data2019[!is.na(data2019$NO_REGISTRADOS) & data2019$NO_REGISTRADOS=="Sin dato",]$NO_REGISTRADOS <- 0
data2019[!is.na(data2019$NULOS) & data2019$NULOS=="Sin dato",]$NULOS <- 0
data2019[!is.na(data2019$TOTAL_CIUDADANOS_VOTARON) & data2019$TOTAL_CIUDADANOS_VOTARON=="Sin dato",]$TOTAL_CIUDADANOS_VOTARON <- 0


data2019[!is.na(data2019$PAN) & data2019$PAN=="Ilegible",]$PAN <- 0
data2019[!is.na(data2019$PRI) & data2019$PRI=="Ilegible",]$PRI <- 0
data2019[!is.na(data2019$PRD) & data2019$PRD=="Ilegible",]$PRD <- 0
data2019[!is.na(data2019$PT) & data2019$PT=="Ilegible",]$PT <- 0
data2019[!is.na(data2019$PVEM) & data2019$PVEM=="Ilegible",]$PVEM <- 0
data2019[!is.na(data2019$MC) & data2019$MC=="Ilegible",]$MC <- 0
data2019[!is.na(data2019$MORENA) & data2019$MORENA=="Ilegible",]$MORENA <- 0
data2019[!is.na(data2019$C_PAN_PRD_MC) & data2019$C_PAN_PRD_MC=="Ilegible",]$C_PAN_PRD_MC <- 0
data2019[!is.na(data2019$C_PAN_PRD) & data2019$C_PAN_PRD=="Ilegible",]$C_PAN_PRD <- 0
data2019[!is.na(data2019$C_PAN_MC) & data2019$C_PAN_MC=="Ilegible",]$C_PAN_MC <- 0
data2019[!is.na(data2019$C_PRD_MC) & data2019$C_PRD_MC=="Ilegible",]$C_PRD_MC <- 0
data2019[!is.na(data2019$C_PT_PVEM_MORENA) & data2019$C_PT_PVEM_MORENA=="Ilegible",]$C_PT_PVEM_MORENA <- 0
data2019[!is.na(data2019$C_PT_PVEM) & data2019$C_PT_PVEM=="Ilegible",]$C_PT_PVEM <- 0
data2019[!is.na(data2019$C_PT_MORENA) & data2019$C_PT_MORENA=="Ilegible",]$C_PT_MORENA <- 0
data2019[!is.na(data2019$C_PVEM_MORENA) & data2019$C_PVEM_MORENA=="Ilegible",]$C_PVEM_MORENA <- 0
data2019[!is.na(data2019$NO_REGISTRADOS) & data2019$NO_REGISTRADOS=="Ilegible",]$NO_REGISTRADOS <- 0
data2019[!is.na(data2019$NULOS) & data2019$NULOS=="Ilegible",]$NULOS <- 0
data2019[!is.na(data2019$TOTAL_CIUDADANOS_VOTARON) & data2019$TOTAL_CIUDADANOS_VOTARON=="Ilegible",]$TOTAL_CIUDADANOS_VOTARON <- 0

#Convertir primero en caracter para que no les asigne otro valor 
data2019 <- mutate(data2019, 
                   PAN=as.character(PAN), 
                   PRI=as.character(PRI), 
                   PRD=as.character(PRD), 
                   PT=as.character(PT),
                   PVEM=as.character(PVEM), 
                   MC=as.character(MC), 
                   MORENA=as.character(MORENA), 
                   C_PAN_PRD_MC=as.character(C_PAN_PRD_MC), 
                   C_PAN_PRD=as.character(C_PAN_PRD), 
                   C_PAN_MC=as.character(C_PAN_MC), 
                   C_PRD_MC=as.character(C_PRD_MC),
                   C_PT_PVEM_MORENA=as.character(C_PT_PVEM_MORENA), 
                   C_PT_PVEM=as.character(C_PT_PVEM), 
                   C_PT_MORENA=as.character(C_PT_MORENA),                     
                   C_PVEM_MORENA=as.character(C_PVEM_MORENA), 
                   NO_REGISTRADOS=as.character(NO_REGISTRADOS),
                   NULOS=as.character(NULOS),
                   TOTAL_CIUDADANOS_VOTARON=as.character(TOTAL_CIUDADANOS_VOTARON),
                   TOTAL_VOTOS=as.character(TOTAL_VOTOS)
            )

#Convertir ahora sí en numércios
data2019 <- mutate(data2019, 
                   PAN=as.numeric(PAN), 
                   PRI=as.numeric(PRI), 
                   PRD=as.numeric(PRD), 
                   PT=as.numeric(PT),
                   PVEM=as.numeric(PVEM), 
                   MC=as.numeric(MC), 
                   MORENA=as.numeric(MORENA), 
                   C_PAN_PRD_MC=as.numeric(C_PAN_PRD_MC), 
                   C_PAN_PRD=as.numeric(C_PAN_PRD), 
                   C_PAN_MC=as.numeric(C_PAN_MC), 
                   C_PRD_MC=as.numeric(C_PRD_MC),
                   C_PT_PVEM_MORENA=as.numeric(C_PT_PVEM_MORENA), 
                   C_PT_PVEM=as.numeric(C_PT_PVEM), 
                   C_PT_MORENA=as.numeric(C_PT_MORENA),                     
                   C_PVEM_MORENA=as.numeric(C_PVEM_MORENA), 
                   NO_REGISTRADOS=as.numeric(NO_REGISTRADOS),
                   NULOS=as.numeric(NULOS),
                   TOTAL_CIUDADANOS_VOTARON=as.numeric(TOTAL_CIUDADANOS_VOTARON),
                   TOTAL_VOTOS=as.numeric(TOTAL_VOTOS)
              )


#CONVERTIR VARIABLES A CARACTERES
data2019$ID_DISTRITO <- as.character(data2019$ID_DISTRITO)
data2019$SECCION <- as.character(data2019$SECCION)
data2019$MUNICIPIO <- as.character(data2019$MUNICIPIO)

#Convertir NA por ceros
data2019$PAN[is.na(data2019$PAN)] <- 0
data2019$PRI[is.na(data2019$PRI)] <- 0
data2019$PRD[is.na(data2019$PRD)] <- 0
data2019$PT[is.na(data2019$PT)] <- 0
data2019$PVEM[is.na(data2019$PVEM)] <- 0
data2019$MC[is.na(data2019$MC)] <- 0
data2019$MORENA[is.na(data2019$MORENA)] <- 0
data2019$C_PAN_PRD_MC[is.na(data2019$C_PAN_PRD_MC)] <- 0
data2019$C_PAN_PRD[is.na(data2019$C_PAN_PRD)] <- 0
data2019$C_PAN_MC[is.na(data2019$C_PAN_MC)] <- 0
data2019$C_PRD_MC[is.na(data2019$C_PRD_MC)] <- 0
data2019$C_PT_PVEM_MORENA[is.na(data2019$C_PT_PVEM_MORENA)] <- 0
data2019$C_PT_PVEM[is.na(data2019$C_PT_PVEM)] <- 0
data2019$C_PT_MORENA[is.na(data2019$C_PT_MORENA)] <- 0
data2019$C_PVEM_MORENA[is.na(data2019$C_PVEM_MORENA)] <- 0
data2019$NO_REGISTRADOS[is.na(data2019$NO_REGISTRADOS)] <- 0
data2019$NULOS[is.na(data2019$NULOS)] <- 0

# Sumar columnas por candidato
data2019$ENRIQUE_CARDENAS_SANCHEZ <- rowSums(data2019[ ,c(13,15,18,20:23)])
data2019$MIGUEL_BARBOSA_HUERTA <- rowSums(data2019[ ,c(16,17,19,24:27)])
data2019$ALBERTO_JIMENEZ_MERINO <- data2019[ ,c(14)]

#Borrar lo que no sirve
data2019 <- data2019[ ,-c(6:8,10,12:27)]

#Reordenar variables
data2019 <- data2019[c(2,3,4,5,12,1,6,13,14,15,8,9,10,7,11)]

#Borrar rows con NA en Municipios y Total de votos
data2019 <- data2019[!rowSums(is.na(data2019[5])), ]
data2019 <- data2019[!rowSums(is.na(data2019[13])), ]
View(data2019)




#Guardar base limpia
write.csv(data2019 , paste(dir4, "elecciones_pueba_2019.csv", sep="/"), fileEncoding ="UTF-8", row.names=F)


####################################################################################

#### A jugar con los datos

tempoA <-  data2019 %>%
          group_by(MUNICIPIO) %>%
          summarise(
            ENRIQUE_CARDENAS_SANCHEZ = sum(ENRIQUE_CARDENAS_SANCHEZ), 
            ALBERTO_JIMENEZ_MERINO = sum(ALBERTO_JIMENEZ_MERINO),
            MIGUEL_BARBOSA_HUERTA = sum(MIGUEL_BARBOSA_HUERTA), 
            NO_REGISTRADOS = sum(NO_REGISTRADOS),
            NULOS = sum(NULOS), 
            TOTAL_VOTOS = sum(TOTAL_VOTOS),
            LISTA_NOMINAL =sum(LISTA_NOMINAL)) %>%
            ungroup() %>%
              group_by(MUNICIPIO) %>%
              mutate(
                     ecs_porc = round((ENRIQUE_CARDENAS_SANCHEZ/TOTAL_VOTOS)*100, 2),
                     ajm_porc = round((ALBERTO_JIMENEZ_MERINO/TOTAL_VOTOS)*100, 2),
                     mbh_porc = round((MIGUEL_BARBOSA_HUERTA/TOTAL_VOTOS)*100, 2),
                     no_reg_porc = round((NO_REGISTRADOS/TOTAL_VOTOS)*100, 2),
                     nulos_porc = round((NULOS/TOTAL_VOTOS)*100, 2),
                     abst_porc = round((TOTAL_VOTOS/LISTA_NOMINAL)*100, 2)
                     )

View(tempoA)



#CREAR VARIABLE DE GANADOR
tempoA$GANADOR <- apply(tempoA[2:5],  1, function(x)names(tempoA[,2:5])[which.max(x)])


#CREAR VARIABLE CON LA VOTACIÓN GANADORA
tempoA$PORC_GANADOR <- apply(tempoA[9:12], 1, max)


#CREAR VARIABLE CLAVE DEL GANADOR
tempoA$CVE_GANADOR <- tempoA$GANADOR

#REEMPLAZAR CARACTERES DE REGISTROS 
tempoA$CVE_GANADOR <- replace(tempoA$CVE_GANADOR, tempoA$CVE_GANADOR=="MIGUEL_BARBOSA_HUERTA", "1")
tempoA$CVE_GANADOR <- replace(tempoA$CVE_GANADOR, tempoA$CVE_GANADOR=="ENRIQUE_CARDENAS_SANCHEZ", "2")
tempoA$CVE_GANADOR <- replace(tempoA$CVE_GANADOR, tempoA$CVE_GANADOR=="ALBERTO_JIMENEZ_MERINO", "3")
tempoA$CVE_GANADOR <- replace(tempoA$CVE_GANADOR, tempoA$CVE_GANADOR=="NO_REGISTRADOS", "4")
tempoA$CVE_GANADOR <- replace(tempoA$CVE_GANADOR, tempoA$CVE_GANADOR=="NULOS", "5")


#CONVERTIR EN CHARACTER Y FACTOR
tempoA$CVE_GANADOR <- as.numeric(tempoA$CVE_GANADOR)
tempoA$CVE_GANADOR <- as.factor(tempoA$CVE_GANADOR)

View(tempoA)

#Guardar base limpia
write.csv(tempoA, paste(dir4, "elecciones_pueba_por_municipio_2019.csv", sep="/"), fileEncoding ="UTF-8", row.names=F)


###################################################################################


####################################################################################


####################################################################################

####### MAPA!

#HACER POLIGONO. PRIMER PASO
#LA PRIMERA INSTRUCCION NO ME FUNCIONO EN LINUX
#munics <- readOGR(paste(dir1, "MUNICIPIOS/MUNICIPIOS.shp", sep="/"), stringsAsFactors=F)

munics <- shapefile("/media/hectorpina/yanipaperdisk/Proyectos R/elecciones_2019/data/in/municipios/MUNICIPIOS.shp")

#EXPLORAR OBJETO
head(munics@data)

#EXTRAER PUEBLA
puebla <- subset(munics, munics@data$CVE_ENT=="21")

#DIBUJAR MAPA
plot(puebla)

#EXPLORAR POLIGONO
puebla@data
head(puebla)
str(puebla)
View(puebla@data)

#VOLVER MINÚSCULAS
#En esta ocasion lo omitire
#names(edomex_secciones) <- tolower(names(edomex_secciones))

#VOLVER CARACTER MI VARIABLE PARA UNIR
puebla$CVE_MUN <- as.character(puebla$CVE_MUN)

#REORDENAR VARIABLES
##puebla@data <- edomex_secciones@data[c(5,1,2,3,4,6,7,8)]

#RECODIFICAR EL MAPA
puebla <- spTransform(puebla, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#VOLVER DATA FRAME EL POLIGONO
pueblaDF <- fortify(puebla, region = "CVE_MUN")
View(pueblaDF)

#MERGE
municipiosDF <- merge(tempoA, pueblaDF, by.x="MUNICIPIO", by.y="id", all.y=T)
View(municipiosDF)

ggplot() +
  geom_polygon(data = pueblaDF, aes(x=long, y=lat, group=group)) +
  coord_map()
  

####################################################################################

#MAPA DE VOTOS GANADOR POR MUNICIPIO DE PUEBLA

cols <- c("1" = "brown", "2" = "blue", "3" = "red", "4" = "gray", "5" = "green")

map <-  ggplot() + 
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=CVE_GANADOR, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_manual(name="Candidatos", values = cols,  labels = c("Miguel Barbosa Huerta", "Enrique Cárdenas Sánchez", "Alberto Jiménez Merino"))+
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Municipios ganados por candidato", caption ="Fuente: INE, 03/06/2019, 9:15 hrs." , color="black") +
  theme_void() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(fill = "#f5f1e4", color  =  NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.5, "lines"),  
        plot.background = element_rect(fill = "#f5f1e4", color  =  NA),
        plot.title = element_text(size = 22, face = "bold", color = "#d1574d"),
        plot.subtitle = element_text(size = 18, color = "#d1574d"),
        plot.margin = unit(rep(2, 4), "lines"),
        plot.caption = element_text(size= 12 ,hjust = 1.4),
        legend.position="right",
        legend.background = element_rect(color = NA, fill = "#f5f1e4"),
        legend.key.size = unit(1, "lines"),
        legend.key.width= unit(1, "lines"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size=14),
        legend.spacing = unit(.5, 'cm')
  ) 

 map


ggsave(paste(dir3, "municipio_candidatos_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)



####################################################################################

##### MAPA DE VOTOS DE MIGUEL BARBOSA EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=mbh_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
        scale_fill_gradient2(name="% votos", low = "#ffffff", mid = "#ccb4b4", high = "#570808", midpoint = 10) +
        scale_alpha(range = c(.2, .75), guide = FALSE) +
        labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de votos de Miguel Barbosa Huerta por municipio", caption ="Fuente: INE, 03/06/2019, 9:15 hrs." , color="black") +
        theme_void() +
        theme(axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              panel.background = element_rect(fill = "#f5f1e4", color  =  NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(0.5, "lines"),  
              plot.background = element_rect(fill = "#f5f1e4", color  =  NA),
              plot.title = element_text(size = 22, face = "bold", color = "#d1574d"),
              plot.subtitle = element_text(size = 18, color = "#d1574d"),
              plot.margin = unit(rep(2, 4), "lines"),
              plot.caption = element_text(size= 12 ,hjust = 1),
              legend.position="bottom",
              legend.background = element_rect(color = NA, fill = "#f5f1e4"),
              legend.key.size = unit(1, "lines"),
              legend.key.width= unit(2, "lines"),
              legend.text = element_text(size = 14),
              legend.title = element_text(size=14),
              legend.spacing = unit(.5, 'cm')
        )

map

ggsave(paste(dir3, "barbosa_votos_municipio_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


##### MAPA DE ENRIQUE CARDENAS EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=ecs_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_gradient2(name="% votos", low = "#ffffff", mid = "#a9bad3", high = "#254b82", midpoint = 10) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de votos de Enrique Cárdenas Sánchez por municipio", caption ="Fuente: INE, 03/06/2019, 9:15 hrs." , color="black") +
  theme_void() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(fill = "#f5f1e4", color  =  NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.5, "lines"),  
        plot.background = element_rect(fill = "#f5f1e4", color  =  NA),
        plot.title = element_text(size = 22, face = "bold", color = "#d1574d"),
        plot.subtitle = element_text(size = 18, color = "#d1574d"),
        plot.margin = unit(rep(2, 4), "lines"),
        plot.caption = element_text(size= 12 ,hjust = 1),
        legend.position="bottom",
        legend.background = element_rect(color = NA, fill = "#f5f1e4"),
        legend.key.size = unit(1, "lines"),
        legend.key.width= unit(2, "lines"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size=14),
        legend.spacing = unit(.5, 'cm')
  )

map


ggsave(paste(dir3, "cardenas_votos_municipio_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


##### MAPA DE VOTOS DE ALBERTO JIMENEZ MERINO EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=ajm_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_gradient2(name="% votos", low = "#ffffff", mid = "#f19999", high = "#c60000", midpoint = 10) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de votos de Alberto Jiménez Merino por municipio", caption ="Fuente: INE, 03/06/2019, 9:15 hrs." , color="black") +
  theme_void() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(fill = "#f5f1e4", color  =  NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.5, "lines"),  
        plot.background = element_rect(fill = "#f5f1e4", color  =  NA),
        plot.title = element_text(size = 22, face = "bold", color = "#d1574d"),
        plot.subtitle = element_text(size = 18, color = "#d1574d"),
        plot.margin = unit(rep(2, 4), "lines"),
        plot.caption = element_text(size= 12 ,hjust = 1),
        legend.position="bottom",
        legend.background = element_rect(color = NA, fill = "#f5f1e4"),
        legend.key.size = unit(1, "lines"),
        legend.key.width= unit(2, "lines"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size=14),
        legend.spacing = unit(.5, 'cm')
    )

map


ggsave(paste(dir3, "jimenez_votos_municipio_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


##### MAPA DE ABSTENCIONISMO EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=abst_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_gradient2(name="% abstención", low = "#ffffff", mid = "#a7c9ce", high = "#18545d", midpoint = 10) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de abstencionismo por municipio", caption ="Fuente: INE, 03/06/2019, 9:15 hrs." , color="black") +
  theme_void() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(fill = "#f5f1e4", color  =  NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.5, "lines"),  
        plot.background = element_rect(fill = "#f5f1e4", color  =  NA),
        plot.title = element_text(size = 22, face = "bold", color = "#d1574d"),
        plot.subtitle = element_text(size = 18, color = "#d1574d"),
        plot.margin = unit(rep(2, 4), "lines"),
        plot.caption = element_text(size= 12 ,hjust = 1),
        legend.position="bottom",
        legend.background = element_rect(color = NA, fill = "#f5f1e4"),
        legend.key.size = unit(1, "lines"),
        legend.key.width= unit(2, "lines"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size=14),
        legend.spacing = unit(.5, 'cm')
  )

map

ggsave(paste(dir3, "abstencionismo_municipio_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


####################################################################################
####################################################################################
####################################################################################
####################################################################################


tempoB <-  data2019 %>%
  group_by(SECCION) %>%
  summarise(
    ENRIQUE_CARDENAS_SANCHEZ = sum(ENRIQUE_CARDENAS_SANCHEZ), 
    ALBERTO_JIMENEZ_MERINO = sum(ALBERTO_JIMENEZ_MERINO),
    MIGUEL_BARBOSA_HUERTA = sum(MIGUEL_BARBOSA_HUERTA)) 

View(tempoB)

#Voltear base
tempoB  <- melt(tempoB, id.vars=c("SECCION"))
View(tempoB)

#Crear boxplot
gr <- ggplot(tempoB, aes(x=variable, y=value)) +
  geom_boxplot(fill="#203562", color= "#000000") +
  labs(title="Gasto en comida fuera del hogar en México", 
       x="Decil", y="Gasto", subtitle="Monto por familia al mes", caption ="Fuente: INE, 04/05/2018, 6:00 hrs.",) +
  theme(axis.text.y = element_text(size = 12, color = "#000000"),
        axis.text.x = element_text(size = 12, color = "#000000"),
        axis.line = element_line(color = "#000000", size = .5),
        panel.background = element_rect(fill = "#e0e0e0", color  =  NA),
        panel.grid.major = element_line(color = "#606060", size = .15),
        panel.grid.minor = element_line(color = "#606060", size = .15),
        panel.spacing = unit(0.5, "lines"),  
        plot.background = element_rect(fill = "#e0e0e0", color  =  NA),
        plot.title = element_text(size = 22, face="bold", color = "#203562"),
        plot.subtitle = element_text(size = 18, color = "#203562"),
        plot.margin = unit(rep(2, 4), "lines")
  )  

gr