rm(list=ls())
setwd(".")

##############################
#                            #
# Israel Piña #
# #EleccionesPuebla2019     #
##############################

require(readxl)
library(stringr)
require(plyr)  # el papá de dplyr
library(dplyr) # manipular data
library(tidyr) # "tidy data" o el paquete que SIEMPRE van a prender
library(raster)
library(ggplot2)
library(viridis)



dir1 <- "../data/in"  # poner ruta 
dir2 <- "../graphs"  # poner ruta
dir3 <- "../mapas"
dir4 <- "../data/out"



#LLAMAR DATOS
mpos <- read_excel(paste(dir1, "municipios_puebla.xls", sep="/"))
eleccion <- read_excel(paste(dir1, "eleccion_puebla_computos_2019.xlsx", sep="/"))

############## UNIR DATAFRAMES ##############

#Seleccionar variables que nos sirven de municipios
mpos <- mpos[,c(3,4,5)]

#Unir dataframes
data2019 <- merge(eleccion, mpos, by="SECCION", all=T)

#Remover objetos
rm(mpos,eleccion)

View(data2019)

############## TERMINAR DE LIMPIAR LA BASE DE DATOS ##############

#Reordenar variables
data2019 <- data2019[c(3,4,15,16,5,6,1,2,7,8:14)]

#Borrar rows con NA en Clave_Casilla
data2019 <- data2019[!rowSums(is.na(data2019[8])), ]

#Poner ceros a la izquierda en ID_Municipio
data2019$ID_MUNICIPIO <- str_pad(data2019$ID_MUNICIPIO, width=3, side="left", pad="0")

#Convertir primero en caracter para que no les asigne otro valor 
data2019 <- mutate(data2019, 
                   ALBERTO_JIMENEZ=as.character(ALBERTO_JIMENEZ), 
                   ENRIQUE_CARDENAS=as.character(ENRIQUE_CARDENAS), 
                   MIGUEL_BARBOSA=as.character(MIGUEL_BARBOSA), 
                   NO_REGISTRADOS=as.character(NO_REGISTRADOS),
                   NULOS=as.character(NULOS),
                   TOTAL_VOTOS=as.character(TOTAL_VOTOS),
                   LISTA_NOMINAL=as.character(LISTA_NOMINAL)
            )

#Convertir ahora sí en numércios
data2019 <- mutate(data2019, 
                   ALBERTO_JIMENEZ=as.numeric(ALBERTO_JIMENEZ), 
                   ENRIQUE_CARDENAS=as.numeric(ENRIQUE_CARDENAS), 
                   MIGUEL_BARBOSA=as.numeric(MIGUEL_BARBOSA), 
                   NO_REGISTRADOS=as.numeric(NO_REGISTRADOS),
                   NULOS=as.numeric(NULOS),
                   TOTAL_VOTOS=as.numeric(TOTAL_VOTOS),
                   LISTA_NOMINAL=as.numeric(LISTA_NOMINAL)
              )

#Convertir NA por ceros - Por si hay
data2019$ALBERTO_JIMENEZ[is.na(data2019$ALBERTO_JIMENEZ)] <- 0
data2019$ENRIQUE_CARDENAS[is.na(data2019$ENRIQUE_CARDENAS)] <- 0
data2019$MIGUEL_BARBOSA[is.na(data2019$MIGUEL_BARBOSA)] <- 0
data2019$NO_REGISTRADOS[is.na(data2019$NO_REGISTRADOS)] <- 0
data2019$NULOS[is.na(data2019$NULOS)] <- 0
data2019$TOTAL_VOTOS[is.na(data2019$TOTAL_VOTOS)] <- 0
data2019$LISTA_NOMINAL[is.na(data2019$LISTA_NOMINAL)] <- 0


#CONVERTIR VARIABLES A CARACTERES QUE OLVIDE ARRIBA JA
data2019$ID_DISTRITO <- as.character(data2019$ID_DISTRITO)
data2019$SECCION <- as.character(data2019$SECCION)
data2019$ID_MUNICIPIO <- as.character(data2019$ID_MUNICIPIO)


#Guardar base limpia
write.csv(data2019 , paste(dir4, "elecciones_puebla_computos_2019.csv", sep="/"), fileEncoding ="UTF-8", row.names=F)


####################################################################################

#### A jugar con los datos - Vamos a sacar votos y porcentajes por municipio

tempoA <-  data2019 %>%
          group_by(ID_MUNICIPIO, MUNICIPIO) %>%
          summarise(
            ALBERTO_JIMENEZ = sum(ALBERTO_JIMENEZ), 
            ENRIQUE_CARDENAS = sum(ENRIQUE_CARDENAS),
            MIGUEL_BARBOSA = sum(MIGUEL_BARBOSA), 
            NO_REGISTRADOS = sum(NO_REGISTRADOS),
            NULOS = sum(NULOS), 
            TOTAL_VOTOS = sum(TOTAL_VOTOS),
            LISTA_NOMINAL =sum(LISTA_NOMINAL)) %>%
            ungroup() %>%
              group_by(ID_MUNICIPIO, MUNICIPIO) %>%
              mutate(
                     aj_porc = round((ALBERTO_JIMENEZ/TOTAL_VOTOS)*100, 2),
                     ec_porc = round((ENRIQUE_CARDENAS/TOTAL_VOTOS)*100, 2),
                     mb_porc = round((MIGUEL_BARBOSA/TOTAL_VOTOS)*100, 2),
                     no_reg_porc = round((NO_REGISTRADOS/TOTAL_VOTOS)*100, 2),
                     nulos_porc = round((NULOS/TOTAL_VOTOS)*100, 2),
                     parti_porc = round((TOTAL_VOTOS/LISTA_NOMINAL)*100, 2),
                     abst_porc = round((100-parti_porc),2)
                     )

View(tempoA)

################################################################

########## PREPARAR LA BASE TEMPORAL PARA MAPEAR #####################

#CREAR VARIABLE DE GANADOR
tempoA$GANADOR <- apply(tempoA[3:6],  1, function(x)names(tempoA[,3:6])[which.max(x)])


#CREAR VARIABLE CON LA VOTACIÓN GANADORA POR PORCENTAJE
tempoA$PORC_GANADOR <- apply(tempoA[10:13], 1, max)


#CREAR VARIABLE CLAVE DEL GANADOR
tempoA$CVE_GANADOR <- tempoA$GANADOR

#REEMPLAZAR CARACTERES DE REGISTROS EN VARIABLE CLAVE DEL GANADOR
tempoA$CVE_GANADOR <- replace(tempoA$CVE_GANADOR, tempoA$CVE_GANADOR=="ALBERTO_JIMENEZ", "1")
tempoA$CVE_GANADOR <- replace(tempoA$CVE_GANADOR, tempoA$CVE_GANADOR=="ENRIQUE_CARDENAS", "2")
tempoA$CVE_GANADOR <- replace(tempoA$CVE_GANADOR, tempoA$CVE_GANADOR=="MIGUEL_BARBOSA", "3")
tempoA$CVE_GANADOR <- replace(tempoA$CVE_GANADOR, tempoA$CVE_GANADOR=="NO_REGISTRADOS", "4")


#CONVERTIR EN  FACTOR LA VARIABLE DEL GANADOR PARA PODER MAPEAR
tempoA$CVE_GANADOR <- as.factor(tempoA$CVE_GANADOR)


#Guardar base limpia
write.csv(tempoA, paste(dir4, "elecciones_puebla_por_municipio_computo_2019.csv", sep="/"), fileEncoding ="UTF-8", row.names=F)


###################################################################################

####################################################################################

####### MAPA!

#HACER POLIGONO. PRIMER PASO

#Abrir shapefiles
munics_puebla <- shapefile("/media/hectorpina/yanipaperdisk/Proyectos R/elecciones_puebla_2019/data/in/mapas_puebla/MUNICIPIO.shp")

#EXPLORAR OBJETO
head(munics_puebla@data)

#DIBUJAR MAPA
plot(munics_puebla)

#EXPLORAR POLIGONO
munics_puebla@data
head(munics_puebla)
str(munics_puebla)
View(munics_puebla@data)

#VOLVER MINÚSCULAS
#En esta ocasion lo omitire
#names(edomex_secciones) <- tolower(names(edomex_secciones))

#VOLVER CARACTER MI VARIABLE PARA UNIR
munics_puebla$municipio <- as.character(munics_puebla$municipio)


#RECODIFICAR EL MAPA
munics_puebla <- spTransform(munics_puebla, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#VOLVER DATA FRAME EL POLIGONO
pueblaDF <- fortify(munics_puebla, region = "municipio")

#Poner ceros a la izquierda en ID
pueblaDF$id <- str_pad(pueblaDF$id, width=3, side="left", pad="0")

#Probar Mapa
ggplot() +
  geom_polygon(data = pueblaDF, aes(x=long, y=lat, group=group)) +
  coord_map()


#MERGE
municipiosDF <- merge(pueblaDF, tempoA, by.x="id", by.y="ID_MUNICIPIO", all.y=T)

#Remover objetos
rm(munics_puebla, pueblaDF, tempoA)


####################################################################################

#MAPA DE VOTOS GANADOR POR MUNICIPIO DE PUEBLA

cols <- c("1" = "red", "2" = "blue", "3" = "brown", "4" = "gray")

map <-  ggplot() + 
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=CVE_GANADOR, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_manual(name="Candidatos", values = cols,  labels = c("Alberto Jiménez", "Enrique Cárdenas", "Miguel Barbosa"))+
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Municipios ganados por candidato", caption ="Fuente: INE, Cómputos finales, 06/06/2019" , color="black") +
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


ggsave(paste(dir3, "computos_municipio_candidatos_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)



####################################################################################

##### MAPA DE VOTOS DE MIGUEL BARBOSA EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=mb_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
        scale_fill_gradient2(name="% votos", low = "#ffffff", mid = "#ccb4b4", high = "#570808", midpoint = 40) +
        scale_alpha(range = c(.2, .75), guide = FALSE) +
        labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de votos de Miguel Barbosa por municipio", caption ="Fuente: INE, Cómputos finales, 06/06/2019" , color="black") +
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

ggsave(paste(dir3, "computos_barbosa_votos_municipio_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


##### MAPA DE ENRIQUE CARDENAS EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=ec_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_gradient2(name="% votos", low = "#ffffff", mid = "#a9bad3", high = "#1c3449", midpoint = 30) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de votos de Enrique Cárdenas por municipio", caption ="Fuente: INE, Cómputos finales, 06/06/2019" , color="black") +
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


ggsave(paste(dir3, "computos_cardenas_votos_municipio_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


##### MAPA DE VOTOS DE ALBERTO JIMENEZ MERINO EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=aj_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_gradient2(name="% votos", low = "#ffffff", mid = "#f19999", high = "#c60000", midpoint = 30) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de votos de Alberto Jiménez por municipio", caption ="Fuente: INE, Cómputos finales, 06/06/2019" , color="black") +
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


ggsave(paste(dir3, "computos_jimenez_votos_municipio_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


################################################################################


##### MAPA DE ABSTENCIONISMO EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=abst_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_viridis(name="% abstención") +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de abstencionismo por municipio", caption ="Fuente: INE, Cómputos finales, 06/06/2019" , color="black") +
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

ggsave(paste(dir3, "computos_abstencionismo_municipio_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


##### MAPA DE PARTICIPACION EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=municipiosDF, aes(x=long, y=lat, fill=parti_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_viridis(name="% participación") +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de participación por municipio", caption ="Fuente: INE, Cómputos finales, 06/06/2019" , color="black") +
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

ggsave(paste(dir3, "computos_participacion_municipio_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


##### Remover objetos
rm(municipiosDF, map)

####################################################################################
####################################################################################
####################################################################################
####################################################################################

################### AHORA MAPEEMOS POR SECCION ELECTORAL #################################

#### Vamos a sacar votos y porcentajes por seccion

tempoB <-  data2019 %>%
  group_by(SECCION) %>%
  summarise(
    ALBERTO_JIMENEZ = sum(ALBERTO_JIMENEZ), 
    ENRIQUE_CARDENAS = sum(ENRIQUE_CARDENAS),
    MIGUEL_BARBOSA = sum(MIGUEL_BARBOSA), 
    NO_REGISTRADOS = sum(NO_REGISTRADOS),
    NULOS = sum(NULOS), 
    TOTAL_VOTOS = sum(TOTAL_VOTOS),
    LISTA_NOMINAL =sum(LISTA_NOMINAL)) %>%
  ungroup() %>%
  group_by(SECCION) %>%
  mutate(
    aj_porc = round((ALBERTO_JIMENEZ/TOTAL_VOTOS)*100, 2),
    ec_porc = round((ENRIQUE_CARDENAS/TOTAL_VOTOS)*100, 2),
    mb_porc = round((MIGUEL_BARBOSA/TOTAL_VOTOS)*100, 2),
    no_reg_porc = round((NO_REGISTRADOS/TOTAL_VOTOS)*100, 2),
    nulos_porc = round((NULOS/TOTAL_VOTOS)*100, 2),
    parti_porc = round((TOTAL_VOTOS/LISTA_NOMINAL)*100, 2),
    abst_porc = round((100-parti_porc),2)
  )


################################################################

########## PREPARAR LA BASE TEMPORAL PARA MAPEAR #####################

#CREAR VARIABLE DE GANADOR
tempoB$GANADOR <- apply(tempoB[2:5],  1, function(x)names(tempoB[,2:5])[which.max(x)])


#CREAR VARIABLE CON LA VOTACIÓN GANADORA POR PORCENTAJE
tempoB$PORC_GANADOR <- apply(tempoB[9:12], 1, max)


#CREAR VARIABLE CLAVE DEL GANADOR
tempoB$CVE_GANADOR <- tempoB$GANADOR

#REEMPLAZAR CARACTERES DE REGISTROS EN VARIABLE CLAVE DEL GANADOR
tempoB$CVE_GANADOR <- replace(tempoB$CVE_GANADOR, tempoB$CVE_GANADOR=="ALBERTO_JIMENEZ", "1")
tempoB$CVE_GANADOR <- replace(tempoB$CVE_GANADOR, tempoB$CVE_GANADOR=="ENRIQUE_CARDENAS", "2")
tempoB$CVE_GANADOR <- replace(tempoB$CVE_GANADOR, tempoB$CVE_GANADOR=="MIGUEL_BARBOSA", "3")
tempoB$CVE_GANADOR <- replace(tempoB$CVE_GANADOR, tempoB$CVE_GANADOR=="NO_REGISTRADOS", "4")


#CONVERTIR EN  FACTOR LA VARIABLE DEL GANADOR PARA PODER MAPEAR
tempoB$CVE_GANADOR <- as.factor(tempoB$CVE_GANADOR)


#Guardar base limpia
write.csv(tempoB, paste(dir4, "computos_elecciones_puebla_por_seccion_2019.csv", sep="/"), fileEncoding ="UTF-8", row.names=F)


###################################################################################

####################################################################################

####### MAPA!

#HACER POLIGONO. PRIMER PASO

#Llamar shapefiles
secc_puebla <- shapefile("/media/hectorpina/yanipaperdisk/Proyectos R/elecciones_puebla_2019/data/in/mapas_puebla/SECCION.shp")

#EXPLORAR OBJETO
head(secc_puebla@data)

#DIBUJAR MAPA
plot(secc_puebla)

#EXPLORAR POLIGONO
secc_puebla@data
head(secc_puebla)
str(secc_puebla)
View(secc_puebla@data)


#VOLVER CARACTER MI VARIABLE PARA UNIR
secc_puebla$seccion <- as.character(secc_puebla$seccion)


#RECODIFICAR EL MAPA
secc_puebla <- spTransform(secc_puebla, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#VOLVER DATA FRAME EL POLIGONO
puebla_seccDF <- fortify(secc_puebla, region = "seccion")

#Probar Mapa
ggplot() +
  geom_polygon(data = puebla_seccDF, aes(x=long, y=lat, group=group)) +
  coord_map()


#MERGE
seccionesDF <- merge(puebla_seccDF, tempoB, by.x="id", by.y="SECCION", all.y=T)

#Remover objetos
rm(secc_puebla, puebla_seccDF, tempoB)

####################################################################################

#MAPA DE VOTOS GANADOR POR MUNICIPIO DE PUEBLA

cols <- c("1" = "red", "2" = "blue", "3" = "brown", "4" = "gray")

map <-  ggplot() + 
  geom_polygon(data=seccionesDF, aes(x=long, y=lat, fill=CVE_GANADOR, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_manual(name="Candidatos", values = cols,  labels = c("Alberto Jiménez", "Enrique Cárdenas", "Miguel Barbosa"))+
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Secciones ganadas por candidato", caption ="Fuente: INE, Cómputos finales, 06/06/2019" , color="black") +
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


ggsave(paste(dir3, "computos_seccion_candidatos_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


####################################################################################

##### MAPA DE VOTOS DE MIGUEL BARBOSA EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=seccionesDF, aes(x=long, y=lat, fill=mb_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_gradient2(name="% votos", low = "#ffffff", mid = "#ccb4b4", high = "#570808", midpoint = 40) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de votos de Miguel Barbosa por sección", caption ="Fuente: INE, Cómputos finales, 06/06/2019" , color="black") +
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

ggsave(paste(dir3, "computos_barbosa_votos_secciones_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


##### MAPA DE ENRIQUE CARDENAS EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=seccionesDF, aes(x=long, y=lat, fill=ec_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_gradient2(name="% votos", low = "#ffffff", mid = "#a9bad3", high = "#1c3449", midpoint = 30) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de votos de Enrique Cárdenas por sección", caption ="Fuente: INE, Cómputos finales, 06/06/2019" , color="black") +
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


ggsave(paste(dir3, "computos_cardenas_votos_seccion_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


##### MAPA DE VOTOS DE ALBERTO JIMENEZ MERINO EN ELECCIONES 2019

map <- ggplot() +
  geom_polygon(data=seccionesDF, aes(x=long, y=lat, fill=aj_porc, group=group), color = "black", size = 0.1) +
  coord_map() +
  scale_fill_gradient2(name="% votos", low = "#ffffff", mid = "#f19999", high = "#c60000", midpoint = 30) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  labs(y="", x="", title="Elección de gobernador - Puebla 2019", subtitle="Porcentaje de votos de Alberto Jiménez por sección", caption ="Fuente: INE, Cómputos finales, 06/06/2019" , color="black") +
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


ggsave(paste(dir3, "computos_jimenez_votos_seccion_puebla2019.png", sep="/"), plot=map, width = 12, height = 12)


##############################################################################################

##### Remover objetos
rm(seccionesDF, map)
