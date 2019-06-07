#datos de miércoles - airbnb

library(tidyverse)
library(foreign)
#install.packages("maps")
library(maps)
library(sf)


buenos_aires <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-05/buenos_aires.csv")
cdmx <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-05/cdmx.csv")
rio <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-05/rio.csv")
santiago <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-05/santiago.csv")


ba <- aggregate(precio~barrio, buenos_aires, mean)
sch <- aggregate(precio~barrio, santiago, mean)
mex <- aggregate(precio~barrio, cdmx, mean)
rj <- aggregate(precio~barrio, rio, mean)

aplicar_log <- function(base) {
  ylog <- log(base$precio)
  base <- cbind (base, ylog)
}

rj <- aplicar_log(rj)
sch<- aplicar_log(sch)
ba<- aplicar_log(ba)
mex<-aplicar_log(mex)

g1 <- ggplot(ba, aes(x=reorder(barrio,-ylog),y= ylog)) + geom_point(stat="identity") + coord_flip()
g2  <- ggplot(sch, aes(x=reorder(barrio,-ylog),y= ylog)) + geom_point(stat="identity") + coord_flip()
g3  <- ggplot(mex, aes(x=reorder(barrio,-ylog),y= ylog)) + geom_point(stat="identity") + coord_flip()
g4  <- ggplot(rj, aes(x=reorder(barrio,-ylog),y= ylog)) + geom_point(stat="identity") + coord_flip()

mapa<- function(pais) {
  ggplot(pais) + 
    geom_sf(data=pais)+
    geom_sf(aes(fill=-ylog))+
    theme_bw()
  
}



##CABA

#shapes de bs as en: http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios-zip.zip
#unzip en wd

dbf.ba <- read.dbf("barrios_badata.dbf")

ba$BARRIO <- toupper(ba$barrio)
dolar_peso_argentino <- 44.89
ba$precio_dolar <- ba$precio / dolar_peso_argentino
ba
dbf.ba.prueba <- dbf.ba %>% 
  left_join(ba, by= "BARRIO")

write.dbf(dbf.ba.prueba, "barrios_badata.dbf")

CABA <- st_read("barrios_badata.shp", quiet = T)


mapa_CABA <- mapa(CABA)
mapa_CABA

mapa_CABA <- mapa_CABA + scale_fill_continuous (type="viridis", name="Precio", breaks=c(-6.5, -9),
                         labels=c("Más barato", "Más caro")) + ggtitle("CABA")
mapa_CABA

png("CABA.png", width=400, height=400)
mapa_CABA
dev.off()


##SANTIAGO
# shapefile descargados de https://www.bcn.cl/siit/mapas_vectoriales/index_html - 
# seleccioné Provicia=="Santiago" en arcgis

dbf.sch <- read.dbf("comunas_santiago_prov.dbf")

dbf.sch$BARRIO <- as.character(dbf.sch$NOM_COM)
Encoding(dbf.sch$BARRIO) <-"UTF-8"

sch$BARRIO <- sch$barrio
sch$barrio <- NULL

dbf.sch.merge <- dbf.sch %>% 
  left_join(sch, by= "BARRIO")

dolar_peso_chileno <- 0.0014

dbf.sch.merge$precio_dolar <- dbf.sch.merge$precio*0.0014

write.dbf(dbf.sch.merge, "comunas_santiago_prov.dbf")

SANTIAGO <- st_read("comunas_santiago_prov.shp", quiet = T)

mapa_santiago <- mapa(SANTIAGO)
mapa_santiago
mapa_santiago <- mapa_santiago + scale_fill_continuous (type="viridis", name="Precio", 
                                        breaks=c(-10, -12.5),
                                        labels=c("Más barato", "Más caro")) +
  ggtitle("Santiago")
mapa_santiago

png("SANTIAGO.png", width=400, height=400)
mapa_santiago
dev.off()

##RÍO DE JANEIRO
#shapefile descargado de: http://www.data.rio/datasets/limite-bairro


dbf.rj <- read.dbf("Limite_Bairro.dbf")

rj$BARRIO <- rj$barrio
dolar_real <- 3.88
rj$precio_dolar <- rj$precio / dolar_real
rj

dbf.rj$BARRIO <- as.character(dbf.rj$BAIRRO)
Encoding (dbf.rj$BARRIO) <- "UTF-8"
dbf.rj.merge <- dbf.rj %>% 
  left_join(rj, by= "BARRIO")

write.dbf(dbf.rj.merge, "Limite_Bairro.dbf")

RIO <- st_read("Limite_Bairro.shp", quiet = T)


mapa_RIO <- mapa(RIO)
mapa_RIO
mapa_RIO <- mapa_RIO + scale_fill_continuous (type="viridis", name="Precio", breaks=c(-4, -8),
                                                labels=c("Más barato", "Más caro")) + ggtitle("Río")
mapa_RIO

png("RIO.png", width=600, height=400)
mapa_RIO
dev.off()

##CIUDAD DE MÉXICO


