library(tidyverse)
library(maps)
library(viridis)
library(sf)
library(foreign)
library(RColorBrewer)
library(extrafontdb)
library(extrafont)
library(lubridate)
library(gganimate)
loadfonts(device = "win", quiet = TRUE)

#Descarga de datos de RICYT
#http://app.ricyt.org/ui/v3/comparative.html?indicator=CINVFPFDI&start_year=2007&end_year=2016

#Descarga de datos de UIS 
#http://data.uis.unesco.org/ - Categoría Percentage of female graduates by field of study

#Datos:

capitulos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/capitulos_rladies.csv")

RICYT <- read.csv2("RICYT.csv")
UIS <- read.csv("EDULIT_DS_28062019082354049.csv")

#Preprocesamiento: % de mujeres en STEM. Se usan datos de UIS (Unicef Institute of Statistics).
#Se completan todos los valores perdidos que sea posible con datos de RICYT (Red de Indicadores de Ciencia y Tecnología -Iberoamericana e Interamericana-)
#(Igualmente quedan valores perdidos)

stem <- RICYT %>% filter(X.2=="Ingeniería y Tecnología") %>%
  mutate (porcentaje=ifelse(is.na(X2016)==FALSE, X2016*100, 
                            ifelse(is.na(X2015)==FALSE, X2015*100,
                                   ifelse(is.na(X2014)==FALSE, X2014*100, X2013*100)))) %>%
  mutate(COUNTRY=País)

stem2 <- UIS %>% filter(Indicator=="Percentage of graduates from Science, Technology , Engineering and Mathematics programmes in tertiary education who are female (%)") %>%
  group_by(Country) %>% filter(Time==max(Time)) %>%
  mutate(COUNTRY=Country)

stem3 <- stem2 %>% select(COUNTRY, Value)

stem4 <- merge(stem, stem3, by=c("COUNTRY"), all=TRUE)

stem_final <- stem4 %>% select(COUNTRY, Value, porcentaje) %>%
  mutate(mujeres_en_stem=ifelse(is.na(Value)==FALSE, Value, porcentaje)) %>%
  mutate(cat_stem=ifelse(is.na(mujeres_en_stem)==TRUE, NA, 
                         ifelse(mujeres_en_stem<20, "Hasta 20%", 
                                ifelse(mujeres_en_stem<30, "Entre 20 y 29%",
                                       ifelse(mujeres_en_stem<40, "Entre 30 y 39%",
                                              ifelse(mujeres_en_stem<50, "Entre 40 y 49%", "50% o mas"))))))


#descarga de shapefile https://tapiquen-sig.jimdo.com/english-version/free-downloads/world/ 

#join stem_final al shape 

dbf.world <- read.dbf("World_Countries.dbf")

dbf.world.2 <- dbf.world %>% 
  left_join(stem_final, by= "COUNTRY")

write.dbf(dbf.world.2, "World_Countries.dbf")

#read shape 

mundo <- st_read("World_Countries.shp", quiet = T)

#Ploteo simple

mapa <- ggplot(mundo) + 
  geom_sf(data=mundo)+
  geom_sf(aes(fill=cat_stem))+
  theme_bw() + scale_fill_viridis(name="% de mujeres dentro de graduados en STEM", 
                                  limits=c("50% o más", "Entre 40 y 49%", 
                                           "Entre 30 y 39%", "Entre 20 y 29%",
                                           "Hasta 20%", NA),
                                    option="inferno", discrete=T)
capitulos_rladies$año <- year(capitulos_rladies$creacion)
capitulos_rladies$group <- seq_len(nrow(capitulos_rladies))


mapa2 <- mapa + geom_point(data=capitulos_rladies, 
                           aes(x=longitud, y=latitud, size=miembros, group=group), 
                           col="steelblue3", alpha=.7)

mapa2 <- mapa2 + labs(title="Capítulos de RLadies en el mundo y mujeres en STEM",
                        x="",
                        y="",
                        size="Miembros del capítulo local",
                        caption="Visualización: @danidlsa | Datos: @R4DS_es, UIS, RICYT") +
  theme(title=element_text(size=14, face="bold", family="Cambria"),
        legend.text=element_text(size=12, family="Cambria"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.title=element_text(size=12, face="bold", family="Cambria"))
mapa2

png ("rladies.png", height=600, width=900)
mapa2
dev.off()

#ploteo animado

mapa_anim <- mapa2 + 
  transition_states(transition_length=1, states = año, wrap=FALSE, state_length=1) +
  labs(subtitle="{closest_state}") +
  shadow_mark(colour="skyblue", alpha=.5) +
  enter_appear()


anim_save('rladies.gif', mapa_anim, width = 900, height = 600)
