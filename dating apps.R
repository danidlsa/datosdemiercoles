apps <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-24/apps_googleplaystore.csv")

table(apps$clasificacion_contenido)

library(tidyverse)
library(extrafontdb)
library(extrafont)
library(RColorBrewer)
install.packages("ggrepel")
library(ggrepel)

windowsFonts(sans="Segoe UI")
loadfonts(device="win")
loadfonts(device="postscript")



dating_apps <- apps %>% filter ((categoria=="Conocer personas" & (clasificacion_contenido=="Maduro" | clasificacion_contenido=="Adultos unicamente")) | 
                                  app=="Tinder" | 
                                  grepl("dating", app)==TRUE |
                                  grepl("Dating", app)==TRUE |
                                  grepl("gay", app)==TRUE |
                                  grepl("Gay", app)==TRUE |
                                  grepl("Lesbian", app)==TRUE |
                                  grepl("lesbian", app)==TRUE)
                                  
  
dating_apps <- dating_apps %>% 
  group_by(app) %>% 
  summarize (instalaciones=max(instalaciones), 
             categoria=first(categoria),
             calificacion=max(calificacion),
             tamanio=first(tamanio),
             tipo=first(tipo),
             resenias=max(resenias),
             precio=max(precio),
             clasificacion_contenido=first(clasificacion_contenido))
dating_apps <- dating_apps[order(-dating_apps$instalaciones),] 

dating_apps_importantes <- dating_apps %>% filter(instalaciones>1e+06)
count(dating_apps_importantes)
#24 aplicaciones de citas (de un total de 168) superan el millón de descargas

g1 <- ggplot(dating_apps_importantes, aes(x=reorder(app, instalaciones), y=instalaciones, fill=as.factor(instalaciones))) + geom_bar(stat="identity") +
  coord_flip() +
  theme_dark() +
  scale_fill_brewer("Número de instalaciones", palette="PuRd") +
  labs(y="Instalaciones", x="", title="Dating apps más descargadas | Playstore",
       caption="Datos: @R4DS_es | Visualización: @danidlsa")

g2 <- ggplot(dating_apps_importantes, aes(calificacion, resenias, size=as.factor(instalaciones))) + 
  geom_point(aes(color=as.factor(instalaciones))) +
  theme_dark() +
  scale_color_brewer("Número de instalaciones", palette="PuRd") +
  scale_size_discrete("Número de instalaciones") +
  labs(x="Calificación", y="Número de reseñas", 
       title="Dating apps más descargadas: Calificación y reseñas",
       caption="Datos: @R4DS_es | Visualización: @danidlsa") +
  geom_text_repel(data = subset(dating_apps_importantes, instalaciones>=5e+07), aes(label = app),
                  size=4, color="white")

png("dating apps_barras.png", height=480, width=680)
g1
dev.off()

png("dating apps_puntos.png", height=480, width=600)
g2
dev.off()
