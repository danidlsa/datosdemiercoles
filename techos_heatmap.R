setwd("C:/Users/Usuario/Dropbox/datos de miércoles/30 map challenge/Viviendas_dbf_8_2013")
library(foreign)
library(tidyverse)

#Census data and shapefiles available for download at http://www.ine.gub.uy 

viv <- read.dbf("Viviendas.dbf")
vtable::vtable(viv)

viv <-  viv %>% mutate(techos=ifelse(VIVDV02>1, 1, 0)) %>%
  mutate(pisos=ifelse(VIVDV03>1, 1, 0)) %>%
  mutate(agua=ifelse(VIVDV05>1, 1, 0)) %>% 
  mutate(count=1) %>%
  mutate(CODSEG=paste0("1",SECC,SEGM))

segm_techos <- aggregate(techos~CODSEG, viv, sum) 
segm_pisos <- aggregate(pisos~CODSEG, viv, sum) 
segm_agua <- aggregate(agua~CODSEG, viv, sum) 
segm_total <- aggregate(count~CODSEG, viv, sum)

segm_total <- segm_total %>% left_join(segm_techos, by="CODSEG") %>%
  left_join(segm_pisos, by="CODSEG") %>%
  left_join(segm_agua, by="CODSEG")

segm_total <- segm_total %>% 
  mutate(por_techos=techos/count*100) %>%
  mutate(por_pisos=pisos/count*100) %>%
  mutate(por_agua=agua/count*100)

segm_total$CODSEG <- as.numeric(segm_total$CODSEG)

dbf.segm <- read.dbf("ine_seg_11.dbf") %>%
  left_join(segm_total, by="CODSEG")

write.dbf(dbf.segm, "ine_seg_11.dbf")

library(sf)
#library(maps)
mapa <- st_read("ine_seg_11.shp") %>% filter(DEPTO==1)

library(viridis)
library(extrafont)

mid <- median(mapa$por_techos, na.rm=T)+10

#Un ejemplo de heatmap
g1 <- ggplot() + geom_sf(data=mapa, aes(fill=por_techos)) +
  theme_void() +
  scale_fill_gradient2(name="% viviendas con techos de chapa \n(y otros materiales livianos)",
                      midpoint = mid, low = "darkblue", mid = "white",
                      high = "red", space = "Lab" )+
  theme(text=element_text(family="Segoe UI"),
        plot.title=element_text(size=16, face="bold", hjust=.5),
        plot.subtitle=element_text(size=14, face="italic", hjust=.5),
        plot.caption=element_text(size=12, face="italic"),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12, face="bold"),
        legend.position="bottom") +
  labs(title="Construcciones precarias en Montevideo",
       subtitle="Material predominante en el techo",
       caption="Fuente de datos: Censos 2011 | Visualización: @danidlsa")

g1

ggsave("techos.png", g1, height=14, width=18, units="cm")
