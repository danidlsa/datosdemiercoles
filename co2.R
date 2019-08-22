library(tidyverse)
library(extrafont)
library(extrafontdb)
library(RColorBrewer)
library(cowplot)
windowsFonts(sans="Segoe UI")
loadfonts(device="win")
loadfonts(device="postscript")

co2 <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-21/co2.csv")


co2_ingreso <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-21/co2_ingreso.csv")

co2_ingreso_filtrada <- co2_ingreso %>% filter(grupo=="Ingreso alto"|
                                                 grupo=="Ingreso bajo"|
                                                 grupo=="Ingreso medio-altos" |
                                                 grupo=="Ingreso medio-bajo") %>%
  mutate(grupo_ok=ifelse(grupo=="Ingreso medio-altos", "Ingreso medio alto",
                           ifelse(grupo=="Ingreso medio-bajo", "Ingreso medio bajo",
                                  grupo)))
theme_graficos <- theme(title=element_text(face="bold", size=14),
                          axis.title=element_text(face="bold", size=12),
                          axis.text=element_text(size=11),
                          legend.title=element_text(size=12, face="bold"),
                          legend.text=element_text(size=12))

g <- ggplot(co2_ingreso_filtrada, aes(x=anio, y=emision_co2, col=grupo_ok)) +
  geom_line(position="identity", size=1.2) +
  theme_minimal() +
  labs(x="",
       y="Emisión CO2",
       title="Emisiones de CO2 por año y países agrupados por ingreso",
       subtitle="Toneladas métricas per cápita (millones). 1960-2014") +
  scale_color_brewer("Grupo de países según ingreso", palette="Set1",
                     limits=c("Ingreso alto",
                              "Ingreso medio alto",
                              "Ingreso medio bajo",
                              "Ingreso bajo")) +
  scale_x_continuous(breaks=c(1960, 1970,1980,1990,2000,2010)) +
  theme_graficos +
  theme(legend.position="")


co2_total <- aggregate(emision_co2~anio, co2_ingreso_filtrada, sum)
co2_ingreso_filtrada_2 <- co2_ingreso_filtrada %>% left_join(co2_total, by="anio") %>%
  mutate(porcentaje_co2=emision_co2.x/emision_co2.y*100)

g2 <- co2_ingreso_filtrada_2 %>% filter(anio==1960 | anio==1970 | anio==1980 |
                               anio==1990 | anio==2000 | anio==2010) %>%
  ggplot(aes(x=anio, y=porcentaje_co2, fill=grupo_ok, label=round(porcentaje_co2))) +
  geom_bar(stat="identity", position="stack") +
  geom_text(position=position_stack(vjust=0.5), size=3) +
  scale_fill_brewer("Grupo de países según ingreso", palette="Set1",
                     limits=c("Ingreso alto",
                              "Ingreso medio alto",
                              "Ingreso medio bajo",
                              "Ingreso bajo")) +
  theme_minimal() + 
  theme_graficos +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010)) +
  labs(x="Año",
       y="Porcentaje del total de emisiones de CO2",
       title="Distribución de las emisiones de CO2 por países agrupados por ingreso",
       subtitle="Porcentaje sobre el total de emisiones. 1960-2010",
       caption="") +
  theme(legend.position="bottom")


g3 <- co2 %>% arrange(-emision_co2)%>% 
  filter(anio==2014) %>%
  head(20) %>% 
  ggplot(aes(x=reorder(pais_region, emision_co2), y=emision_co2)) +
  coord_flip() + 
  geom_point(color="sienna", size=5) +
  geom_density(stat="identity", color="black", size=0.1) +
  theme_minimal() +
  theme_graficos +
  labs(y="Emisión CO2",
       x="País",
       title="20 países con mayor emisión de CO per cápita en 2014",
       subtitle="Toneladas métricas per cápita (millones)",
       caption="Visualización: @danidlsa | Data: Banco Mundial")
  
gr<- plot_grid(g, g2, nrow=2)

todos <- plot_grid(gr, g3, nrow=1)

ggsave("CO2.png", todos, height=9, width=18)
