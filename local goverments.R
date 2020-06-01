library(tidyverse)
library(vtable)
library(extrafont)
windowsFonts(sans="Roboto")
loadfonts(device="win")
loadfonts(device="postscript")
library(ggpol)


t <- read.csv("datos/ODS 5/local goverments.csv")


t<- t %>% select(GeoAreaName, TimePeriod, Value) %>%
  mutate(Mujeres=as.numeric(Value)) %>%
  mutate(Hombres=100-Mujeres) %>%
  select(-Value) %>%
  gather(Mujeres:Hombres, key="Sexo", value = "Porcentaje")

t$pais <- ifelse(t$GeoAreaName=="Brazil", "Brasil", as.character(t$GeoAreaName))

g <- ggplot(t) +
  geom_arcbar(aes(shares = Porcentaje,r0=5, r1=10, fill = Sexo)) + 
  scale_fill_manual(values=c("Mujeres"="#DB0962", "Hombres"="#6B5F65")) +
  coord_fixed() + 
  theme_void() + 
  theme(strip.text.x=element_text(size=13, face="bold")) +
  facet_wrap(~pais, ncol=2) + 
  theme(legend.position="")
g

library(png)
library(grid)
m <- readPNG("C:/Users/Usuario/Dropbox/ODS ciedur/insumos/mujer_blanco.png")
m <- rasterGrob(m, interpolate=TRUE)

h <- readPNG("C:/Users/Usuario/Dropbox/ODS ciedur/insumos/hombre_blanco.png")
h <- rasterGrob(h, interpolate=TRUE)

t$labels <- paste0(round(t$Porcentaje), "%")
textos <- subset(t, Sexo=="Mujeres") %>% select(pais, labels)

g_1 <- g + geom_text(data=textos, mapping=aes(x=9.8, y=6, label=labels),
                     size=4.7, fontface="bold", color="grey25") +
  annotation_custom(m, xmin=7, xmax=10, ymin=.5, ymax=3.2) +
  geom_curve(aes(x=10, xend=9.5, y=5.4, yend=1.5), color="grey10", arrow=arrow(length = unit(0.4, "cm")),
             curvature=-.5) +
  annotation_custom(h, xmin=-10, xmax=-7, ymin=.5, ymax=3.2)
g_1 <- g_1 + labs(title="Proporción de escaños en órganos deliberativos locales ocupados por mujeres",
                  subtitle="Año 2018",
                  caption="Fuente de datos: Base de datos oficial de Indicadores ODS desarrollada por NNUU\nVisualización: @danidlsa") +
  theme(plot.title=element_text(size=14))

g_1


ggsave("local governments_con nombre.png", g_1, height=12, width=20, units="cm")
