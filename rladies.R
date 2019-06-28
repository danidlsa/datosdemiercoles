capitulos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/capitulos_rladies.csv")
devtools::install_github("tylermorganwall/rayshader")

library(rayshader)
library(tidyverse)
library(viridis)
library(extrafontdb)
library(extrafont)
library(maps)

loadfonts(device = "win", quiet = TRUE)


p <- ggplot() + 
  borders("world", alpha=0.1, fill="purple") +
  geom_point(data = capitulos_rladies, aes(x = longitud, y = latitud, 
                                       col=miembros)) +
  theme_minimal() + 
  scale_color_viridis(name="Cantidad de miembros", option="magma") + 
  labs(title="Capítulos de RLadies en el mundo",
       x="",
       y="",
       caption="Visualización: @danidlsa | Fuente de datos: meetup.com") +
  theme(title=element_text(size=14, face="bold", family="Helvetica LT Std Light"),
        legend.text=element_text(size=12, family="Helvetica LT Std Light"),
        axis.ticks = element_blank(),
        axis.text = element_blank())
p


plot_gg(p, width = 5,height = 5, raytrace = FALSE, preview = TRUE)
plot_gg(p, width = 5,height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta=10, phi=30, windowsize = c(800,800))
render_snapshot(clear = TRUE)

