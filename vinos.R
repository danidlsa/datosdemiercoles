vinos <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-12/vinos.csv")

library(tidyverse)
library(ggridges)
#install.packages("cowplot")
library(cowplot)

sudam <- subset (vinos, pais=="Uruguay" | pais=="Argentina" | 
                   pais=="Chile" | pais=="Perú")


g1 <- ggplot(sudam, aes(puntos, log(precio), col=pais)) + geom_point() + 
  facet_wrap(~pais, ncol = 4) + theme(legend.position="") + 
  labs(title="Vinos sudaméricanos: Precio en función del puntaje (según Wine Enthusiast)", x="Puntaje", y="Precio (log)")
g1 <- g1 + stat_smooth(method="lm", col="black")

g2 <- ggplot(sudam, aes(pais, puntos, fill=pais)) + 
  geom_boxplot(position="dodge") + labs (title="Puntaje", x="País", y="Puntos",
                                         caption=" ") +
  theme(legend.position="")


g3 <- ggplot(sudam, aes(x = log(precio), y = pais, fill = pais, group = pais)) + 
  geom_density_ridges(alpha=0.7) + 
  labs(title = "Precio",
       x = "Precio (log)",
       y = "País",
       caption="@danidlsa") +
  theme(legend.position="") 

bottomrow <- plot_grid(g2, g3, nrow=1)

png("vinos.png", height=500, width=600)
plot_grid(g1, bottomrow, nrow=2)
dev.off()


#Análisis descriptivo

funcion_descriptivos <- function(base, Pais) {
  bar <- filter(base, Pais==pais)
  is.na(bar$precio) <- NULL
  max_bar <- max(bar$precio, na.rm=TRUE)
  min_bar <- min(bar$precio, na.rm=TRUE)
  length_bar <- length(bar$pais)  
  media_bar <- mean(bar$precio, na.rm=TRUE)
  mediana_bar <- median(bar$precio, na.rm=TRUE)
  print (c("Cantidad de casos:",length_bar))
  print (c("Precio promedio:", media_bar))
  print (c("Precio máximo:", max_bar))
  print (c("Precio mínimo:", min_bar))
  print (c("Precio en el percentil 50:", mediana_bar))
}

funcion_descriptivos (sudam, "Uruguay")
funcion_descriptivos (sudam, "Perú")
funcion_descriptivos (sudam, "Argentina")
funcion_descriptivos (sudam, "Chile")
