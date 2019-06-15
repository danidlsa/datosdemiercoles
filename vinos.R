vinos <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-12/vinos.csv")
View(vinos)

library(tidyverse)
library(ggridges)
#install.packages("cowplot")
library(cowplot)

sudam <- subset (vinos, pais=="Uruguay" | pais=="Argentina" | 
                   pais=="Chile" | pais=="Perú")


g1 <- ggplot(sudam, aes(puntos, log(precio), col=pais)) + geom_point() + 
  facet_wrap(~pais, ncol = 4) + theme(legend.position="") + 
  labs(title="Precio en función del puntaje", x="Puntaje", y="Precio (log)")
g1 <- g1 + stat_smooth(method="lm", col="black")


g2 <- ggplot(sudam, aes(pais, puntos, fill=pais)) + 
  geom_boxplot(position="dodge") + labs (title="Puntos", x="País", y="Puntos") +
  theme(legend.position="")


g3 <- ggplot(sudam, aes(x = log(precio), y = pais, fill = pais, group = pais)) + 
  geom_density_ridges(alpha=0.7) + 
  labs(title = "Precio",
       x = "Precio (log)",
       y = "País") +
  theme(legend.position="") 

toprow <- plot_grid(g2, g3, nrow=1)

png("vinos.png", height=600, width=600)
plot_grid(toprow, g1, nrow=2)
dev.off()
