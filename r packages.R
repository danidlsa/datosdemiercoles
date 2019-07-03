
library(tidyverse)
library(extrafontdb)
library(extrafont)
loadfonts(device = "win", quiet = TRUE)
descargas_R <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-03/descargas_R.csv")
paquetes_cran <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-03/paquetes_CRAN.csv")

install.packages("cranlogs")
library(cranlogs)


r <- cran_top_downloads("last-week")
r <- r %>% mutate(tidyverse_packages=ifelse(package=="ggplot2", "Dentro de Tidyverse",
                                            ifelse(package=="dplyr", "Dentro de Tidyverse",
                                                   ifelse(package=="tidyr", "Dentro de Tidyverse",
                                                          ifelse(package=="readr", "Dentro de Tidyverse",
                                                                 ifelse(package=="purrr", "Dentro de Tidyverse",
                                                                        ifelse(package=="tibble", "Dentro de Tidyverse",
                                                                               ifelse(package=="stringr", "Dentro de Tidyverse",
                                                                                      ifelse(package=="forcats", "Dentro de Tidyverse", "Fuera de Tidyverse")))))))))

g <- ggplot(r, aes(x=reorder(package, -rank), y=count, col=package, label=count)) + 
  facet_grid(tidyverse_packages~., scales="free", space="free") + 
  geom_point(stat="identity", size=5) +
  geom_density(stat="identity", color="black", size=0.1) + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position="",
        strip.text.y=element_text(size=12, angle=0, face="bold", family="Segoe UI"),
        axis.text.y=element_text(size=12, family="Segoe UI"),
        axis.title.x=element_text(size=12, face="bold", family="Segoe UI"),
        axis.text.x=element_text(size=11, family="Segoe UI"),
        title=element_text(family="Segoe UI", face="bold")) +
  scale_color_viridis(discrete=T) +
  labs(x="Paquete", y="Número de descargas",
       title="10 paquetes más descargados desde CRAN",
       subtitle="[Entre el 26/6/19 y el 2/7/2019]",
       caption="Visualización: @danidlsa | Fuente de datos: paquete cranlogs") +
  geom_text(size=3, nudge_y = 1, hjust=-0.3, col="black") +
  scale_y_continuous(limits=c(0, 640000), breaks=c(0,200000, 400000, 600000))


png("cranlogs.png", height=480, width=680)
g
dev.off()
