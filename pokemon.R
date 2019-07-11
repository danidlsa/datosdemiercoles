pokemon <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-10/pokemon.csv")

library(tidyverse)

#devtools::install_github("schochastics/Rokemon")
library(Rokemon)
library(extrafont)
library(extrafontdb)
library(cowplot)
library(ggimage)

#extrafont::font_import() #only run ones
extrafont::loadfonts()

pk <- pokemon %>% filter(ID_poke>=1 & ID_poke<=9) %>%
  mutate(mega_evol=ifelse(grepl("Mega", nombre_traducido)==TRUE, 1,0)) %>%
  filter(mega_evol==0) %>%
  mutate(nombre_lower=tolower(nombre_ingles))
 
g1 <- ggplot(pk, aes(ataque, defensa)) +
  geom_pokemon(aes(image=nombre_lower), size=.15) +
  theme_gba() +
  labs(title="Iniciales: 1a generación",
       x="Ataque",
       y="Defensa",
       caption="@danidlsa") +
  scale_y_continuous(limits=c(40,105))

g2 <- pk %>% gghealth("nombre_traducido","puntos_vida",init.size = 5)+
  labs(x="",y="Puntos de vida", caption="@danidlsa")
g2

g3 <- ggplot(pk, aes(fuerza_especial_ataque, velocidad)) +
  geom_pokemon(aes(image=nombre_lower), size=.15) +
  theme_gameboy() +
  labs(title="Iniciales: 1a generación",
       y="Velocidad",
       x="Fuerza ataque especial",
       caption="@danidlsa")
g3

png("gba_advanced.png", height=480, width=480)
g1
dev.off()

png("gghealth.png", height=480, width=480)
g2
dev.off()

png("gameboy.png", height=480, width=480)
g3
dev.off()
