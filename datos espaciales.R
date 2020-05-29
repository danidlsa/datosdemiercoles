library(tidyverse)
library(ggmap)
library(extrafont)

register_google(key = "***")


mdeo_watercolor <- get_map(location="Montevideo", zoom=13,
                           maptype=c("watercolor"))

ggmap(mdeo_watercolor) + theme_void() +
  labs(title="Montevideo desierta",
       subtitle="Acuarelas con Google Maps API",
       caption="Visualización: @danidlsa") +
  theme(plot.title=element_text(size=14, face="bold", hjust=.5, family="Akbar", color="#46a830"),
        plot.subtitle=element_text(size=12, hjust=.5, family="Akbar", color="#46a830"),
        plot.caption=element_text(size=10, family="Segoe UI", hjust=.5, color="#454444"))

ggsave("Montevideo desierta.png", height=10, width=10, units="cm")
