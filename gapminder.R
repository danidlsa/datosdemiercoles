#Relación entre esperanza de vida y GDP por cápita según continente y año

#install.packages("readr")
#install.packages("gganimate")
#install.packages("gifski")
#install.packages("png")
library (gifski)
library(readr)
library (ggplot2)
library (gganimate)
library (png)

#Data
gapminder <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv")

#Ggplot estático para usar como base
gral <- ggplot(gapminder, aes(x = pib_per_capita, y=esperanza_de_vida, size = poblacion, 
                           col = pais)) + geom_point(show.legend = FALSE) +
  facet_wrap(~continente)
gral <- gral + xlab ("PBI per cápita") + ylab ("Esperanza de vida")
gral

#Animación -
gral_t <- gral + transition_time(anio) +
  labs(title="Año: {round(frame_time)}") +  
  shadow_wake(wake_length = 0.1, alpha = FALSE)

gral_t

#Save
anim_save('gral_t.gif')

