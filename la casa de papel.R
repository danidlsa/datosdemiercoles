#Nota: el preprocesamiento, salvo algunas modificaciones propias, es forkeado del repo de /picanum
#Ver: https://github.com/picanum/DatosDeMiercoles 

setwd("C:/Users/Usuario/Dropbox/datos de miércoles")
rm(list=ls())
dat <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv")


library(dplyr)
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
library(stringr)
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
#install.packages("fastmatch")
library(SnowballC)
library(fastmatch)
library(extrafont)
library(extrafontdb)

windowsFonts(sans="Segoe UI")
loadfonts(device="win")
loadfonts(device="postscript")

#Añadimos una variable que indique qué episodio es de todos (no sólo dentro de una temporada). Lo hacemos "a mano" mediante ifelse

dat$epi_temp <- dat$episodio + ifelse(dat$temporada == 1, 0, ifelse(dat$temporada == 2, 9, 15))

####CREACIÓN DE LA MATRIZ DE TÉRMINOS####


#Con esta función quitamos los paréntesis de los subtítulos ya que no forman parte del diálogo en sí
quitarparentesis <- function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))

#Con esta función aplicamos un preprocesamiento basado en quitar palabras stoppers, signos de puntuación, números y ponerlo todo en minúsculas
preprocesamiento <- function(x) removeWords(removePunctuation(removeNumbers(tolower(x))),stopwords("spanish"))

dat$texto2 <- preprocesamiento(quitarparentesis(dat$texto))

#La función de preprocesamiento no quita los símbolos de apertura de exclamación ni de interrogación, así que los quitamos también
dat$texto2 <- str_remove_all(dat$texto2, "¡")
dat$texto2 <- str_remove_all(dat$texto2, "¿")


dat$texto2 <- iconv(dat$texto2,from="UTF-8",to="ASCII//TRANSLIT") #Esta es una modificación al procesamiento original: cambio tipo de encoding porque saltan errores nuevos de dejarlo como está

#Creamos la matriz de términos y rescatamos aquellos que se hayan mencionado más de 50 veces a lo largo de la serie
DTM1 <- DocumentTermMatrix(Corpus(VectorSource(t(dat$texto2))))
DTM1<-as.matrix(DTM1)
DTM1<-as.data.frame(DTM1)
DTM1<-DTM1[,which(apply(DTM1,2,sum) > 10)]

#Unimos la matriz de términos al data.frame original... y dejamos respirar un poco a la memoria RAM :')
dat <- data.frame(dat,DTM1)
rm("DTM1")
gc()


#En dat2 almacenaremos el número de veces que se repite un término en cada episodio
dat2 <- reshape2::melt(dat[,c(7,9:700)], id.vars = "epi_temp")

dat2$variable <- as.character(dat2$variable)


dat2 <- aggregate(value ~ epi_temp + variable, dat2, FUN=sum)

#Acto seguido sacamos la duración de cada episodio, y la unimos a dat2
tiempos <- aggregate(tiempo_salida ~ epi_temp, dat, FUN = function(x) max(x)/60)
dat2 <- merge(dat2, tiempos, by = "epi_temp")

#Hasta acá va el reprocesamiento de @picanum. Finalmente tomo una sola base, sin considerar temporadas ni episodios

dat3 <- aggregate(value~variable, dat2, sum)

##WORDCLOUD

#install.packages("ggwordcloud")
library(ggwordcloud)

#install.packages("wordcloud2")

#library(wordcloud2)

ggplot(dat3, aes(label=variable, size=value)) + 
  geom_text_wordcloud() + 
  theme_minimal() + 
  scale_radius(range = c(0, 20), limits = c(0, NA))

library(png)

dat3 <- dat3 %>%  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))



dat3 <- dat3[order(-dat3$value),] 


#Las 4 palabras más utilizadas quedan por fuera de la nube por estar muy despegadas en el conteo. 
#Por eso lo bajo manualmente, para que no se pierdan. 
		     
head(dat3)
dat3[1,2]<- 300
dat3[2,2]<- 290
dat3[3,2]<- 285
dat3[4,2]<- 280

head(dat3)
library(grid)

##las pegatinas utilizadas están en:
# https://github.com/danidlsa/datosdemiercoles/blob/master/pegatina%202.png
# https://github.com/danidlsa/datosdemiercoles/blob/master/pegatina-dali-la-casa-de-papel_ok.png
# descargar a wd
		     
#imagen para annotation
peg <- readPNG("pegatina 2.png")
g <- rasterGrob(peg, interpolate=TRUE)

#wordcloud
x<- ggplot(dat3,
  aes(label = variable, angle=angle, color=value, size=value)) +
  geom_text_wordcloud_area(
    mask = png::readPNG("pegatina-dali-la-casa-de-papel_ok.png"),
  rm_outside = TRUE)  +
  theme_minimal() +
  scale_color_gradient(low="darkred", high="red") +
  labs (title="La casa de papel: palabras más repetidas",
	subtitle="Temporadas 1-3",
	caption="@danidlsa") +
  scale_size_area(max_size = 6) +
 annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

pdf("la casa de papel.pdf", height=6, width=6)
x
dev.off()



