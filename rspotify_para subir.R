#Rspotify

install.packages("Rspotify")
library(Rspotify)
install.packages("httpuv")
library(httpuv)
library(stringi)
library(stringr)
library(tidyverse)

########### Extraer Top 50 ############ - el código original para extraer el top 50 es de https://github.com/cienciadedatos/datos-de-miercoles

#reemplazar keys con valores que correspondan antes de correr código

keys <- spotifyOAuth(app_id="XXX",client_id="XXX", client_secret="XXX")

#OJO, para que funcione la autenticación, en la API - edit settings - redirect url puse http://localhost:1410/
#eso recomendaban acá: https://www.r-bloggers.com/my-new-r-package/

paises_es <- c("Argentina", "Bolivia", "Chile", "Colombia", "Costa Rica",
               "Cuba","la Republica Dominicana", "Dominican Republic",
               "Ecuador", "El Salvador", "Equatorial Guinea", "España",
               "Guatemala", "Honduras", "México", "Nicaragua", "Panamá",
               "Paraguay", "Perú", "Puerto Rico", "Uruguay", "Venezuela")
user_playlists_1 <- getPlaylists("qn9el801z6l32l2whymqqs18p", token = keys)
user_playlists_2 <- getPlaylists("qn9el801z6l32l2whymqqs18p", 50, token = keys)
tops_50 <- rbind(user_playlists_1, user_playlists_2)
# encontré aparte el de venezuela que no estaba incluido
tops_50 <- rbind(tops_50, c("624oAiyjMdmpdJWIylharU", "El Top 50 de Venezuela", "suo2sbl91eeth3elwrfuq7qwn", 50))

paises <- purrr::map_chr(tops_50$name, ~ str_remove(.x, "El Top 50 de "))
bool_es <- purrr::map_lgl(paises, ~ .x %in% paises_es)
tops_50_es <- tops_50[bool_es, ]

viralcharts_user = "qn9el801z6l32l2whymqqs18p"

canciones_tops50_es <- purrr::map(tops_50_es$id[-length(tops_50_es$id)],
                                  ~ getPlaylistSongs(user_id = viralcharts_user,
                                                     .x,
                                                     token = keys))
canciones_tops50_es[[18]] <- getPlaylistSongs(user_id = "suo2sbl91eeth3elwrfuq7qwn",
                                              "624oAiyjMdmpdJWIylharU",
                                              token = keys)

dataset_canciones = tibble()
for (i in 1:length(canciones_tops50_es)) {
  dataset_canciones = rbind(dataset_canciones, cbind(canciones_tops50_es[[i]],
                                                     top = as.character(tops_50_es$name)[i],
                                                     numero = 1:nrow(canciones_tops50_es[[i]])))
}
features_canciones = tibble()
for (j in 1:nrow(dataset_canciones)) {
  features_canciones = rbind(features_canciones,
                             getFeatures(dataset_canciones$id[j], keys))
}
dataset_spotify = cbind(dataset_canciones, features_canciones)

fechas = purrr::map(unique(dataset_spotify$album_id), ~getAlbumInfo(.x, keys)[1, 6])
album_fechas =  tibble(album_id = unique(dataset_spotify$album_id),
                       fecha = as.character(unlist(fechas)))
dataset_spotify = dataset_spotify[, -2] %>%
  left_join(album_fechas, by = "album_id")

dataset_spotify = dataset_spotify %>%
  select(-id, -artist_id, - album_id, -uri, -analysis_url)

nombres_columnas = c("cancion", "popularidad", "artista", "artista_completo",
                     "album", "top_pais", "puesto", "bailabilidad", "energia",
                     "nota_musical", "volumen", "modo", "hablado", "acustico",
                     "instrumental","en_vivo", "positividad", "tempo",
                     "duracion", "tiempo_compas", "fecha")
colnames(dataset_spotify) <- nombres_columnas


#top 50 de uruguay

uruguay <- subset(dataset_spotify, top_pais=="El Top 50 de Uruguay")
View(uruguay)

#EXTRAIGO MEDIAS POR PAÍS DE BAILABILIDAD, ENERGÍA, POSITIVIDAD, MODO, POPULARIDAD

bailables_paises <- aggregate(bailabilidad~top_pais, data=dataset_spotify, mean)
bailables_paises
energia_paises <- aggregate(energia~top_pais, data=dataset_spotify, mean)
energia_paises
positividad_paises <-aggregate(positividad~top_pais, data=dataset_spotify, mean) 
positividad_paises
modo_paises <- aggregate(modo~top_pais, data=dataset_spotify, mean) 
modo_paises
popularidad_paises<- aggregate(popularidad~top_pais, data=dataset_spotify, mean)
popularidad_paises

t <- merge.data.frame(bailables_paises, energia_paises, by=c("top_pais"), all.x=TRUE)
t <- merge.data.frame(t, positividad_paises, by=c("top_pais"), all.x=T)
t <- merge.data.frame(t, modo_paises, by=c("top_pais"), all.x=T)
t <- merge.data.frame(t, popularidad_paises, by=c("top_pais"), all.x=T)
t$popularidad<- t$popularidad/100
t

#puntos con gganimate 

library(reshape2)
t.melt <- melt(t, id="top_pais")

g2 <- ggplot (t.melt, aes(x=variable, y=value, col=variable)) + 
  geom_point(stat="identity", size=5)
g2<- g2 + xlab("Variable") + ylab("") +
  theme(axis.title=element_text(size=12), 
        axis.text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12)) +
  scale_colour_discrete("")
g2 <- g2 + coord_flip()
g2

library(gganimate)
install.packages("gifski")
library(gifski)
install.packages("png")
library(png)
gral_t <- g2 + transition_states(states = top_pais, transition_length=1, state_length = 1,
                                 wrap=TRUE) + labs(title="{closest_state}")

animate(gral_t)
animate (gral_t, duration = 36, fps=20) # = 365 days/yr x 3 years x 0.25 sec/day = 274 seconds)

gral_t

anim_save('rspotify1.gif')

#graficos radiales

devtools::install_github("ricardo-bion/ggradar", 
                         dependencies=TRUE)

library(ggradar)


uruguay_venezuela_argentina <- subset(t, top_pais=="El Top 50 de Uruguay" | 
                                        top_pais=="El Top 50 de Venezuela" |
                                        top_pais=="El Top 50 de Argentina")
uruguay_venezuela_argentina$modo<- NULL
g1 <- ggradar(uruguay_venezuela_argentina)
g1

png ("radar.png", height=600, width=800)
g1
dev.off()


#MODO Menor - Es más popular en Uruguay! Por qué? Porque somos unos inherentemente depresivos?

t$modo_menor <- 1-t$modo

g3 <- ggplot (t, aes(x = reorder(top_pais, modo_menor),y= modo_menor)) + 
  geom_bar(stat="identity") +
  coord_flip() + ylab ("% de canciones en modo menor") + 
  xlab("País") +   theme(axis.title=element_text(size=12), 
                         axis.text=element_text(size=12)) + 
  scale_y_continuous(labels=scales::percent)


png("canciones modo menor.png", height=600, width=600)
g3
dev.off()

##exploración de canciones en modo menor de Uruguay, Venezuela (en el otro extremo), y Argentina (¿no tendríamos gustos musicales parecidos?)

uruguay_menor <- subset(uruguay, modo==0)

for (i in uruguay$artista) {
  print(i)
}

#base de artistas uruguayos completa
uruguay$artista

datosArtistas.uru <- data.frame()
for (i in uruguay$artista) {
  print(i)
  datosArtistas.uru <- rbind(datosArtistas.uru,
                             data.frame(artista=i, searchArtist(i, token=keys)))
}


artistas.uru <- aggregate(artista~genres, data=datosArtistas.uru, first)

#base de artistas uruguayos en modo menor

uruguay_menor$artista_completo

datosArtistas.urumenor <- data.frame()
for (i in uruguay_menor$artista) {
  print(i)
  datosArtistas.urumenor <- rbind(datosArtistas.urumenor,
                                  data.frame(artista=i, searchArtist(i, token=keys)))
}

artistas.urumenor <- aggregate(artista~genres, data=datosArtistas.urumenor, first)

#comparación venezuela

venezuela <- subset(dataset_spotify, top_pais=="El Top 50 de Venezuela")
venezuela$artista

#base de artistas venezuelaos en modo menor

venezuela_menor <- subset(venezuela, modo==0)
venezuela_menor$artista_completo

datosArtistas.venemenor <- data.frame()
for (i in venezuela_menor$artista) {
  print(i)
  datosArtistas.venemenor <- rbind(datosArtistas.venemenor,
                                   data.frame(artista=i, searchArtist(i, token=keys)))
}


artistas.venemenor <- aggregate(artista~genres, data=datosArtistas.venemenor, first)

#artistas argentina

argentina <- subset(dataset_spotify, top_pais=="El Top 50 de Argentina")
View(argentina)


argentina$artista[argentina$artista=="ROSALÍA"]<-"Rosalia" #La palabra ROSALÍA me hacía saltar un error...

datosArtistas.arg <- data.frame()
for (i in argentina$artista) {
  print(i)
  datosArtistas.arg <- rbind(datosArtistas.arg,
                             data.frame(artista=i, searchArtist(i, token=keys)))
}


artistas.arg <- aggregate(artista~genres, data=datosArtistas.arg, first)


argentina_menor <- subset(argentina, modo==0)
datosArtistas.argmenor <- data.frame()
for (i in argentina_menor$artista) {
  print(i)
  datosArtistas.argmenor <- rbind(datosArtistas.argmenor,
                                  data.frame(artista=i, searchArtist(i, token=keys)))
}
artistas.argmenor <- aggregate(artista~genres, data=datosArtistas.argmenor, first)


##Recodifico los géneros

grepl("trap",artistas.urumenor$genres)

artistas.urumenor$genero_rec <- ifelse(
  grepl("trap", artistas.urumenor$genres)==T, "Trap", 
  ifelse(grepl("cumbia", artistas.urumenor$genres)==T, "Cumbia",
         ifelse(grepl("reggaeton", artistas.urumenor$genres)==T, "Reggaeton",
                ifelse(grepl("pop", artistas.urumenor$genres)==T, "Pop", "Otro")))
)

artistas.uru$genero_rec <- ifelse(
  grepl("trap", artistas.uru$genres)==T, "Trap", 
  ifelse(grepl("cumbia", artistas.uru$genres)==T, "Cumbia",
         ifelse(grepl("reggaeton", artistas.uru$genres)==T, "Reggaeton",
                ifelse(grepl("pop", artistas.uru$genres)==T, "Pop", "Otro")))
)

#géneros argentina

artistas.arg$genero_rec <- ifelse(
  grepl("trap", artistas.arg$genres)==T, "Trap", 
  ifelse(grepl("cumbia", artistas.arg$genres)==T, "Cumbia",
         ifelse(grepl("reggaeton", artistas.arg$genres)==T, "Reggaeton",
                ifelse(grepl("pop", artistas.arg$genres)==T, "Pop", "Otro")))
)


artistas.argmenor$genero_rec <- ifelse(
  grepl("trap", artistas.argmenor$genres)==T, "Trap", 
  ifelse(grepl("cumbia", artistas.argmenor$genres)==T, "Cumbia",
         ifelse(grepl("reggaeton", artistas.argmenor$genres)==T, "Reggaeton",
                ifelse(grepl("pop", artistas.argmenor$genres)==T, "Pop", "Otro")))
)

#venezuela


grepl("trap",artistas.venemenor$genres)

artistas.venemenor$genero_rec <- ifelse(
  grepl("trap", artistas.venemenor$genres)==T, "Trap", 
  ifelse(grepl("cumbia", artistas.venemenor$genres)==T, "Cumbia",
         ifelse(grepl("reggaeton", artistas.venemenor$genres)==T, "Reggaeton",
                ifelse(grepl("pop", artistas.venemenor$genres)==T, "Pop", "Otro")))
)

#PONDERO ARTISTAS X N CANCIONES EN TOP 50 y grafico uruguay (modo menor)

uruguay_menor$uno <- 1
canciones_por_artista_uru_menor <- aggregate(uno~artista, data=uruguay_menor, sum)

artistas.urumenor <- merge(artistas.urumenor, canciones_por_artista_uru_menor, by=c("artista"), all.x=TRUE)

gg <-ggplot (artistas.urumenor, aes(x=genero_rec, y=uno, fill=genero_rec)) + 
  geom_bar(stat="identity", position="stack") + xlab("Género musical") + ylab("Cantidad de canciones en top 50") +
  ggtitle("Uruguay: canciones en modo menor") + theme(legend.position="", 
                                                      axis.text=element_text(size=12), 
                                                      axis.title=element_text(size=12)) +
  scale_y_continuous(breaks=c(2,4,6,8,10,12), limits=c(0,12))
gg

#pondero y grafico argentina_menor

argentina_menor$uno <- 1
canciones_por_artista_arg_menor <- aggregate(uno~artista, data=argentina_menor, sum)

artistas.argmenor <- merge(artistas.argmenor, canciones_por_artista_arg_menor, by=c("artista"), all.x=TRUE)

gg2 <-ggplot (artistas.argmenor, aes(x=genero_rec, y=uno, fill=genero_rec)) + 
  geom_bar(stat="identity", position="stack") + xlab("Género musical") + ylab("Cantidad de canciones en top 50") +
  ggtitle("Argentina: canciones en modo menor") + theme(legend.position="", 
                                                        axis.text=element_text(size=12), 
                                                        axis.title=element_text(size=12)) +
  scale_y_continuous(breaks=c(2,4,6,8,10,12), limits=c(0,12))
gg2


#pondero y grafico venezuela (modo menor)

venezuela_menor$uno <- 1
canciones_por_artista_vene_menor <- aggregate(uno~artista, data=venezuela_menor, sum)

artistas.venemenor <- merge(artistas.venemenor, canciones_por_artista_vene_menor, by=c("artista"), all.x=TRUE)

gg3 <-ggplot (artistas.venemenor, aes(x=genero_rec, y=uno, fill=genero_rec)) + 
  geom_bar(stat="identity", position="stack") + xlab("Género musical") + ylab("Cantidad de canciones en top 50") +
  ggtitle("Venezuela: canciones en modo menor") + theme(legend.position="", 
                                                        axis.text=element_text(size=12), 
                                                        axis.title=element_text(size=12)) +
  scale_y_continuous(breaks=c(2,4,6,8,10,12), limits=c(0,12))
gg3


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

png("canciones en modo menor_arg uru vene.png", height=800, width=800)
multiplot(gg, gg2, gg3, cols=2)
dev.off()

