
#install.packages("syuzhet")
library(tidyverse)
library(tm)
library(syuzhet)
library(SnowballC)

tabarez <- search_tweets(q = "Tabárez", n=18000)

# Palabras más utilizadas

tabarez$tuit_clean <- gsub("http\\S+","", tabarez$text)

custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))

x_topicos <- tabarez %>%
  select(tuit_clean)

x_topicos <- x_topicos %>% 
  unnest_tokens(word, tuit_clean) %>%
  anti_join(custom_stop_words) %>%
  count(word, sort = TRUE)

x_topicos <- x_topicos %>% mutate(not_number= is.na(as.integer(word))) %>%
  filter(not_number==T)
x_topicos$not_number <- NULL

x_topicos <- x_topicos[order(-x_topicos$n),]
x_topicos_20 <- head(x_topicos, 20)


ggplot(x_topicos_20, aes(x=reorder(word, n), y=n)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y="N°",
       x="Palabra",
       title=paste0("20 palabras más mencionadas")) +
  geom_text(data=subset(x_topicos, n!=max(n)), aes(label=n), size=3, hjust=-.5) +
  geom_text(data=subset(x_topicos, n==max(n)), aes(label=n), size=3, hjust=1, color="white") +   
  theme_bw() +
  scale_color_brewer(palette="Dark2") +
  theme(legend.position = "",
        axis.title=element_text(size=12),
        plot.title=element_text(face="bold", size=14),
        strip.text=element_text(size=12), 
        axis.text.x=element_text(angle=90),
        plot.subtitle=element_text(size=13, face="italic"))


## Sentiment analysis


#Limpieza del texto 
tabarez <- gsub("http.*","",tabarez$text)
tabarez <- gsub("https.*","",tabarez)

tabarez <- gsub("#\\w+","",tabarez)
tabarez <- gsub("@\\w+","",tabarez)

tabarez <- gsub("[[:punct:]]","",tabarez)
tabarez <- gsub("\\w*[0-9]+\\w*\\s*", "",tabarez)


#Transformamos la base de textos importados en un vector para
#poder utilizar la función get_nrc_sentiment
palabra.df <- as.vector(tabarez)

#Aplicación de la función del paquete syuzhet
emocion.df <- get_nrc_sentiment(char_v = palabra.df, language = "spanish")

# Aquí podemos chequear si los resultados son lógicos:
emocion.df2 <- cbind(tabarez, emocion.df)

# Construcción de data frame agregada
emocion.df3 <- data.frame(t(emocion.df))
emocion.df3 <- data.frame(rowSums(emocion.df3))
names(emocion.df3)[1] <- "cuenta"
emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)
rownames(emocion.df3) <- NULL
print(emocion.df3)

# Salvo en csv y hago traducción manual, y agrego una variable para el color del gráfico.
# PD. esto se podría hacer automático, pero las emociones son muy poquitas, hay que elegir las batallas
write.csv2(emocion.df3, "Emociones tabarez.csv")

rm(list=ls())

sentimientos <- read.csv2("Emociones tabarez.csv")


ggplot(sentimientos, aes(x=reorder(sentimiento_es, porcentaje), y=porcentaje, fill=bueno_malo)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y="% tuits", x="Sentimiento", title="¿Qué emociones despierta la palabra \n'TABAREZ' en Twitter hoy?",
       subtitle="Fecha: 19/11/2021",
       caption="@danidlsa") +
  theme_bw() +
  theme(text=element_text(face="bold"),
        plot.title=element_text(size=14, face="bold"),
        axis.text.y=element_text(size=12, face="bold"),
        legend.position="") +
  scale_fill_manual(values=c("darkgreen", "red")) +
  geom_text(aes(label=round(porcentaje, digits=1)), size=3, nudge_y=-1)

ggsave("Sentimientos Tabarez.png", height=12, width=16, units="cm")

## AUF

rm(list=ls())
auf <- search_tweets(q="la AUF", n=18000)


#Limpieza del texto 
auf <- gsub("http.*","",auf$text)
auf <- gsub("https.*","",auf)

auf <- gsub("#\\w+","",auf)
auf <- gsub("@\\w+","",auf)

auf <- gsub("[[:punct:]]","",auf)
auf <- gsub("\\w*[0-9]+\\w*\\s*", "",auf)


#Transformamos la base de textos importados en un vector para
#poder utilizar la función get_nrc_sentiment
palabra.df <- as.vector(auf)

#Aplicación de la función del paquete syuzhet
emocion.df <- get_nrc_sentiment(char_v = palabra.df, language = "spanish")

# Aquí podemos chequear si los resultados son lógicos:
emocion.df2 <- cbind(auf, emocion.df)

# Construcción de data frame agregada
emocion.df3 <- data.frame(t(emocion.df))
emocion.df3 <- data.frame(rowSums(emocion.df3))
names(emocion.df3)[1] <- "cuenta"
emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)
rownames(emocion.df3) <- NULL
print(emocion.df3)

# Salvo en csv y hago traducción manual, y agrego una variable para el color del gráfico.
# PD. esto se podría hacer automático, pero las emociones son muy poquitas, hay que elegir las batallas
write.csv2(emocion.df3, "Emociones auf.csv")

rm(list=ls())

sentimientos <- read.csv2("Emociones auf.csv")

ggplot(sentimientos, aes(x=reorder(sentimiento_es, porcentaje), y=porcentaje, fill=bueno_malo)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y="% tuits", x="Sentimiento", title="¿Qué emociones despierta la palabra \n'AUF' en Twitter hoy?",
       subtitle="Fecha: 19/11/2021",
       caption="@danidlsa") +
  theme_bw() +
  theme(text=element_text(face="bold"),
        plot.title=element_text(size=14, face="bold"),
        axis.text.y=element_text(size=12, face="bold"),
        legend.position="") +
  scale_fill_manual(values=c("darkgreen", "red")) +
  geom_text(aes(label=round(porcentaje, digits=1)), size=3, nudge_y=-1)

ggsave("Sentimientos auf.png", height=12, width=16, units="cm")

