#install.packages("rtweet")
library(rtweet)
library(tidyverse)
library(tidytext)
library(lubridate)
library(igraph)
library("stringr")
library("stringi")
library("tm")

key= "XXX"
secret= "XXX"
appname <- "XXX"

access_token = "XXX"
access_secret= "XXX"
# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

sindicato_miedo <- search_tweets(q = "#SindicatoDelMiedo", n=18000)
sindicato_miedo <- sindicato_miedo[order(sindicato_miedo$retweet_count),]

sindicato_miedo$count <- 1

retuiteados <- sindicato_miedo %>% filter(retweet_count>0)


sindicato_miedo <- sindicato_miedo %>% mutate(dia=as.Date(created_at))

resum <- aggregate(count~dia, sindicato_miedo, sum)

g <- ggplot(resum, aes(x=dia, y=count)) +
  geom_line(stat="identity", color="#1da1f2", size=1) +
  labs(x="Fecha", y="N° de tuits", title="Tuits con el hashtag #sindicato_miedo",
       caption="@danidlsa") +
  ggdark::dark_theme_bw() +
  theme(plot.title=element_text(color="#1da1f2"))

g

ggsave("tuits en el tiempo.png", height=10, width=13, units="cm")  


# Primeros 25 tuits

primeros25 <- sindicato_miedo[order(sindicato_miedo$created_at),]

primeros25 <- head(primeros25, n=25)

# Más retuits

originales <- sindicato_miedo %>% filter(is_retweet==F)
originales <- originales[order(-originales$retweet_count),]

# tuiteros con más tuits

tuiteros <- sindicato_miedo %>% group_by(screen_name) %>%
  count()
tuiteros <- tuiteros[order(-tuiteros$n),]
militantes <- tuiteros %>% filter(n>=10)

prueba_user <- search_users("danidlsa")

users <- users_data(sindicato_miedo)
militantes <- militantes %>% left_join(users, by="screen_name") %>% unique()

descripciones <- militantes$description.x

write.table(descripciones, "clipboard", row.names=F)
# Comunidades

author<-paste("@",sindicato_miedo$screen_name, sep="")
retweets<-sindicato_miedo$retweet_count
handles<-str_extract_all(sindicato_miedo$text,'@[A-Za-z]+[A-Za-z0-9_]+')
author.retweeted<- sapply(handles,function(x) x[1]) #Collect the first author's screenname
date<-sindicato_miedo$created_at
text<-sindicato_miedo$text


##Let's now create the network
data <- cbind(author,author.retweeted) #Collect dyads with Hub (screen-retw) and authority (handle-original)
net <- graph.empty() #open an empty network graph
net <- add.vertices(net, length(unique(c(data))),name=as.character(unique(c(data)))) #Load vertices
net <- add.edges(net, t(data)) #Add edges
E(net)$text <- text #add as an edge variable the texts of the tweets 
summary(net) 


##Create a layout for the network
l <- layout_with_fr(net, grid = c("nogrid"))
##Estimate community membership
my.com.fast <- walktrap.community(net)

#Plot your layout
plot(l, col=my.com.fast$membership, pch=16, cex=.5)

table(my.com.fast$membership)
V(net)$new.membership<-my.com.fast$membership


# Make a table of the number of tweets Most active Authorities in top Opposition subgroups1
d <- degree(graph=net, mode="in")
d <- as.data.frame(sort(d,decreasing = FALSE))
#d <- d[order(d$Freq, decreasing=T), ]
colnames(d) <- c("Tweets")
tail(d)

d <- degree(induced.subgraph(graph=net, vids=which(V(net)$new.membership==3)), mode="in")
d <- as.data.frame(sort(d,decreasing = FALSE))
#d <- d[order(d$Freq, decreasing=T), ]
colnames(d) <- c("Tweets")
head(d)



ind<-degree(net, mode="in") #arrows in
outd<-degree(net, mode="out") #arrows out
my.label<- names(ind) #names to the object

comm <- my.com.fast$membership #My community id's


## With labels

pdf(file = "sindicato_miedo-labels.pdf", 40, 40, pointsize=12, compress=FALSE)
plot.igraph(net, layout=l,vertex.label=my.label, vertex.size=(log(ind+1)), vertex.label.color="grey10", vertex.color=my.com.fast$membership, edge.width= .01, edge.arrow.size=.2,  edge.curved=TRUE, vertex.label.cex=.3)
dev.off()

png(file = "sindicato_miedo_low.png", 40, 40, units="cm",res=300)
plot.igraph(net, layout=l,vertex.label=my.label, vertex.size=(log(ind+1)), vertex.label.color="grey10", vertex.color=my.com.fast$membership, 
            edge.width= .01, edge.arrow.size=.2,  edge.curved=TRUE, vertex.label.cex=.3, mark.groups=list(which(comm==1), which(comm==4),which(comm==5),which(comm==6),which(comm==11), which(comm==30)), mark.col=terrain.colors(6, alpha = 0.2))
dev.off()

png(file = "sindicato_miedo.png", 40, 40, units="cm",res=500)
plot.igraph(net, layout=l,vertex.label=my.label, vertex.size=(log(ind+1)), vertex.label.color="grey10", vertex.color=my.com.fast$membership, 
            edge.width= .01, edge.arrow.size=.2,  edge.curved=TRUE, vertex.label.cex=.3, mark.groups=list(which(comm==1), which(comm==4),which(comm==5),which(comm==6),which(comm==11), which(comm==30)), mark.col=terrain.colors(6, alpha = 0.2))
dev.off()



