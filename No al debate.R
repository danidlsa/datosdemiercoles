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

noaldebate <- search_tweets(q = "#NoAlDebate", n=18000)
noaldebate <- noaldebate[order(noaldebate$retweet_count),]

noaldebate$count <- 1

retuiteados <- noaldebate %>% filter(retweet_count>0)


noaldebate <- noaldebate %>% mutate(dia=as.Date(created_at))

resum <- aggregate(count~dia, noaldebate, sum)

g <- ggplot(resum, aes(x=dia, y=count)) +
  geom_line(stat="identity", color="#1da1f2", size=1) +
  labs(x="Fecha", y="N° de tuits", title="Tuits con el hashtag #NoAlDebate",
       caption="@danidlsa") +
  ggdark::dark_theme_bw() +
  theme(plot.title=element_text(color="#1da1f2"))

g

texto1= "Candidatos del FA a la IMM pactan\nno debatir con Laura Raffo"
texto2= "Primer tuit: \n5 de agosto"

g +annotate("text",  y = 2000, x= noaldebate$dia[15] ,
         label = texto1, size=3.5) +
  geom_vline(xintercept=noaldebate$dia[550], color="white") +
  geom_curve(aes(x=noaldebate$dia[50], xend=noaldebate$dia[550], 
                 y=1850, yend=1400), color="white", 
             arrow=arrow(length = unit(0.2, "cm")),
             curvature=.5) +
  annotate("text",  y = 800, x= noaldebate$dia[2] ,
            label = texto2, size=3.5) +
  geom_curve(aes(x=noaldebate$dia[2], xend=noaldebate$dia[1], 
                 y=600, yend=10), color="white", 
             arrow=arrow(length = unit(0.2, "cm")),
             curvature=.3)
ggsave("tuits en el tiempo.png", height=10, width=13, units="cm")  


# Comunidades

author<-paste("@",noaldebate$screen_name, sep="")
retweets<-noaldebate$retweet_count
handles<-str_extract_all(noaldebate$text,'@[A-Za-z]+[A-Za-z0-9_]+')
author.retweeted<- sapply(handles,function(x) x[1]) #Collect the first author's screenname
date<-noaldebate$created_at
text<-noaldebate$text


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

pdf(file = "noaldebate-labels.pdf", 40, 40, pointsize=12, compress=FALSE)
plot.igraph(net, layout=l,vertex.label=my.label, vertex.size=(log(ind+1)), vertex.label.color="grey10", vertex.color=my.com.fast$membership, edge.width= .01, edge.arrow.size=.2,  edge.curved=TRUE, vertex.label.cex=.3)
dev.off()

png(file = "noaldebate_low.png", 40, 40, units="cm",res=300)
plot.igraph(net, layout=l,vertex.label=my.label, vertex.size=(log(ind+1)), vertex.label.color="grey10", vertex.color=my.com.fast$membership, 
            edge.width= .01, edge.arrow.size=.2,  edge.curved=TRUE, vertex.label.cex=.3, mark.groups=list(which(comm==1), which(comm==4),which(comm==5),which(comm==6),which(comm==11), which(comm==30)), mark.col=terrain.colors(6, alpha = 0.2))
dev.off()

png(file = "noaldebate.png", 40, 40, units="cm",res=500)
plot.igraph(net, layout=l,vertex.label=my.label, vertex.size=(log(ind+1)), vertex.label.color="grey10", vertex.color=my.com.fast$membership, 
            edge.width= .01, edge.arrow.size=.2,  edge.curved=TRUE, vertex.label.cex=.3, mark.groups=list(which(comm==1), which(comm==4),which(comm==5),which(comm==6),which(comm==11), which(comm==30)), mark.col=terrain.colors(6, alpha = 0.2))
dev.off()


# Retuit más popular

retuiteados <- retuiteados %>% filter(retweet_status_id=="1293319273235058689") %>%
  mutate(account_created_at_fecha=as.Date(account_created_at))

ggplot(retuiteados, aes(x=account_created_at_fecha, y=followers_count)) +
  geom_point(color="black") +
  labs(title="Bot check para hashtag #NoAlDebate",
       caption="@danidlsa")

cero_followers <- retuiteados %>% filter(followers_count==0)

menos_20_followers <- retuiteados %>% filter(followers_count<20)

ggsave("bot_check.png", height=10, width=12, units="cm")
