library(tidyverse)
library(magick)
library(ggimage)
library(ggrepel)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

img = "https://i.redd.it/ua66qc428wo41.jpg"

maximos <- office_ratings %>% group_by(season) %>% 
  filter(imdb_rating==max(imdb_rating))

minimos <- office_ratings %>% group_by(season) %>% 
  filter(imdb_rating==min(imdb_rating))

p <- ggplot(office_ratings, aes(x=season, y=imdb_rating, group=season)) +
  geom_violin(trim=F, fill="#008583") +
  scale_x_continuous(breaks=seq(1, 9, by=1)) +
  theme_classic() +
  labs(x="Temporada", y="Rating IMDB",
       title="The Office: Ratings de IMBD por temporada",
       caption="Visualización: @danidlsa") +
  theme(plot.title=element_text(size=14, face="bold", color="black"),
        axis.title=element_text(color="black", face="bold", size=12),
        axis.text=element_text(color="black", size=12)) +
  geom_label_repel(data=maximos, aes(label=title), nudge_y=.5, size=3) +
  geom_label_repel(data=minimos, aes(label=title), nudge_y=-.5, color="darkred", size=3)
p  

ggbackground(p, img)

ggsave("the office ratings.png", height=16, width=24, units="cm")
