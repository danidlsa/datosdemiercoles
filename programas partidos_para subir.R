library(tidyverse)
library(stringr)
library(tm)

##Uso readLines para leer los programas ya pasados a texto plano por @rivaquiroga

fa <- readLines("https://raw.githubusercontent.com/rivaquiroga/programas-presidenciales-2019/master/uruguay_frente-amplio_martinez_2019.txt")
#fa
Encoding(fa) <- "UTF-8"
head(fa)
pc <- readLines("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-09-04/uruguay_partido-colorado_talvi_2019.txt")
Encoding(pc) <- "UTF-8"
pn <- readLines("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-09-04/uruguay_partido-nacional_lacalle-pou_2019.txt")
Encoding(pn) <- "UTF-8"

#Preprocesamiento

quitarparentesis <- function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))
preprocesamiento <- function(x) removeWords(removePunctuation(removeNumbers(tolower(x))),stopwords("spanish"))

fa <- as.data.frame(fa)
pn <- as.data.frame(pn)
pc <- as.data.frame(pc)

fa$fa <- preprocesamiento(quitarparentesis(fa$fa))
pn$pn<- preprocesamiento(quitarparentesis(pn$pn))
pc$pc <- preprocesamiento(quitarparentesis(pc$pc))

rm("preprocesamiento")
rm("quitarparentesis")

fa$fa  <- str_remove_all(fa$fa, "¡")
fa$fa  <- str_remove_all(fa$fa, "¿")

pn$pn  <- str_remove_all(pn$pn, "¡")
pn$pn  <- str_remove_all(pn$pn, "¿")

pc$pc  <- str_remove_all(pc$pc, "¡")
pc$pc  <- str_remove_all(pc$pc, "¿")


fa$fa <- iconv(fa$fa,from="UTF-8",to="ASCII//TRANSLIT")
pn$pn <- iconv(pn$pn,from="UTF-8",to="ASCII//TRANSLIT")
pc$pc <- iconv(pc$pc,from="UTF-8",to="ASCII//TRANSLIT")

fa <- fa %>% mutate(fa=ifelse(grepl("seguridad social", fa)==TRUE, "seguridadsocial",
                                     ifelse(grepl("ordenamiento territorial", fa)==T, "ordenamientoterritorial",
                                            ifelse(grepl("relaciones laborales", fa)==T, "relacioneslaborales", 
                                                   ifelse(grepl("inclusión financiera", fa)==T, "inclusionfinanciera", 
                                                          ifelse(grepl("bienestar animal", fa)==T, "bienestaranimal", 
                                                                 ifelse(grepl("fuerzas armadas", fa)==T, "fuerzasarmadas", fa)))))))
pc <- pc %>% mutate(pc=ifelse(grepl("seguridad social", pc)==TRUE, "seguridadsocial",
                                     ifelse(grepl("ordenamiento territorial", pc)==T, "ordenamientoterritorial",
                                            ifelse(grepl("relaciones laborales", pc)==T, "relacioneslaborales", 
                                                   ifelse(grepl("inclusión financiera", pc)==T, "inclusionfinanciera", 
                                                          ifelse(grepl("bienestar animal", pc)==T, "bienestaranimal", 
                                                                 ifelse(grepl("fuerzas armadas", pc)==T, "fuerzasarmadas", pc)))))))

pn <- pn %>% mutate(pn=ifelse(grepl("seguridad social", pn)==TRUE, "seguridadsocial",
                                     ifelse(grepl("ordenamiento territorial", pn)==T, "ordenamientoterritorial",
                                            ifelse(grepl("relaciones laborales", pn)==T, "relacioneslaborales", 
                                                   ifelse(grepl("inclusión financiera", pn)==T, "inclusionfinanciera", 
                                                          ifelse(grepl("bienestar animal", pn)==T, "bienestaranimal", 
                                                                 ifelse(grepl("fuerzas armadas", pn)==T, "fuerzasarmadas", pn)))))))

#Matrices de conteo de palabras

matriz_fa <- DocumentTermMatrix(Corpus(VectorSource(t(fa$fa))))
matriz_fa<-as.matrix(matriz_fa)
matriz_fa<-as.data.frame(matriz_fa)
matriz_fa<-matriz_fa[,which(apply(matriz_fa,2,sum) > 5)]

fa <- data.frame(fa,matriz_fa)
rm("matriz_fa")

matriz_pn <- DocumentTermMatrix(Corpus(VectorSource(t(pn$pn))))
matriz_pn<-as.matrix(matriz_pn)
matriz_pn<-as.data.frame(matriz_pn)
matriz_pn<-matriz_pn[,which(apply(matriz_pn,2,sum) > 5)]

pn <- data.frame(pn,matriz_pn)
rm("matriz_pn")

matriz_pc <- DocumentTermMatrix(Corpus(VectorSource(t(pc$pc))))
matriz_pc<-as.matrix(matriz_pc)
matriz_pc<-as.data.frame(matriz_pc)
matriz_pc<-matriz_pc[,which(apply(matriz_pc,2,sum) > 5)]

pc <- data.frame(pc,matriz_pc)
rm("matriz_pc")


fa_count <- reshape2::melt(fa[,c(2:1785)])
fa_count$variable <- as.character(fa_count$variable)
fa_count <- aggregate(value ~ variable, fa_count, sum)

pn_count <- reshape2::melt(pn[,c(2:1613)])
pn_count$variable <- as.character(pn_count$variable)
pn_count <- aggregate(value ~ variable, pn_count, sum)

pc_count <- reshape2::melt(pc[,c(2:1641)])
pc_count$variable <- as.character(pc_count$variable)
pc_count <- aggregate(value ~ variable, pc_count, sum)

rm(list=c("fa", "pn", "pc"))

##Análisis: palabras más mencionadas en los programas

fa_count <- fa_count %>% mutate(variable=ifelse(variable=="losas", NA, 
                                                ifelse(variable=="asi", NA, variable))) %>%
  drop_na()



g1 <- function(x) {
  x[order(-x$value),] %>% 
  head(20) %>% 
  ggplot(aes(x=reorder(variable, value), y=value)) +
  geom_bar(stat="identity") +
  coord_flip()
}

pn_count <- pn_count %>% mutate(variable=ifelse(variable=="anos", "años", 
                                                ifelse(variable=="ser", NA, variable))) %>%
  drop_na()


pc_count <- pc_count %>% mutate(variable=ifelse(variable=="ser", NA, 
                                                ifelse(variable=="asi", NA, variable))) %>%
  drop_na()

g1(fa_count)
g1(pn_count)
g1 (pc_count)

#Análisis de ISSUES

issues<- function(x) {
  x %>% filter(variable=="economia" |
                 variable=="desarrollo" |
                 variable=="seguridad" |
                 variable=="crimen" |
                 variable=="policia" |
                 variable=="delito" |
                 variable=="seguridadsocial" |
                 variable=="prevision" |
                 variable=="prevision social" |
                 variable=="educacion" |
                 variable=="trabajo" |
                 variable=="trabajos" |
                 variable=="empleo" |
                 variable=="empleos" |
                 variable=="vivienda" |
                 variable=="viviendas" |
                 variable=="habitat" |
                 variable=="cuidados" |
                 variable=="drogas" |
                 variable=="adicciones" |
                 variable=="cultura" |
                 variable=="deporte" |
                 variable=="integracion" |
                 variable=="pobreza" |
                 variable=="emigracion"|
                 variable=="migrantes" |
                 variable=="relacioneslaborales" |
                 variable=="salud" |
                 variable=="participacion" |
                 variable=="democracia" |
                 variable=="defensa" |
                 variable=="ejercito" |
                 variable=="comercio" |
                 variable=="tecnología"|
                 variable=="ciencia" |
                 variable=="innovacion" |
                 variable=="agro"|
                 variable=="agroindustria" |
                 variable=="agropecuario" |
                 variable=="rural" |
                 variable=="agrotoxicos" |
                 variable=="industria" |
                 variable=="energia" |
                 variable=="ambiente" |
                 variable=="productivo" |
                 variable=="productividad" |
                 variable=="ordenamientoterritorial" |
                 variable=="diversidad" |
                 variable=="gestion" |
                 variable=="eficiencia" |
                 variable=="transparencia" |
                 variable=="impuestos" |
                 variable=="fiscalidad" |
                 variable=="bienestaranimal" |
                 variable=="mides" |
                 variable=="genero" |
                 variable=="discapacidad" |
                 variable=="militar" |
                 variable=="administracion" |
                 variable=="fuerzasarmadas" |
                 variable=="ffaa" |
                 variable=="inseguridad")
}

fa_issues <- issues(fa_count)
pn_issues <- issues(pn_count)
pc_issues <- issues(pc_count)

rm("issues")

issues_2 <- function(x) {
  x %>% mutate(issue=ifelse(variable=="impuestos" | variable=="fiscalidad", "fiscalidad",
                            ifelse(variable=="transparencia" | variable=="eficiencia" | variable=="gestion" | variable=="administracion", "Reforma del Estado",
                                   ifelse(variable=="agro" | variable=="agropecuario" | variable=="rural" | variable=="agrotoxicos", "Agro",
                                          ifelse(variable=="ordenamientoterritorial" | variable=="ambiente", "Ambiente y ordenamiento territorial",
                                                 ifelse(variable=="tecnología" | variable=="ciencia" | variable=="innovacion", "Tecnología, Ciencia e Innovación",
                                                        ifelse(variable=="participacion" | variable=="democracia", "Participación y democracia",
                                                               ifelse(variable=="integracion" | variable=="pobreza" | variable=="mides", "Integración, pobreza y desarrollo social",
                                                                      ifelse(variable=="trabajo" | variable=="trabajos" | variable=="empleo" | variable=="empleos", "Trabajo",
                                                                             ifelse(variable=="seguridadsocial" | variable=="prevision" | variable=="previsional", "Seguridad social",
                                                                                    ifelse(variable=="industria" | variable=="energia" | variable=="productivo" | variable=="productividad", "Industria y desarrollo productivo",
                                                                                           ifelse(variable=="ejercito" | variable=="defensa" | variable=="militar" | variable=="ffaa" | variable=="fuerzasarmadas", "Defensa",
                                                                                                  ifelse(variable=="crimen" | variable=="policia" | variable=="delito" | variable=="seguridad" | variable=="inseguridad", "Seguridad ciudadana",
                                                                                                         ifelse(variable=="drogas" | variable=="adicciones", "Drogas",
                                                                                                                ifelse(variable=="bienestaranimal", "Bienestar animal", 
                                                                                                                       ifelse(variable=="comercio", "Comercio",
                                                                                                                              ifelse(variable=="vivienda" | variable=="viviendas" | variable=="habitat", "Vivienda",
                                                                                                                                     ifelse(variable=="cuidados", "Cuidados",
                                                                                                                                            ifelse(variable=="cultura" | variable=="deporte", "Cultura y deporte",
                                                                                                                                                   ifelse(variable=="diversidad", "Diversidad",
                                                                                                                                                          ifelse(variable=="desarrollo" | variable=="economia", "Economía y desarrollo",
                                                                                                                                                                 ifelse(variable=="discapacidad", "Discapacidad",
                                                                                                                                                                        ifelse(variable=="educacion", "Educación",
                                                                                                                                                                               ifelse(variable=="genero", "Género",
                                                                                                                                                                                      ifelse(variable=="migrantes", "Migrantes",
                                                                                                                                                                                             ifelse(variable=="salud", "Salud", "Relaciones laborales"))))))))))))))))))))))))))
}

fa_issues <- issues_2(fa_issues)
pc_issues <- issues_2(pc_issues)
pn_issues <- issues_2(pn_issues)

fa_issues <- aggregate(value~issue, fa_issues, sum)
pc_issues <- aggregate(value~issue, pc_issues, sum)
pn_issues <- aggregate(value~issue, pn_issues, sum)

colnames(fa_issues)[2] <- "fa"
colnames(pc_issues)[2] <- "pc"
colnames(pn_issues)[2] <- "pn"

issues_partidos <- merge(fa_issues, pn_issues, by=c("issue"), all.x=TRUE)
issues_partidos <- merge(issues_partidos, pc_issues, by=c("issue"), all.x=T)

#rm(list=setdiff(ls(), c("issues_partidos", "fa_count", "pc_count", "pn_count")))

issues_partidos <- issues_partidos %>% gather(fa,pc,pn, key="partido", value="value")
issues_partidos <- issues_partidos %>% mutate(issue=ifelse(issue=="fiscalidad", "Fiscalidad", issue)) %>%
  mutate(partido_ok=ifelse(partido=="fa", "FA",
                        ifelse(partido=="pn", "PN", "PC")))
  

##Visualizaciones issues

library(vtable)
library(magick)

vtable(issues_partidos)
#install.packages("OpenImageR")
library(grid)
library(OpenImageR)
lacalle <- readImage("lacalle.jpg")
talvi <- readImage("talvi.jpg")
martinez <- readImage("martinez.jpg")
imagenes <- list(martinez,talvi,lacalle)

library(ggthemes)
library(extrafont)

g <- ggplot(issues_partidos, aes(x=factor(partido_ok), y=issue, size=value,
                                 color=partido,
                                 label = value,
                                 text = paste('\nCantidad de menciones de palabras relacionadas:', 
                                              '\n',issue, ':', value))) +
  geom_point(alpha=.7) +
  scale_color_manual(values=c("fa"="darkblue", "pn"="lightblue", "pc"="darkred"),
                     guide=F)+
  scale_size(range=c(1,18), name="Cantidad de menciones") +
  labs(x="Partido",
       y="",
       caption="Visualización: @danidlsa",
       title="Issues más mencionados en las bases programáticas",
       subtitle="Elecciones 2019 - Uruguay") +
  theme_calc() +
  theme(text = element_text(family = "Segoe UI"),
        plot.title = element_text(color='black',
                                  size=18, face="bold"),
        strip.text = element_text(size = 9),
        legend.title=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14, face="bold"),
        axis.title.x=element_text(size=15, face="bold"),
        plot.subtitle=element_text(size=15),
        legend.position="",
        plot.caption=element_text(size=12, face="bold"))


library(cowplot)

pimage <- axis_canvas(g, axis = 'x') + 
  draw_image(martinez, x = .5, scale =1) +
  draw_image(talvi, x = 1.5, scale = 1) +
  draw_image(lacalle, x = 2.5, scale = 1)


g2 <- ggdraw(insert_xaxis_grob(g, pimage, position = "bottom"))


ggsave("issues.png", g2, height=30, width=28, units="cm")

library(plotly)
library(htmlwidgets)

gg <- ggplotly((g + ggtitle("Issues más mencionados en las bases programáticas") + 
                              labs(x="Partidos | Elecciones UY 2019")), 
               tooltip = "text",
               height = 1000,
               width=1000)  %>% 
  layout(showlegend=F)

saveWidget(gg, 'issues_partidos.html')


