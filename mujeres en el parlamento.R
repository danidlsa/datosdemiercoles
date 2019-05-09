setwd("C:/Users/USUARIO/Dropbox/datos de miércoles")

datos_uip <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv")

#install.packages("ggpol")

library(ggplot2)
library(ggpol)
library(gganimate)

#reestructuración de los datos para usar paquete ggpol


datos_uip$mujeres <- round((datos_uip$numero_integrantes*datos_uip$porcentaje_mujeres)/100)
datos_uip$mujeres

datos_uip$hombres <- datos_uip$numero_integrantes - datos_uip$mujeres

datos_selec <- subset (datos_uip, numero_integrantes>0 & camara=="baja")

datos_selec

m <- aggregate (mujeres~pais, data=datos_selec, sum)
m$Sexo <- "mujeres"
h<- aggregate (hombres~pais, data=datos_selec, sum)
h$Sexo <- "hombres"
colnames(m) <- c("País", "Bancas", "Sexo")
colnames(h) <- c("País", "Bancas", "Sexo")
cuota <- aggregate (cuota_genero~pais, data=datos_selec, max)
colnames(cuota) <- c("País", "Ley de cuota de género")
p <- rbind.data.frame (m, h)
p$Color <- ifelse(p$Sexo=="mujeres", "purple", "yellow")

p <- p [order(p$País),] 
cuota <- cuota[order(cuota$País),]
p <- merge(p, cuota, by.y="País")
p<- p [order(p$País),]

#gráficos

plot_1  <- ggplot(p) + 
  geom_arcbar(aes(shares = Bancas,r0=5, r1=10, fill = Sexo)) + 
  scale_fill_manual(values = p$Color, labels = p$Sexo) +
  coord_fixed() + labs(caption = "@danidlsa para #datosdemiércoles") +
  theme_void() + 
  theme(strip.text.x=element_text(size=10)) +
  ggtitle ("Cantidad de bancas en Cámara Baja según sexo") +
  theme (plot.title = element_text(size=16, hjust = 0.5, face="bold"))
plot_1

plot_1 + facet_wrap(~País)

#separo según cuota de género

p_sincuota <- subset(p, `Ley de cuota de género`=="No")
p_concuota <- subset(p, `Ley de cuota de género`=="Sí")

plot_2 <- ggplot(p_sincuota) + 
  geom_arcbar(aes(shares = Bancas,r0=5, r1=10, fill = Sexo)) + 
  scale_fill_manual(values = p_sincuota$Color, labels = p_sincuota$Sexo) +
  coord_fixed() + labs(cap = "@danidlsa para #datosdemiércoles") +
  theme_void() + 
  theme(strip.text.x=element_text(size=10)) +
  ggtitle ("Países sin Ley de cuotas") +
  theme (plot.title = element_text(size=16, hjust = 0.5, face="bold")) +
  facet_wrap(~País) + theme(legend.position="right")
plot_2

plot_3 <- ggplot(p_concuota) + 
  geom_arcbar(aes(shares = Bancas,r0=5, r1=10, fill = Sexo)) + 
  scale_fill_manual(values = p_concuota$Color, labels = p_concuota$Sexo) +
  coord_fixed() + labs(cap = "@danidlsa para #datosdemiércoles") +
  theme_void() + 
  theme(strip.text.x=element_text(size=10)) +
  ggtitle ("Países con Ley de cuotas") +
  theme (plot.title = element_text(size=16, hjust = 0.5, face="bold")) +
  facet_wrap(~País) + theme(legend.position="")
plot_3



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

png("graph_parlamentos.png", width = 480, height = 640)
multiplot(plot_2, plot_3, cols=1)
dev.off()
