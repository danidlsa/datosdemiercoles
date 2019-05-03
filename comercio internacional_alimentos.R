#Exportaciones de Alimentos en Sudamérica

#install.packages("waffle")
library (ggplot2)
library(waffle)

#datos - reduzco a países de sudamérica y a la exportación de alimentos

comercio <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")

alimentos <- subset (comercio, nombre_comunidad_producto=="Alimentos")

sudamerica <- subset (alimentos,  
                        nombre_pais_origen=="Argentina" |
                        nombre_pais_origen=="Bolivia" |
                        nombre_pais_origen=="Chile" |
                        nombre_pais_origen=="Colombia" |
                        nombre_pais_origen=="Paraguay" |
                        nombre_pais_origen=="Perú" |
                        nombre_pais_origen=="Uruguay" |
                        nombre_pais_origen=="Venezuela"
                      )

v <- aggregate (valor_exportado_dolares~anio+nombre_pais_origen, data=sudamerica, sum)
v

# para utilizar el paquete waffle versión 0.7.0 (la disponible en CRAN), se debe convertir la información que está en formato data.frame a un vector
#hago un vector por año

#2016

v2016 <- subset (v, anio==2016)

lista_16 <- v2016$valor_exportado_dolares
names(lista_16) <- v2016$nombre_pais_origen
lista_16

#gráfico waffle

wa16 <- waffle(lista_16/100000000, rows=8, size=1.2, title="2016", 
             xlab="Cada unidad equivale a 100 millones de dólares exportados")
wa16 <- wa16 + theme(legend.position="none")
wa16

#2015

v2015 <- subset (v, anio==2015)
lista_15 <- v2015$valor_exportado_dolares
names(lista_15) <- v2015$nombre_pais_origen
lista_15

wa15 <- waffle(lista_15/100000000, rows=8, size=1.2, title="2015", 
               xlab="Cada unidad equivale a 100 millones de dólares exportados")
wa15 <- wa15 + theme(legend.position="none")
wa15

#2014

v2014 <- subset (v, anio==2014)
lista_14 <- v2014$valor_exportado_dolares
names(lista_14) <- v2014$nombre_pais_origen
lista_14

wa14 <- waffle(lista_14/100000000, rows=8, size=1.2, title="2014", 
               xlab="Cada unidad equivale a 100 millones de dólares exportados")
wa14 <- wa14 + theme(legend.position="none")
wa14

#2013
v2013 <- subset (v, anio==2013)
lista_13 <- v2013$valor_exportado_dolares
names(lista_13) <- v2013$nombre_pais_origen
lista_13

wa13 <- waffle(lista_13/100000000, rows=8, size=1.2, title="2013", 
               xlab="Cada unidad equivale a 100 millones de dólares exportados")
wa13 <- wa13 + theme(legend.position="none")
wa13

#este último waffle es para que aparezcan las referencias en la visualización y no sea necesario que aparezcan para cada gráfico, ni que cambien el tamaño de ninguno
#alerta: solución casera!

ref <- waffle (lista_13/100000000, rows=1)
ref <- ref + theme(legend.position="top")
ref

##dado que esta versión de waffle no permite utilizar data.frames, tenemos un gráfico para cada año. 
#utilizo la siguiente función para colocarlos a los 4 en la misma imagen:

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


#save

png("graph.png", width = 1024, height = 768)
multiplot(wa13, wa14, wa15, wa16, ref, cols=1)
dev.off()
