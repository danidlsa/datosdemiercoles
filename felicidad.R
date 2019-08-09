library(tidyverse)
library(cowplot)

felicidad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-07/felicidad.csv")


#ploteo de variables en scatterplots

felicidad_2018 <- felicidad %>% 
  filter(anio==2018)

grafico <- function(predictor) {
    ggplot(data=felicidad_2018,
           aes(x = predictor, 
               y = escalera_vida, 
               color = escalera_vida,
               label = pais)) +
    geom_point(size = 3) +
    labs (y = 'Felicidad') +
    theme(legend.position="",
          axis.title=element_text(size=11),
          axis.text=element_text(size=10)) +
    scale_color_continuous(low="darkblue", high="lightgreen") +
    stat_smooth(method="lm")
}
 
p1 <- grafico(felicidad_2018$log_pib) + xlab("PBI (log)")
p2 <- grafico(felicidad_2018$soporte_social) + xlab("Soporte social")
p3 <- grafico(felicidad_2018$libertad) + xlab("Libertad de tomar decisiones")
p4 <- grafico(felicidad_2018$percepcion_corrupcion) + xlab("Percepción de la corrupción")
p5 <- grafico(felicidad_2018$confianza) + xlab("Confianza interpersonal")
p6 <- grafico(felicidad_2018$expectativa_vida) + xlab("Expectativa de vida")

graficos <- plot_grid(p1, p2, p3, p4, p5, p6, nrow=3)




###REGRESIÓN LINEAL

lmfelicidad = lm(escalera_vida~log_pib+
                   soporte_social+
                   libertad+
                   percepcion_corrupcion+
                   confianza+
                   expectativa_vida, 
                 data = felicidad) 
summary(lmfelicidad)

print(summary(lmfelicidad))

reg <- summary(lmfelicidad)

#install.packages("jtools")
library(jtools)
library(huxtable)

reg.2 <- summ(lmfelicidad, digits=1, model.info = F)
t <- export_summs(reg.2)


# Ploteo gráficos + tabla resumen de regresión
tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
tbl <- tableGrob(t, rows=NULL, theme=tt)


library(grid)
library(gridExtra)
final <- grid.arrange(graficos, tbl,
             ncol=2,
             as.table=TRUE,
             top=textGrob("¿Qué explica la felicidad de las personas en el mundo?", 
                          gp=gpar(fontsize=16, fontface="bold")),
             bottom="De acuedo a datos del World Happines Report 2019, el 75% de la felicidad promedio en cada país podría explicarse por el efecto de 6 variables: 
             el PBI per cápita (+), el soporte social (contar con ayuda en caso de necesidad) (+), la libertad para tomar decisiones (+), 
             la percepción de la corrupción (-), la confianza interpersonal (viendo el gráfico, no parece afectar demasiado) y la expectativa de vida (+).
             El componente que más afecta a la felicidad es el soporte social.
             
             Visualización: @danidlsa")

ggsave(filename = "felicidad_cowplot.png", plot=final, height=21, width=30, units = "cm")
