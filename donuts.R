library(tidyverse)
library(readxl)
library(RColorBrewer)
library(extrafont)
windowsFonts(sans="Berlin Sans FB")
loadfonts(device="win")
loadfonts(device="postscript")

donut <- read_excel("donuts.xlsx")

donut$fraction = donut$n_consumer / sum(donut$n_consumer)
donut$ymax = cumsum(donut$fraction)
donut$ymin = c(0, head(donut$ymax, n=-1))

donut$labelPosition <- (donut$ymax + donut$ymin) / 2

donut$label <- paste0(donut$do_you_consume_donuts, "\nN°: ", donut$n_consumer)

ggplot(donut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=do_you_consume_donuts)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position="",
        panel.background = element_rect(fill="#eb3293"),
        plot.background  = element_rect(fill="#eb3293"),
        plot.margin = margin(30, 80, 30, 80),
        panel.spacing.y = unit(0, "pt"),
        plot.title=element_text(size=20, face="bold", color="#fbd00b", hjust=.5),
        plot.subtitle=element_text(size=16, face="italic", color="#fbd00b", hjust=.5),
        plot.caption=element_text(color="#fbd00b", size=14)) +
  geom_label (x=2, aes(y=labelPosition, label=label), size=5, color="#eb3293") +
  scale_fill_manual(values = c("#fbd00b", "#3B3002", "#AD9218")) +
  scale_color_manual(values=c("#fbd00b", "#3B3002", "#AD9218")) +
  labs(title="Consumo de donuts en Estados Unidos (2019)",
       subtitle="Número de consumidores en millones",
       caption="@danidlsa")

ggsave("donuts.png", height=14, width=20, units="cm")
                    