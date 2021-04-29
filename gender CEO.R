library(tidyverse)
library(ggplot2)
#devtools::install_github("hrbrmstr/waffle")
library(waffle)
library(cowplot)
library(extrafont)
library(stringr)
library(magrittr)
library(stringi)

#data
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

# I downloaded a names dataset in https://data.world/arunbabu/gender-by-names  
names_df <- read.csv("gender_refine-csv.csv")

names_df <- names_df %>% 
  filter(gender!=3) %>%
  mutate(gender=ifelse(gender==0, "F", "M"))

names_df$score=NULL

## Separating first names of directors and filtering out movies

departures <- departures %>% 
  mutate(separate_strings= strsplit(exec_fullname, "\\, |\\,| "))


departures$ceo_first_name <- NA

for (i in 1:nrow(departures)) {
  departures$ceo_first_name[i] <- departures$separate_strings[[i]] %>% extract2(1)
}


names_df <- names_df %>% mutate(ceo_first_name=name) %>% select(-name)

departures <- departures %>% left_join(names_df, by="ceo_first_name") 

departures2 <- departures %>% filter(is.na(gender)==FALSE)


# wrangling
ceo_waffle <- departures %>% 
  mutate(
    reason = case_when(
      departure_code == 1 ~ "death",
      departure_code == 2 ~ "illness",
      departure_code == 3 ~ "dismissed for job performance",
      departure_code == 4 ~ "dismissed for legal violations or concerns",
      departure_code == 5 ~ "retired",
      departure_code == 6 ~ "new opportunity",
      departure_code == 7 ~ "other",
      departure_code == 8 ~ "missing",
      departure_code == 9 ~ "execucomp error",))%>% 
  drop_na(reason) %>% 
  filter(reason != "execucomp error") %>% 
  filter(reason != "other") %>% 
  filter(reason!= "missing") %>% 
  filter(fyear>2017) %>%
  filter(is.na(gender)==F)

table(ceo_waffle$gender)
table(departures$gender)

ceo_waffle <- ceo_waffle %>% group_by(gender, reason) %>%
  count()

ceo_waffle$reason <- factor(ceo_waffle$reason, levels = c("retired", 
                                                          "dismissed for job performance", 
                                                          "dismissed for legal violations or concerns",
                                                          "illness",
                                                          "new opportunity",
                                                          "death"))

ceo_waffle <- ceo_waffle %>% mutate(gender=ifelse(gender=="F", "Women", "Men"))
#geom_waffle
waffle <- ggplot(ceo_waffle, aes(fill = reason, values = n)) +
  geom_waffle(n_rows = 11, size = 0.03, colour = "black") +
  scale_fill_manual(name = NULL,
                    values = c("retired" = "#ffc93c", 
                               "dismissed for job performance" = "#ffee93",
                               "dismissed for legal violations or concerns" = "grey",
                               "illness" = "#1b2021",
                               "new opportunity" = "#31326f",
                               "death" = "white")) +
  coord_equal() +
  facet_wrap(~gender) +
  labs(title = "Reasons CEOs leave the company", subtitle = "counting from 2017 untill 2020",
       caption="@danidlsa") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7, color = "#8E8E8E", family="Bahnschrift"),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.key.size = unit(0.7, "line"),
    legend.key = element_rect(size = 0.1, color = NA),
    legend.background = element_rect(color = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.5, family="Bahnschrift"),
    plot.subtitle = element_text(hjust = 0.5, family="Bahnschrift")
  ) 

waffle

ggsave("gender CEO.png", height=12, width=16, units="cm")
