#load libraries
library(tidyverse)
library(showtext)
library(ragg)
library(geofacet)
#not in function
`%notin%` <- Negate(`%in%`)

#load data
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

#cleaning
airmen <- airmen %>% mutate(pilot_type = case_when(
  pilot_type=="Liaison pilot" | pilot_type=="Liason pilot" ~ "Liason Pilot",
  pilot_type=="Twin engine" ~ "Twin Engine",
  pilot_type=="Service pilot" ~ "Service Pilot",
  pilot_type=="Single engine" ~ "Single Engine"
))

airmen_clean <- airmen %>% drop_na(state) %>% filter(state %notin% c("Haiti", "VI", "Unk", "CN", "HT", "TD", "KN"))

airmen_clean$pilot_type <- factor(airmen_clean$pilot_type,
                                  levels = c("Service Pilot",
                                             "Liason Pilot",
                                             "Twin Engine",
                                             "Single Engine"))

airmen_clean <- airmen_clean %>% mutate(state = toupper(state))

#load font
font_add(family="bold", "AdventPro-Bold.ttf")
font_add(family="regular", "SignikaNegative-Light.ttf")
showtext_auto()

#create faceted plot
facet_plot <- airmen_clean %>% group_by(pilot_type, state) %>% summarise(n=n()) %>%
  ggplot(aes(x=pilot_type, y=n, color=pilot_type, fill=pilot_type)) +
  geom_bar(stat="identity") +
  coord_polar(theta="y") +
  ylim(0,75) +
  facet_geo(~state) +
  scale_fill_manual(values=c("#ffd700", "#00aa00", "#4682b4", "#dc143c")) +
  scale_color_manual(values=c("#ffd700", "#00aa00", "#4682b4", "#dc143c")) +
  labs(title = "TUSKEGEE AIRMEN",
       subtitle = "TYPES OF PILOTS BY STATE",
       caption = "MORIAH TAYLOR | @MORIAH_TAYLOR58 | #TIDYTUESDAY \n #TUSKEGEEAIRMENCHALLENGE | #DUBOISCHALLENGE2022") +
  theme(
    #titles
    plot.title = element_text(family="bold", hjust=0.5, size=60),
    plot.subtitle = element_text(family="bold", hjust=0.5, size=28),
    plot.caption = element_text(family="bold", hjust=0.5, size=28, color="#d2b48c"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #background
    panel.background = element_rect(fill="#f9efe6", color=NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="#f9efe6", color=NA),
    #axes
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    #strips
    strip.background = element_rect(fill="#f9efe6"),
    strip.text = element_text(family="bold", size=40),
    #legend
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(family="bold", size=20),
    legend.background = element_rect(fill="#f9efe6", color=NA),
    legend.box.background = element_rect(fill="#f9efe6", color=NA)
  )

#save faceted plot
ggsave("Pilot Types Spiral - Faceted.png",
       facet_plot,
       device = agg_png(width = 5, height = 6, units = "in", res = 300))

#make one big plot
plot <- airmen_clean %>% group_by(pilot_type) %>% summarise(n=n()) %>%
  ggplot(aes(x=pilot_type, y=n, color=pilot_type, fill=pilot_type)) +
  geom_bar(stat="identity") +
  coord_polar(theta="y") +
  geom_text(aes(label=n), size=15, color="black", nudge_y = 20, family="bold") +
  ylim(0,800) +
  scale_fill_manual(values=c("#ffd700", "#00aa00", "#4682b4", "#dc143c")) +
  scale_color_manual(values=c("#ffd700", "#00aa00", "#4682b4", "#dc143c")) +
  labs(title = "TUSKEGEE AIRMEN",
       subtitle = "TYPES OF PILOTS",
       caption = "MORIAH TAYLOR | @MORIAH_TAYLOR58 | #TIDYTUESDAY \n #TUSKEGEEAIRMENCHALLENGE | #DUBOISCHALLENGE2022") +
  theme(
    #titles
    plot.title = element_text(family="bold", hjust=0.5, size=60),
    plot.subtitle = element_text(family="bold", hjust=0.5, size=28),
    plot.caption = element_text(family="bold", hjust=0.5, size=28, color="#d2b48c"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #background
    panel.background = element_rect(fill="#f9efe6", color=NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="#f9efe6", color=NA),
    #axes
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    #strips
    strip.background = element_rect(fill="#f9efe6"),
    strip.text = element_text(family="bold", size=40),
    #legend
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(family="bold", size=20),
    legend.background = element_rect(fill="#f9efe6", color=NA),
    legend.box.background = element_rect(fill="#f9efe6", color=NA)
  )

ggsave("Pilot Types Spiral.png",
       plot,
       device = agg_png(width = 5, height = 6, units = "in", res = 300))