#load packages
library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(showtext) #add font
library(ragg) #ggsave

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 32)
athletes <- tuesdata$athletes

#count number of unique countries
length(unique(athletes$abb))
#count total medals per country
country_counts <- athletes %>% count(abb)

#filter to top countries
top_countries <- c("USA", "GBR", "CHN", "FRA", "AUS", "CAN")
filtered_athletes <- athletes %>% filter(abb %in% top_countries)

#count total medals per sport
sports_counts <- filtered_athletes %>% count(type)

#count medals per country per year
country_counts_peryear <- filtered_athletes %>% group_by(abb, year) %>% summarise(medal_count=n())


#load font
font_add(family = "regular", "Rubik.ttf")
showtext_auto()

#create theme
my_theme <- theme(
  #titles
  plot.title=element_text(family="regular", vjust=1, hjust=0.5, size=60, color="white"),
  plot.subtitle=element_text(family="regular", vjust=1, hjust=0.5, size=45, color="white"),
  plot.caption=element_text(family="regular", size=40, color="#cccccc", vjust=-4, hjust=0.5),
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black", color="#cccccc"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_text(size=40, family="regular", color="white"),
  axis.text = element_text(size=35, family="regular", color="white"),
  #no legend
  legend.position = "none",
  #facet wrap titles
  strip.background = element_rect(fill="black", color="#cccccc"),
  strip.text.x = element_text(size=40, family="regular", color="white"))



bar_multi <- country_counts_peryear %>% ggplot(aes(x=year, y=medal_count)) + 
  geom_bar(stat="identity") + facet_wrap(~abb) + my_theme +
  labs(title="Paralympic Medals from Top 6 Overall Countries",
       caption="Moriah Taylor | #TidyTuesday 2021 Week 32 | @moriah_taylor58") +
  xlab("Year") + ylab("Number of Paralympic Medals")
  

ggsave("paralympics_bar_multi.png",
       plot=bar_multi,
       device = agg_png(width = 8, height = 8, units = "in", res = 300))

##do the same chart but stacked with medal type##
#count medals per country per year
country_counts_bymedal <- filtered_athletes %>% group_by(abb, year, medal) %>% summarise(medal_count=n())

medals <- c("Gold", "Silver", "Bronze")
medal_colors <- c("#be9625", "#6f6f6f", "#8C5216")

country_counts_bymedal$medal <- fct_relevel(country_counts_bymedal$medal, "Gold", "Silver", "Bronze")

stacked_multi <- country_counts_bymedal %>%
  ggplot(aes(x=year, y=medal_count)) + 
  geom_col(aes(fill=medal), position=position_stack()) + facet_wrap(~abb) + my_theme +
  scale_fill_manual(values=medal_colors) +
  labs(title="Paralympic Medals from Top 6 Overall Countries",
       subtitle="All of the countries below have over 1000 medals in the Paralympics",
       caption="Moriah Taylor | #TidyTuesday 2021 Week 32 | @moriah_taylor58") +
  xlab("") + ylab("Number of Paralympic Medals")


ggsave("paralympics_stacked_multi.png",
       plot=stacked_multi,
       device = agg_png(width = 8, height = 8, units = "in", res = 300))


