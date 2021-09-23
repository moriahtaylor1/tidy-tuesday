#load packages
library(tidyverse) #wrangling
library(tidytuesdayR) #tidytuesday
library(showtext) #add font
library(ragg) #ggsave
library(waffle) #geom_waffle()

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 39)
emmys <- tuesdata$nominees

#clean categories column
emmys$category_clean <- substr(emmys$category, 1, nchar(emmys$category)-7)
emmys$category_clean <- tools::toTitleCase(emmys$category_clean)
#get counts of categories
category_counts <- emmys %>% count(category_clean) %>% arrange(-n)

#create indicator for animation
emmys <- emmys %>% mutate(animated = case_when(
  str_detect(category_clean, "Animated") ~ 1,
  TRUE ~ 0
))
#filter to animated media
animated <- emmys %>% filter(animated==1)

#look at animated categories
animated_categories_counts <- animated %>% count(category_clean)

#get rid of hour-long specials
animated_shorts <- animated %>% filter(category_clean!="Outstanding Animated Program (for Programming One Hour or more)")

#look at title counts
animated_shorts_title_counts <- animated_shorts %>% count(title) %>% arrange(-n)

#take closer look at top 5 animations
top5_list <- c("The Simpsons", "Robot Chicken", "Bob's Burgers", "Adventure Time", "Steven Universe")
animated_top5 <- animated_shorts %>% filter(title %in% top5_list)

#group by award
top5_grouped <- animated_top5 %>% group_by(category, title) %>% summarise(year=last(year),
                                                                          distributor=last(distributor),
                                                                          type=last(type),
                                                                          category_clean=last(category_clean))
#load font
font_add(family="regular", "Spartan-Regular.ttf")
showtext_auto()
#plot theme
dark_waffle_theme <- theme(
  #titles
  plot.title=element_text(family="regular", hjust=0.5, size=40, color="white"),
  plot.title.position = "plot",
  plot.subtitle = element_text(family="regular", size=25, hjust=0.5, color="white"),
  plot.caption=element_text(family="regular", size=18, color="#333333", hjust=0.5),
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  #multi-plot text titles
  strip.text = element_text(family="regular", size=22, color="white"),
  strip.background = element_rect(fill="black"),
  #legend
  legend.position = "top",
  legend.background = element_rect(fill="black", color="black"),
  legend.box.background = element_rect(fill="black", color="black"),
  legend.text = element_text(family="regular", color="white", size=26),
  legend.title = element_blank(),
  legend.key = element_rect(fill="black"))

#waffle chart
waffle <- top5_grouped %>% count(title, type) %>%
            ggplot(aes(values=n, fill=type)) + 
            geom_waffle(flip = TRUE, n_rows=3, size=1) + 
            facet_wrap(~title, nrow=1, strip.position = "bottom") +
            coord_equal() +
            labs(title="Animated Series with the Most Emmy Nominations",
                 caption="Moriah Taylor | #TidyTuesday | @moriah_taylor58") +
            scale_fill_manual(values=c("#cccccc", "#C33F8F")) +
            dark_waffle_theme

ggsave("animated_series_waffle.png",
       plot = waffle,
       device = agg_png(width = 6, height = 3.5, units = "in", res = 300))




