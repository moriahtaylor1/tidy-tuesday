#load packages
library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(showtext) #add font
library(ragg) #ggsave
library(ggtext) #element_markdown() 


tuesdata <- tidytuesdayR::tt_load(2021, week = 36)
birds <- tuesdata$bird_baths

#filter out total rows
birds_clean <- birds[!is.na(birds$survey_year),]

#get top 4 birds
top4birds <- birds %>% slice_max(bird_count, n=4) %>% pull(bird_type)

#filter observations to these 4 birds
top4df <- birds_clean %>% filter(bird_type %in% top4birds)

#create a season column
top4df <- top4df %>% mutate(season = case_when(survey_year==2014 ~ "Winter",
                                               survey_year==2015 ~ "Summer"))

#get counts of observations per season and per area type
counts <- top4df %>% group_by(bird_type, season, urban_rural) %>% 
                                  summarise(bird_sum = sum(bird_count))

#create variable with 4 different levels denoting each season and each area type
counts <- counts %>% mutate(area_season = case_when(
                              season=="Winter"&urban_rural=="Rural" ~ "Winter/Rural",
                              season=="Winter"&urban_rural=="Urban" ~ "Winter/Urban",
                              season=="Summer"&urban_rural=="Rural" ~ "Summer/Rural",
                              season=="Summer"&urban_rural=="Urban" ~ "Summer/Urban"
))

#basic plot
counts %>% ggplot(aes(x=urban_rural, y=bird_sum, fill=area_season)) +
                geom_bar(position="stack", stat="identity") +
                facet_wrap(~bird_type)

##plot styling##
font_add(family="regular", "")
showtext_auto()


#theme
my_theme <- theme(
  #titles
  plot.title=element_markdown(family="regular", hjust=0.5, size=60, color="black"),
  plot.title.position = "plot",
  plot.subtitle = element_textbox(family="regular", size=26, color="darkgrey")
  plot.caption=element_text(family="regular", size=35, color="white", hjust=0.5),
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text = element_text(size=35, family="regular", color="black"),
  axis.text.x = element_blank(),
  #no legend
  legend.position = "none")

#colors - light green, light purple, dark green, dark purple
fill_colors <- c("#8ca775", "#9075a7", "#406d19", "#46196d")

counts %>% ggplot(aes(x=urban_rural, y=bird_sum, fill=area_season)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=fill_colors) +
  facet_wrap(~bird_type)