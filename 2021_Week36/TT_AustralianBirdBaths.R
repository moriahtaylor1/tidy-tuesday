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
font_add(family="regular", "BalsamiqSans-Regular.ttf")
showtext_auto()


#theme
my_theme <- theme(
  #titles
  plot.title=element_markdown(family="regular", hjust=0.5, size=45, color="black"),
  plot.title.position = "plot",
  plot.subtitle = element_text(family="regular", size=30, hjust=0.5, color="#333333"),
  plot.caption=element_text(family="regular", size=35, color="black", hjust=0.5),
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#cccccc"),
  plot.background = element_rect(fill = "#cccccc"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_text(size=30, family="regular", color="black"),
  axis.text.x = element_blank(),
  #multi-plot text titles
  strip.text = element_text(family="regular", size=35, color="black"),
  strip.background = element_rect(fill="white", color="black"),
  #no legend
  legend.position = "none")

#colors - light green, light purple, dark green, dark purple
fill_colors <- c("#8ca775", "#9075a7", "#406d19", "#46196d")

#plot
counts_plot <- counts %>% ggplot(aes(x=urban_rural, y=bird_sum, fill=area_season)) +
          geom_bar(position="stack", stat="identity", color="black") +
          scale_fill_manual(values=fill_colors) +
          coord_cartesian(expand=FALSE) +
          ylim(0,275) +
          facet_wrap(~bird_type) + 
          labs(title="Bird Bath Spottings in <span style='color:#406d19'>Rural</span> and <span style='color:#46196d'>Urban</span> Areas of Australia",
               subtitle="These stacked bar charts depict the four most populous birds in the dataset.\n**Lighter bars indicate summer sightings and darker bars indicate winter sightings.",
               caption="\nMoriah Taylor | @moriah_taylor58 | #TidyTuesday") +
          my_theme

#save plot
ggsave("spottings.png",
       plot=counts_plot,
       device = agg_png(width = 6, height = 6, units = "in", res = 300))