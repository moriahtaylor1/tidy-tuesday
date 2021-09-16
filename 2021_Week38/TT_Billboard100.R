#load packages
library(tidyverse) #data wrangling
library(tidytuesdayR) #tidytuesday data
library(ragg) #save ggplot
library(showtext) #custom font
library(ggtext) #element_markdown

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 38)
billboard <- tuesdata$billboard
audio <- tuesdata$audio_features



music <- merge(billboard, audio, by=c('song_id', 'song', 'performer'))
music_clean <- music[!is.na(music$mode),]
music_clean$week_id <- as.Date(music_clean$week_id, "%m/%d/%Y")
music_clean$year <- as.numeric(format(music_clean$week_id, "%Y"))

num1_key_byyear <- music_clean %>% filter(year<2021 & week_position==1) %>% 
                    group_by(year) %>% 
                    summarise(count_major = length(which(mode==1)),
                              count_minor = length(which(mode==0))) %>%
                    pivot_longer(c(count_major, count_minor))
                
##PLOT STYLING##
#load font
font_add(family="regular", "Nunito-Regular.ttf")
showtext_auto()

#theme
dark_theme <- theme(
  #titles
  plot.title=element_text(family="regular", hjust=0.5, size=70, color="white"),
  plot.title.position = "plot",
  plot.subtitle = element_markdown(family="regular", size=50, hjust=0.5, color="white"),
  plot.caption=element_text(family="regular", size=35, color="#333333", hjust=0.5),
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
  axis.text = element_text(size=28, family="regular", color="white"),
  #no legend
  legend.position = "none")
  
keys <- num1_key_byyear %>% ggplot(aes(x=year, y=value, color=name, group=name)) + 
                              geom_line(stat="identity", size=1) +
                              scale_color_manual(values=c("#B487E4", "#6AC710")) + 
                              scale_x_continuous(breaks=scales::pretty_breaks()) +
                              scale_y_continuous(breaks=scales::pretty_breaks()) +
                              coord_cartesian(expand=FALSE) + 
                              xlab("Year") + ylab("Number of Songs") +
                              labs(title="Number of #1 Billboard Hits",
                                   subtitle="Written in <i style='color:#B487E4'>Major</i> and <i style='color:#6AC710'>Minor</i> Keys") +
                              dark_theme

ggsave("keys.png",
       plot = keys,
       device = agg_png(width = 9, height = 6, units = "in", res = 300))

