library(tidyverse)
library(tidytuesdayR)
library(ggplot2)
library(ggtext)
library(showtext)
library(extrafont)
library(ragg)
library(ggthemes)

##LOAD DATA##
tuesdata <- tidytuesdayR::tt_load(2021, week = 24)
fishing <- tuesdata$fishing

##EDA AND TRANSFORMATION##
#filter to Lake Erie
erie <- fishing %>% filter(lake=="Erie")
#filter to Ohio
ohio <- erie %>% filter(region=="Ohio (OH)")
#look at unique species
unique(ohio$species)
#make list of fish of interest
fishies <- c("Walleye", "Yellow Perch", "White Bass")
#filter to fish of interest
fish3 <- ohio %>% filter(species %in% fishies)
#filter to the years 2010 and 2015
years <- c(2010, 2015)
fish <- fish3 %>% filter(year %in% years)

##VISUALIZATION##
font_add(family = "regular", "Merriweather-Regular.ttf")
showtext_auto()

my_theme <- theme(
  #titles
  plot.title=element_markdown(family="regular", vjust=0.5,
                          hjust=0, size=45, color="white"),
  plot.caption=element_text(family="regular", size=22, color="#cccccc",
                            vjust=-3, hjust=0.5),
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
  axis.title.x = element_text(size=40, family="regular", color="white"),
  axis.title.y = element_blank(),
  axis.text.y = element_text(size=30, family="regular", color="white", vjust=4),
  axis.text.x = element_blank(),
  #no legend
  legend.position = "none")

fish2010 <- fish %>% filter(year==2010)
fish2015 <- fish %>% filter(year==2015)

year_colors <- c("#487497", "#B3B047") #steel blue and lime green

(fish_plot <- fish %>% ggplot(aes(fill=as.factor(year), y=grand_total, x=species)) +
                geom_bar(position="dodge", stat="identity") + 
                scale_fill_manual(values=year_colors) +
                geom_text(aes(label=grand_total, family="regular"), 
                         position=position_dodge(width=0.9), hjust=1.1, size=8, color="white") + 
                labs(
                  title="Fish Populations in Lake Erie in <span style='color:#487497'>2010</span> and <span style='color:#B3B047'>2015</span>",
                  caption="Source: Great Lakes Fishery Commission | Moriah Taylor | Twitter: moriah_taylor58 | GitHub: moriahtaylor1") +
                ylab("Grand Total") +
                coord_flip() + 
                my_theme)

ggsave("fish_plot.png",
       plot=fish_plot,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))


