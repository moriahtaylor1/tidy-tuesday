library(tidyverse)
library(tidytuesdayR)
library(ggplot2)
library(ggtext)
library(showtext)
library(extrafont)
library(ragg)
library(ggthemes)
library(png)
library(grid)

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
                          hjust=1, size=55, color="white"),
  plot.caption=element_text(family="regular", size=22, color="#cccccc",
                            vjust=-4, hjust=0.5),
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
  axis.text.y = element_text(size=35, family="regular", color="white"),
  axis.text.x = element_blank(),
  #no legend
  legend.position = "none")

year_colors <- c("#B3B047", "#487497",) #lime green and steel blue

#load images
walleye_png <- readPNG("walleye-clipart.png")
whitebass_png <- readPNG("bass-clipart.png")
yellowperch_png <- readPNG("perch-clipart.png")
#prep images for plot
walleye_img <- rasterGrob(walleye_png, interpolate = TRUE)
whitebass_img <- rasterGrob(whitebass_png, interpolate = TRUE)
yellowperch_img <- rasterGrob(yellowperch_png, interpolate = TRUE)

(fish_plot <- fish %>% ggplot(aes(fill=as.factor(-year), y=grand_total, x=species)) +
                geom_bar(position="dodge", stat="identity") + 
                scale_fill_manual(values=year_colors) +
                geom_text(aes(label=grand_total, family="regular"), 
                         position=position_dodge(width=0.9), hjust=1.1, size=10, color="white") + 
                labs(
                  title="Fish Populations in Lake Erie in <i style='color:#B3B047'>2010</i> and <i style='color:#487497'>2015</i>",
                  caption="Source: Great Lakes Fishery Commission | Moriah Taylor | Twitter: moriah_taylor58 | GitHub: moriahtaylor1") +
                ylab("")+
                #add images
                annotation_custom(yellowperch_img, xmin=2, xmax=4, ymin=0, ymax=2000) +
                annotation_custom(whitebass_img, xmin=-Inf, xmax=Inf, ymin=0, ymax=2000) +
                annotation_custom(walleye_img, xmin=-1, xmax=3, ymin=0, ymax=2000) +
                #make horizontal
                coord_flip() + 
                #add theme
                my_theme)

ggsave("fish_plot.png",
       plot=fish_plot,
       device = agg_png(width = 8, height = 5, units = "in", res = 300))


