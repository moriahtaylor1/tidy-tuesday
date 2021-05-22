#load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(extrafont)
library(showtext)
library(sysfonts)
library(ggthemes)
library(ragg)
library(maps)
library(sf)
library(mapview)
library(ggridges)
library(viridis)  #cb-friendly palettes

#load data
water <- read_csv("water.csv")

#remove some NA rows (have lots of data to work with so okay to remove many rows)
water <- water %>% drop_na(water_source) %>% drop_na(install_year) %>% drop_na(country_name) %>% filter(country_name != 'Peru')

#see which countries have the most data points
country_count <- water %>% group_by(country_name) %>% tally()

#subset to uganda
uganda <- water %>% filter(country_name=="Uganda")

#filter to desired years and group by year and get count of installs
uganda_installs <- uganda %>% filter(install_year>1989) %>% filter(install_year<2016) %>% 
                    group_by(install_year) %>% tally()

#load fonts
font_add(family = "regular", "Nunito-Regular.ttf")
font_add(family = "light", "Nunito-Light.ttf")
font_add(family = "bold", "Nunito-Bold.ttf")
showtext_auto()

#my plot theme #ef8650
my_theme <- theme(
  # titles
  plot.title = element_text(family = "bold", size = 50, color = "yellow", hjust=0.5, vjust=0.5),
  plot.subtitle = element_text(family = "regular", size = 40, color = "white", hjust=0.5),
  plot.caption = element_text(family = "light", size = 40, color = "#666666", hjust = 1),
  
  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  
  # axis
  axis.title = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(family="regular", size=40, color="white", hjust=7),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  
  #legend
  legend.position = "none"
)



plot_title <- expression(atop(x="2007 AND 2008 WERE PEAK YEARS FOR THE INSTALLATION",
                              y="OF WATER SOURCES IN UGANDA"))

strips <- ggplot(uganda_installs, aes(x=reorder(install_year, -install_year), y=1, fill=n)) + 
            geom_tile()  + 
            labs(x = "",
                 y = "",
                 title=plot_title, 
                 subtitle="\nNote: installations drop to less than 400 per year after 2014",
                 caption="\n#30DayChartChallenge Day 12 | Moriah Taylor | Source: #TidyTuesday | Twitter: moriah_taylor58") + 
                 #fill="Number of Installations") + 
            scale_fill_viridis_c(direction=-1, option="plasma") +
            scale_x_discrete(breaks=seq(1990,2015,5)) +
            geom_text(aes(label=n),hjust=0.5, vjust=0.15, size=10, alpha=0.8, color="white", family="bold") + 
            my_theme + coord_flip()

# save image
ggsave("strips_plot.png",
       plot = strips,
       device = agg_png(width = 8, height = 10, units = "in", res = 300))
