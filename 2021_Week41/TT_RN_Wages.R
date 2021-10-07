#load packages
library(tidyverse) #wrangling
library(tidytuesdayR) #tidytuesday
library(showtext) #add font
library(ragg) #ggsave
library(biscale) #bivariate choropleth
library(maps) #polygon data
library(sf) #sf
library(cowplot) #ggdraw

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 41)
nurses <- tuesdata$nurses

#filter to most recent year
nurses20 <- nurses %>% filter(Year==2020)

#change column names
names(nurses20)[8] <- "median_salary"
names(nurses20)[18] <- "location_quotient"

#begin process of creating a bivariate map

#step 1: get polygons
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

#step 2: merge polygon data with our data
nurses20$State <- tolower(nurses20$State) #change state names to lowercase for merge
nurses20_sub <- nurses20 %>% select(State, median_salary, location_quotient) #select only the columns we need
names(states)[1] <- "State" #change id column to be named 'State' for merge
nurses_polygons <- merge(states, nurses20, by="State") #merge data on State

#step 3: create bivariate classes using our data
our_classes <- bi_class(nurses_polygons, x=median_salary, y=location_quotient, style="quantile", dim=3)

#step 4a: create theme for map
#load font
font_add(family="regular", "Roboto-Regular.ttf")
showtext_auto()
#create map
map_theme <- theme(
  #titles
  plot.title=element_text(family="regular", hjust=0, size=40, color="black"),
  plot.title.position = "plot",
  plot.subtitle = element_text(family="regular", size=40, hjust=0, color="black"),
  plot.caption=element_text(family="regular", size=18, color="black", hjust=1),
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.spacing = ggplot2::unit(c(-0.1, 0.2, 0.2, 0.2), "cm"),
  plot.background = element_rect(fill = "white"),
  plot.margin=ggplot2::unit(c(0.5, 0.5, 0.2, 0.5), "cm"),
  #axes
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  #legend
  legend.position = "top",
  legend.background = element_rect(fill="white", color="white"),
  legend.box.background = element_rect(fill="white", color="white"),
  legend.text = element_text(family="regular", color="black", size=26),
  legend.title = element_blank(),
  legend.key = element_rect(fill="white"))

#step 4b: create map
our_map <- ggplot() + 
            geom_sf(data=our_classes, mapping=aes(fill=bi_class), color="white", size=0.1, show.legend=FALSE) +
            bi_scale_fill(pal="DkBlue", dim=3) + 
            labs(title="Bivariate Choropleth Map of",
                 subtitle = "Median RN Salary and Location Quotient*",
                 caption="*Location Quotient quantifies how concentrated registered nurses are compared to the national average.")+
            map_theme

#step 5: create legend
our_legend <- bi_legend(pal="DkBlue",
                        dim=3,
                        xlab="Median Salary",
                        ylab="Location Quotient",
                        size=15)

#step 6: draw map with legend
map_with_legend <- ggdraw() + 
                      draw_plot(our_map, 0, 0, 1, 1) +
                      draw_plot(our_legend, 0.65, 0.72, 0.2, 0.2, scale=1)

#final step: save map
ggsave("nurses_bivariate.png",
       plot = map_with_legend,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))
