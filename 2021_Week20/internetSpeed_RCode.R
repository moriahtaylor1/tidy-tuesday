library(tidytuesdayR) #tidy tuesday data
library(tidyverse) #wrangling
library(lubridate) #wrangling
library(extrafont) #fonts
library(showtext) #fonts
library(ragg)  #save as png
library(rmarkdown) #markdown
library(maps)  #geocoding
library(ggmap)  #geocoding
library(ggthemes) #custom plot theme
library(viridis)  #colorblind-friendly palette
library(sf)  #geocoding
library(tidycensus)  #geocoding

tuesdata <- tidytuesdayR::tt_load(2021, week = 20)

speeds <- tuesdata$broadband
zip_codes <- tuesdata$broadband_zip

#rename columns
names(speeds) <- c("state", "county_id", "subregion", "broadband_fcc", "broadband_usage")
zip_codes <- zip_codes %>% rename(state = ST,
                                  subregion = "COUNTY NAME",
                                  county_id = "COUNTY ID",
                                  zipcode = "POSTAL CODE",
                                  broadband_usage = "BROADBAND USAGE")

#tidy county names into common format
speeds$subregion <- word(speeds$subregion, 1)
speeds$subregion <- tolower(speeds$subregion)
zip_codes$subregion <- word(zip_codes$subregion, 1)
zip_codes$subregion <- tolower(zip_codes$subregion)

#changing usage column to numeric
speeds$broadband_usage <- as.numeric(speeds$broadband_usage)
#creating percentage column
speeds <- speeds %>%
  mutate(broadband_perc = broadband_usage * 100)

#subset data for states of interest
nc <- speeds %>%
  filter(state == "NC")

ohio <- speeds %>%
  filter(state == "OH")

#getting map data for each state that is ggplot-friendly
nc_county <- map_data('county', 'north carolina')
ohio_county <- map_data('county', 'ohio')

nc_state <- map_data('state', 'north carolina')
ohio_state <- map_data('state', 'ohio')

#joining data on common counties
nc_data <- merge(nc_county, nc, on="subregion")
ohio_data <- merge(ohio_county, ohio, on="subregion")

font_add(family = "regular", "Merriweather-Regular.ttf")
showtext_auto()


#themes
my_theme <- theme(
  # title, subtitle, caption
  plot.title = element_text(family="regular", size=55, color="white", vjust=0.5, hjust=0.5),
  plot.subtitle = element_blank(),
  plot.caption = element_text(family="regular", size=25, color="white", hjust=1),
  
  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  
  # axis
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  
  #no legend
  legend.position = "top",
  legend.title = element_text(family="regular", size=25, color="white", vjust=0.8),
  legend.background = element_rect(fill="black"),
  legend.text = element_text(family="regular", size=18, color="white", vjust=0.8)
  
)

nc_plot <- ggplot(nc_data, #county outline and filled
                   aes(x=long, y = lat, fill=broadband_perc, group=as.factor(subregion), color="white")) +
  
  #state outline
  geom_polygon(color="white") +
  
  
  labs(title = "Broadband Access in North Carolina",
       fill = "% of Usage at Broadband Speed",
       caption = "Moriah Taylor | Source: #TidyTuesday | Twitter: moriah_taylor58 | GitHub: moriahtaylor1") +
  
  scale_fill_viridis() + my_theme


oh_plot <- ggplot(ohio_data, #county outline and filled
                  aes(x=long, y = lat, fill=broadband_perc, group=as.factor(subregion), color="white")) +
  
  #state outline
  geom_polygon(color="white") +
  
  
  labs(title = "Broadband Access in Ohio",
       fill = "% of Usage at Broadband Speed",
       caption = "Moriah Taylor | Source: #TidyTuesday | Twitter: moriah_taylor58 | GitHub: moriahtaylor1") +
  
  scale_fill_viridis() + my_theme

# save image
ggsave("oh_broadband.png",
       plot = oh_plot,
       device = agg_png(width = 6, height = 7, units = "in", res = 300))


# save image
ggsave("nc_broadband.png",
       plot = nc_plot,
       device = agg_png(width = 10, height = 5, units = "in", res = 300))
