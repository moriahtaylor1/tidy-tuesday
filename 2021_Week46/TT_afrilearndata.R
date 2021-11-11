##load packages##
library(tidytuesdayR) #tidytuesday
library(tidyverse) #data wrangling
library(showtext) #custom font
library(ragg) #ggsave
library(afrilearndata)
library(sf)
library(viridis)
library(readxl)

#load font
font_add(family = "regular", "Oswald-Regular.ttf")
font_add(family = "script", "SeaweedScript-Regular.ttf")
showtext_auto()

map_theme <- theme(
  #titles
  plot.title = element_text(hjust=0.5, family="regular", size=25, color="#2C1B07"),
  plot.caption = element_text(hjust=0.5, family="regular", size=12, color="#2C1B07"),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  #background
  plot.background = element_rect(fill="#CDB48F", color=NA),
  panel.background = element_rect(fill="#CDB48F", color=NA),
  panel.grid.major = element_line(color="#856841"),
  panel.grid.minor = element_blank(),
  #axes
  axis.title = element_blank(),
  axis.text = element_text(color="#856841", family="script", size=15),
  axis.ticks = element_line(color="#856841"),
  #legend
  legend.text = element_text(color="#2C1B07", family="regular", size=12),
  legend.title = element_text(color="#2C1B07", family="regular", size=14),
  legend.background = element_rect(fill="#CDB48F"),
  legend.key = element_rect(fill="#CDB48F"),
)

gdp_plot <- africountries %>% ggplot() +
              geom_sf(aes(geometry=geometry, fill=gdp_md_est), color="#2C1B07") +
              scale_fill_viridis(option="mako", direction=-1, label=scales::comma) +
              labs(title="GDP of African Countries",
                   caption="Moriah Taylor | @moriah_taylor58 | {afrilearndata} package in R",
                   fill="GDP (millions)") +
              map_theme

ggsave("africa_gdp.png",
       plot = gdp_plot,
       device = agg_png(width = 3, height = 3, units = "in", res = 300))

#read in electricity access data
electricity <- read_excel("wdi_electricity.xlsx")
#merge electricity access data with given data
afri_electricity <- merge(africountries, electricity, by="iso_a3", all.x=TRUE)
#divide percentages by 100
afri_electricity <- afri_electricity %>% mutate(electricity_perc = electricity_access/100)

electricity_plot <- afri_electricity %>% ggplot() +
                      geom_sf(aes(geometry=geometry, fill=electricity_perc), color="#2C1B07") +
                      scale_fill_viridis(option="rocket", direction=-1, label=scales::percent) +
                      labs(title="Electricity Access (% of Population) [2019]",
                           caption="Moriah Taylor | @moriah_taylor58 | {afrilearndata} and World Bank DataBank",
                           fill="") +
                      map_theme

ggsave("africa_electricity.png",
       plot = electricity_plot,
       device = agg_png(width = 3, height = 3, units = "in", res = 300))


#read in internet usage data
internet <- read_excel("wdi_internet.xlsx")
#pivot internet dataframe
internet_pivot <- internet %>% pivot_longer(cols=!iso_a3,
                                            names_to="year",
                                            names_prefix="yr",
                                            values_to="internet_usage")
#subset africountries to necessary columns
africountries_subset <- africountries %>% select(iso_a3, geometry)
#merge internet dataframe and africountries subset
afrinternet <- merge(internet_pivot, africountries_subset, by="iso_a3")
#change text NA values to actual NA values
for (n in 1:nrow(afrinternet)){
  if (afrinternet[n,3]=="NA"){
    afrinternet[n,3] <- NA
  }
}
#turn internet usage into a percentage
afrinternet <- afrinternet %>% mutate(internet_usage_perc = as.numeric(internet_usage)/100)

internet_plot <- afrinternet %>% ggplot() +
                  geom_sf(aes(geometry=geometry, fill=internet_usage_perc), color="#2C1B07") +
                  facet_wrap(~year, ncol=1) +
                  scale_fill_viridis(option="rocket", direction=-1, label=scales::percent) +
                  labs(title="Internet Usage (% of Population)",
                       caption="Moriah Taylor | @moriah_taylor58 | {afrilearndata} and World Bank DataBank",
                       fill="") +
                  map_theme +
                  theme(plot.title = element_text(size=40),
                        plot.caption = element_text(size=16),
                        legend.text = element_text(size=25),
                        axis.text=element_blank(),
                        legend.position="top",
                        strip.text = element_text(family="script", color="#2C1B07", size=35),
                        strip.background = element_rect(fill=NA))
ggsave("africa_internet.png",
       plot = internet_plot,
       device = agg_png(width = 3, height = 10, units = "in", res = 300))
  