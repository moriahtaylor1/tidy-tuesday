#load packages
library(tidyverse)
library(sf)
library(layer)
library(ragg)
#notin function
`%notin%` <- Negate(`%in%`)

#load shape file
shape <- read_sf("Alternative_Fueling_Stations.shp")

#filter to public stations
public_shapes <- shape %>% filter(ACCESS_COD=="public")

#filter to continental United States
cont_shapes <- public_shapes %>% filter(STATE %notin% c("AK", "HI", "PR"))

#create maps
electric <- st_as_sf(cont_shapes %>% filter(FUEL_TYPE_ == "ELEC"))
propane <- st_as_sf(cont_shapes %>% filter(FUEL_TYPE_ == "LPG"))
biodiesel <- st_as_sf(cont_shapes %>% filter(FUEL_TYPE_ == "BD"))
ethanol <- st_as_sf(cont_shapes %>% filter(FUEL_TYPE_ == "E85"))

#create tilted maps
tilt_electric <- tilt_map(electric)
tilt_ethanol <- tilt_map(ethanol, y_shift=50)
tilt_propane <- tilt_map(propane, y_shift=100)
tilt_biodiesel <- tilt_map(biodiesel, y_shift=150)

#put maps in list
maps_list <- list(tilt_electric, tilt_ethanol, tilt_propane, tilt_biodiesel)

#plot tilted maps
plot <- plot_tiltedmaps(maps_list,
                layer = c("value", "value", "value", "value"),
                palette = c("magma", "magma", "magma", "magma"))

#save plot
ggsave("layered map - plain.png",
       plot,
       device = agg_png(width = 8, height = 10, units = "in", res = 300))