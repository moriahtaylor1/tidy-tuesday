#load packages
library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(showtext) #add font
library(ragg) #ggsave
library(patchwork) #put together multiple plots
library(ggstream) #stream plot
library(geofacet)  #facet_geom()

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week=30)
drought <- tuesdata$drought

#create year column
drought$valid_end <- as.Date(drought$valid_end, format="%Y-%m-%d")
drought$year <- format(drought$valid_end, "%Y")

#create more descriptive version of drought levels
drought <- drought %>% mutate(drought_lvl = factor(drought_lvl, levels = c("None", "D0", "D1", "D2", "D3", "D4")),
                              lvl = case_when(drought_lvl == "None" ~ "No Drought", 
                                              drought_lvl == "D0" ~ "Abnormally Dry",
                                              drought_lvl == "D1" ~ "Moderate Drought",
                                              drought_lvl == "D2" ~ "Severe Drought",
                                              drought_lvl == "D3" ~ "Extreme Drought",
                                              drought_lvl == "D4" ~ "Exceptional Drought"),
                              lvl = factor(lvl, levels = c("No Drought", "Abnormally Dry", "Moderate Drought", "Severe Drought", 
                                                           "Extreme Drought", "Exceptional Drought")))

#get area totals for each date
drought_area_totals <- drought %>% drop_na() %>% filter(drought_lvl != "None") %>% 
                            group_by(valid_end, lvl) %>% summarise(total_area = sum(area_total))
#convert to dataframe
drought_area_totals <- data.frame(drought_area_totals)

##plotting##
#colors
colors <- c("#f69e75", "#f05d42", "#de2d44", "#a81b5a", "#741f58")
#load font
font_add("title", "fonts/PatuaOne-Regular.ttf")
font_add("regular", "fonts/Nunito-Regular.ttf")
showtext_auto()
#plot theme
stream_theme <- theme(
  #titles
  plot.title=element_text(family="title", size=45, color="white", hjust=0.5, vjust=1),
  plot.caption=element_text(family="regular", size=30, color="#cccccc", hjust=0.5, vjust=1),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_text(size=45, family="regular", color="white"),
  axis.text.x = element_text(size=30, family="regular", color="white"),
  axis.text.y = element_blank(),
  #no legend
  legend.position = "top",
  legend.background = element_rect(fill="black"),
  legend.title = element_blank(),
  legend.text = element_text(size=20, family="regular", color="white"))

#stream plot based on total area in drought conditions in all of United States
total_area_stream <- drought_area_totals %>% ggplot(aes(valid_end, total_area, fill=lvl)) +
                          geom_stream(n_grid=nrow(drought_area_totals)) + 
                          scale_fill_manual(values=colors) +
                          labs(title="Drought Conditions in the United States",
                               caption="Moriah Taylor | #TidyTuesday | Twitter: @moriah_taylor58") +
                          xlab("") + ylab("Total Area") + stream_theme
#save plot
ggsave("drought_total_area.png",
       plot=total_area_stream,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))

