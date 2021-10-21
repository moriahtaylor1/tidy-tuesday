#load packages
library(tidytuesdayR) #tidytuesday
library(tidyverse) #data wrangling
library(showtext) #custom font
library(ragg) #ggsave
library(geofacet) #facet wrap by state

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 43)
pumpkins <- tuesdata$pumpkins

#separate id column into year and type code
pumpkins_clean1 <- pumpkins %>% 
                      separate(col = id, 
                               into = c("year", "type_code"),
                               sep = "-")
#description of type_code as new column `type`
pumpkins_clean2 <- pumpkins_clean1 %>% mutate(type = case_when(
  type_code=="F" ~ "Field Pumpkin",
  type_code=="P" ~ "Giant Pumpkin",
  type_code=="S" ~ "Giant Squash",
  type_code=="W" ~ "Giant Watermelon",
  type_code=="L" ~ "Long Gourd",
  type_code=="T" ~ "Tomato"
)) %>%
  select(type, everything())

#changing character columns to be numeric
pumpkins_clean3 <- pumpkins_clean2 %>% 
  filter(!is.na(as.numeric(place))) %>%  # removes all non-numeric places
  mutate(place = parse_number(place),
         weight_lbs = parse_number(weight_lbs),
         ott = parse_number(ott),
         est_weight = parse_number(est_weight),
         pct_chart = parse_number(pct_chart)) 


#filter to "giant" crops with estimated weight stat
giant_crops <- pumpkins_clean3 %>% filter(type_code %in% c("P", "S", "W")) %>%
                filter(est_weight != 0)

# load in flat violin function
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

##---BUILDING A RAINCLOUD PLOT---##

#STEP 1: SPECIFY DATA AND AESTHETICS
giant_crops %>% ggplot(aes(x=type, y=est_weight, fill=type, color=type))
  
#STEP 2: ADD HALF-VIOLINS
giant_crops %>% ggplot(aes(x=type, y=est_weight, fill=type, color=type)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8)

#STEP 3: ADD POINTS
giant_crops %>% ggplot(aes(x=type, y=est_weight, fill=type, color=type)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
  geom_point(position = position_jitter(width = 0.15), size = 1, alpha = 0.5)

#STEP 4: COORD FLIP
giant_crops %>% ggplot(aes(x=type, y=est_weight, fill=type, color=type)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
  geom_point(position = position_jitter(width = 0.15), size = 1, alpha = 0.5) +
  coord_flip()

#STEP 5: IMPROVE APPEARANCE
#load font
font_add(family = "regular", "Merriweather-Regular.ttf")
showtext_auto()

#create theme
crops_theme <- theme(
  # titles
  plot.title = element_text(family="regular", size=35, color="#322607", hjust=0.5),
  plot.title.position = "plot",
  plot.subtitle = element_text(family="regular", size=24, color="#322607", hjust=0.5),
  plot.caption = element_text(family = "regular", size = 18, color = "#7d7766", hjust = 0.5),
  plot.caption.position = "plot",
  
  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#D1C6AA"),
  plot.background = element_rect(fill = "#D1C6AA"),
  plot.margin = margin(0.5, 1, 0.5, 0.5, unit="cm"),
  
  # axis
  axis.title = element_blank(),
  axis.text = element_text(family="regular", size=22, color="#322607"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  
  # no legend
  legend.position = "none"
)
final_plot <- giant_crops %>% ggplot(aes(x=type, y=est_weight, fill=type, color=type)) +
                geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
                geom_point(position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
                coord_flip(clip="off", expand=FALSE) +
                #add y limit so outliers don't ruin visual
                ylim(0,2500) +
                #change colors
                scale_fill_manual(values=c("#B25B0C", "#495529", "#085719")) +
                scale_color_manual(values=c("#B25B0C", "#495529", "#085719")) +
                #add labels
                labs(title="Oh My Gourd!",
                     subtitle="Estimated weights of giant crops up to 2500 pounds",
                     caption="\nMoriah Taylor | #TidyTuesday | @moriah_taylor58") +
                #add theme
                crops_theme
#FINAL STEP: SAVE PLOT#
ggsave("Crops_Raincloud.png",
       final_plot,
       device = agg_png(width = 5, height = 3.5, units = "in", res = 300))


##---FACET PLOT---##
#theme
facet_theme <- theme(
  #titles
  plot.title = element_text(family="regular", color="#085719", size=50, hjust=0.5),
  plot.title.position = "plot",
  plot.subtitle = element_text(family="regular", color="black", size=35, hjust=0.5),
  plot.caption = element_text(family="regular", color="black", size=24, hjust=0.5),
  
  #panel and plot
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "#cccccc"),
  
  #axes
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  
  #strip
  strip.text = element_text(family="regular", size=14, color="black"),
  strip.background = element_rect(fill="white", color="black"),
  
  #legend
  legend.position = "top",
  legend.background = element_rect(fill="#cccccc", color="#cccccc"),
  legend.box.background = element_rect(fill="#cccccc", color="#cccccc"),
  legend.text = element_text(family="regular", color="black", size=16),
  legend.title = element_text(family="regular", color="black", size=20, vjust=0.5),
  legend.key = element_rect(fill="#cccccc")
)

#get measurements of watermelons in the united states
us_watermelons <- pumpkins_clean3 %>% filter(type_code=="W") %>%
                      filter(country=="United States") 

#get medians for each state
medians <- us_watermelons %>% group_by(state_prov) %>%
              summarise(median=median(weight_lbs))

#merge medians with other measurements
us_watermelons_w_medians <- us_watermelons %>% left_join(medians)

#create plot
facet_melons <- us_watermelons_w_medians %>% ggplot(aes(x=weight_lbs, fill=median)) +
                  geom_density() +
                  scale_fill_gradient(low="#e6eee8", high="#085719") +
                  coord_cartesian(expand=FALSE)+
                  facet_geo(~state_prov) +
                  labs(title = "State Your Melons!",
                       subtitle = "Weights of competition watermelons by state",
                       caption = "\nMoriah Taylor | #TidyTuesday | @moriah_taylor58",
                       fill = "Median Weight (lbs)") +
                  facet_theme

ggsave("Watermelon_Weights_Facet.png",
       facet_melons,
       device = agg_png(width = 9, height = 6, units = "in", res = 300))
