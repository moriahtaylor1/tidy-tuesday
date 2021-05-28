#load packages
library(tidytuesdayR) #tidy tuesday data
library(tidyverse) #wrangling
library(lubridate) #dates
library(extrafont) #fonts
library(showtext) #fonts
library(ragg)  #save as png
library(ggthemes) #custom plot theme
library(viridis)  #cb-friendly palettes
library(ggplot2) #plots
library(ggalt)

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 22)
drivers <- tuesdata$drivers
records <- tuesdata$records


###EDA###

#take a look at beginning of data
glimpse(drivers)
glimpse(records)


###TRANSFORMATION###

#divide into those who used shortcuts and those who didn't
records_short <- records %>% filter(shortcut=="Yes")
records_long <- records %>% filter(shortcut=="No")

#divide into single lap and full race
one_lap_short <- records_short %>% filter(type == "Single Lap")
full_race_short <- records_short %>% filter(type == "Three Lap")
one_lap_long <- records_long %>% filter(type == "Single Lap")
full_race_long <- records_long %>% filter(type == "Three Lap")

#create df for data of interest
df <- records_long %>% filter(type == "Three Lap")

#get track names
tracks <- unique(df$track)
#assign track names to cups
mushroom_cup <- tracks[1:4]
flower_cup <- tracks[5:8]
star_cup <- tracks[9:12]
special_cup <- tracks[13:16]

#create cup column
df <- df %>% mutate(cup = case_when(
                      track %in% mushroom_cup ~ "mushroom",
                      track %in% flower_cup ~ "flower",
                      track %in% star_cup ~ "star",
                      track %in% special_cup ~ "special"))



##LOOK AT TRACK TIMES BY CUP WITHOUT SHORTCUTS##
mushroom_df <- df %>% 
  filter(cup == "mushroom") %>%
  arrange(date) %>%
  group_by(track) %>%
  mutate(row_num = row_number())

flower_df <- df %>% 
  filter(cup == "flower") %>%
  arrange(date) %>%
  group_by(track) %>%
  mutate(row_num = row_number())

star_df <- df %>% 
  filter(cup == "star") %>%
  arrange(date) %>%
  group_by(track) %>%
  mutate(row_num = row_number())

special_df <- df %>% 
  filter(cup == "special") %>%
  arrange(date) %>%
  group_by(track) %>%
  mutate(row_num = row_number())

#mushroom cup facet plot
ggplot(data=mushroom_df, aes(x=row_num, y=time)) + geom_lollipop() +
  facet_wrap(~track, scales = "free")

#flower cup facet plot
ggplot(data=flower_df, aes(x=row_num, y=time)) + geom_lollipop() +
  facet_wrap(~track, scales = "free")

#star cup facet plot
ggplot(data=star_df, aes(x=row_num, y=time)) + geom_lollipop() +
  facet_wrap(~track, scales = "free")

#special cup facet plot
ggplot(data=special_df, aes(x=row_num, y=time)) + geom_lollipop() +
  facet_wrap(~track, scales = "free")



##LOOK AT TRACK TIMES BY CUP WITH SHORTCUTS##
df_short <- full_race_short %>% mutate(cup = case_when(
  track %in% mushroom_cup ~ "mushroom",
  track %in% flower_cup ~ "flower",
  track %in% star_cup ~ "star",
  track %in% special_cup ~ "special"))

mushroom_df2 <- df_short %>% 
  filter(cup == "mushroom") %>%
  arrange(date) %>%
  group_by(track) %>%
  mutate(row_num = row_number())

flower_df2 <- df_short %>% 
  filter(cup == "flower") %>%
  arrange(date) %>%
  group_by(track) %>%
  mutate(row_num = row_number())

star_df2 <- df_short %>% 
  filter(cup == "star") %>%
  arrange(date) %>%
  group_by(track) %>%
  mutate(row_num = row_number())

special_df2 <- df_short %>% 
  filter(cup == "special") %>%
  arrange(date) %>%
  group_by(track) %>%
  mutate(row_num = row_number())

#mushroom cup facet plot
ggplot(data=mushroom_df2, aes(x=row_num, y=time)) + geom_lollipop() +
  facet_wrap(~track, scales = "free")

#flower cup facet plot
ggplot(data=flower_df2, aes(x=row_num, y=time)) + geom_lollipop() +
  facet_wrap(~track, scales = "free")

#star cup facet plot
ggplot(data=star_df2, aes(x=row_num, y=time)) + geom_lollipop() +
  facet_wrap(~track, scales = "free")

#special cup facet plot
ggplot(data=special_df2, aes(x=row_num, y=time)) + geom_lollipop() +
  facet_wrap(~track, scales = "free")

##VISUALIZE FLOWER CUP W/ SHORTCUT##
#font
font_add(family = "regular", "Rubik-VariableFont_wght.ttf")
font_add(family = "title", "PressStart2P-Regular.ttf")
showtext_auto()

#my theme
plot_theme <- theme(
  # titles
  plot.title = element_text(family = "title", size = 55, color = "white", hjust=0.5, vjust=0.5),
  plot.subtitle = element_text(family = "regular", size = 40, color = "white", hjust = 0.5),
  plot.caption = element_text(family = "regular", size = 25, color = "#cccccc", hjust = 0.5),
  
  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  
  # axis
  axis.title.y = element_text(family="regular", size=35, color="white"),
  axis.text.y = element_text(family="regular", size=25),
  axis.text.x = element_blank(),
  axis.ticks.x.bottom = element_blank(),
  axis.title.x = element_blank(),
  
  #multi-plot text titles
  strip.text = element_text(family="regular", size=35),
  
  #no legend
  legend.position = "none"
)
#flower cup facet plot
flower_plot <- ggplot(data=flower_df2, aes(x=row_num, y=time, color=time)) + 
  geom_lollipop() + scale_color_viridis(option="turbo") +
  facet_wrap(~track, scales = "free") + 
  labs(title="FLOWER CUP",
       subtitle="SPEED RUN TIMES USING SHORTCUTS",
       caption = "Source: #Tidy Tuesday Week 22 | Moriah Taylor | Twitter: moriah_taylor58 | GitHub: moriahtaylor1") + 
  ylab("Time (seconds)") + plot_theme

flower_plot

#save plot
ggsave("flower_cup_plot.png",
       plot = flower_plot,
       device = agg_png(width = 7, height = 7, units = "in", res = 300))

##MODEL RELATIONSHIP BETWEEN DATE AND RACE TIMES##
#rainbow road subset
rainbow <- full_race_long %>% filter(track=="Rainbow Road")

#linear model for rainbow road
lin_model <- lm(time~date, data=rainbow)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lin_model)

#exponential model
exp_model <- lm(log(time)~date, data=rainbow)

#summary of models
summary(lin_model)
summary(exp_model)

#turn track into factor on full dataset
full_race_long$track_f <- factor(full_race_long$track) 

#linear and exponential models for all data
#where no shortcuts were used
full_lin_model <- lm(time~date+track_f, data=full_race_long)
full_exp_model <- lm(log(time)~date+track_f, data=full_race_long)

#summary of models
summary(full_lin_model)
summary(full_exp_model)