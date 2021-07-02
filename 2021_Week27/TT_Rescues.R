#load packages
library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(showtext) #add font
library(ragg) #ggsave
library(ggthemes) #plot themes
library(patchwork) #put together multiple plots
library(cowplot) #put together multiple plots
#load font
font_add(family="regular", "SignikaNegative-Regular.ttf")
showtext_auto()

##WEEK 27 DATA: ANIMAL RESCUES##
tuesdata <- tidytuesdayR::tt_load(2021, week = 27)
animal_rescues <- tuesdata$animal_rescues

#some cleaning
#remove year 2021 since it is not complete yet
animal_rescues <- animal_rescues %>% filter(cal_year != 2021)
#reduce to most common types of animals
common_animals <- c("Bird", "Cat", "Deer", "Dog", "Fox", "Horse", "Squirrel")
commonRescues <- animal_rescues %>% filter(animal_group_parent %in% common_animals)
#remove rows with NULL values for pump_hours_total
cleanRescues <- commonRescues %>% filter(pump_hours_total != "NULL")
#change column types
cleanRescues$pump_hours_total <- as.integer(cleanRescues$pump_hours_total)
cleanRescues$incident_notional_cost <- as.integer(cleanRescues$incident_notional_cost)
cleanRescues$animal_group_parent <- as.factor(cleanRescues$animal_group_parent)

#DIRECTION 1: SIDE-BY-SIDE BAR PLOT
bothyears <- cleanRescues %>% filter(cal_year %in% c(2019,2020))
bothcounts <- bothyears %>% group_by(cal_year) %>% count(animal_group_parent)
bothcounts <- as.data.frame(bothcounts)
bothcounts$cal_year <- as.factor(bothcounts$cal_year)
(year_counts <- bothcounts %>% ggplot(aes(fill=cal_year, y=n, x=animal_group_parent)) +
    geom_bar(position="dodge", stat="identity") + 
    scale_fill_manual(values=c("#181899", "#DD1554")) +
    geom_text(aes(label=n, family="regular"), 
              position=position_dodge(width=1), vjust=-.5, size=8, color="black") +
    labs(title="Number of Animal Rescues") +
    theme(
      plot.title=element_text(family="regular", vjust=0.5,
                              hjust=0.5, size=55, color="black"),
      #background
      panel.border=element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
      #axes
      axis.ticks.length=unit(0.15, "cm"),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(size=55, family="regular", color="black"),
      axis.text.y = element_blank(),
      #no legend
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size=60, family="regular", color="black")))


#DIRECTION 2: JITTER PLOT
(animal_jitter <- ggplot(data=bothyears, aes(x=as.factor(animal_group_parent), 
                              y=as.integer(pump_hours_total))) +
  geom_point(position="jitter", aes(color=animal_group_parent)) + 
  labs(title="")+
  ylab("Rescue Length (Hours)") +
  coord_flip() +
  scale_color_viridis_d(option="plasma") +
  theme(
    plot.title=element_text(family="regular", vjust=0.5,
                                hjust=1, size=12, color="white"),
    #background
    panel.border=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "darkgrey"),
    plot.background = element_rect(fill = "darkgrey"),
    plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
    #axes
    axis.ticks.length=unit(0.15, "cm"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_text(size=10, family="regular", color="white"),
    axis.title.y = element_blank(),
    axis.text = element_text(size=10, family="regular", color="white"),
    #no legend
    legend.position = "none"))

#DIRECTION 3: LINEAR REGRESSION
summary(lm(pump_hours_total ~ as.factor(animal_group_parent), data=cleanRescues))
#DIRECTION 4: TIME SERIES ANALYSIS

#DIRECTION 5: CAT PLOT COMPARING YEARS
library(interactions)
#add cal_year_f as a column to bothyears
bothyears$cal_year_f <- as.factor(bothyears$cal_year)
#create model
fit <- lm(pump_hours_total ~ cal_year_f * animal_group_parent, data=bothyears)
#create a cat plot
animals_pred <- cat_plot(fit, pred=animal_group_parent, modx=cal_year_f,
         x.label="", y.label="",
         main.title = "Animal Species as a Predictor \nof Rescue Length (Hours)", legend.main="Year",
         colors=c("#181899", "#DD1554"))

#year_counts
#animal_jitter
#animals_pred

ggdraw() +
  draw_plot(year_counts, x=0, y=0.39, width=1, height=.4) +
  draw_plot(animal_jitter, x=0.05, y=0.02, width=0.45, height=0.35)+
  draw_plot(animals_pred, x=0.5, y=0.02, width=0.45, height=0.35)

ggsave("year_counts.png",
       plot=year_counts,
       device = agg_png(width = 9, height = 5, units = "in", res = 300))

