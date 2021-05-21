#load packages
library(tidytuesdayR) #tidy tuesday data
library(tidyverse) #wrangling
library(extrafont) #fonts
library(showtext) #fonts
library(ragg)  #save as png
library(ggthemes) #custom plot theme
library(ghibli)   #studio ghibli palettes

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 21)
salaries <- tuesdata$survey

#subset to computing and tech industry
tech <- salaries %>% filter(industry=="Computing or Tech")

#change all titles to lowercase
tech$job_title <- tolower(tech$job_title)

#see most common titles
tibble <- tech %>% count(job_title, sort=TRUE)

#select jobs of interest
jobs_list <- c("software engineer", "software developer", "project manager", "data scientist", "data analyst", 
               "business analyst", "web developer")

tech_subset <- tech %>% filter(job_title %in% jobs_list)

#filter to jobs in the US (many variations of country name)
us <- c("US", "United States", "usa", "us", "Unite States", "United States", "United States of America",
        "united states", "U.S", "U.S.", "United states", "U.S.A.", "United States Of America", "Uniyed states",
        "Usa")
us_tech <- tech_subset %>% filter(country %in% us)

#create a plot theme
font_add(family = "regular", "Merriweather-Regular.ttf")    #load in font
showtext_auto()

my_theme <- theme(
  # titles
  plot.caption = element_text(family = "regular", size = 24, color = "#555555", hjust = 0),
  
  # panel and plot background
  panel.grid.major = element_line(color="#222222"),
  panel.grid.minor = element_line(color="#222222"),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  
  # axis
  axis.title.y = element_blank(),
  axis.title.x = element_text(family="regular", size=32, color="white"),
  axis.text.y = element_text(family="regular", size=40, color="white"),
  axis.text.x = element_text(family="regular", size=26, color="white", angle=40),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
)

# load in raincloud plot function
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

#create plot
(salaries_plot <-
    ggplot(data = us_tech,
           aes(x = job_title, y = annual_salary, fill = job_title)) +
    
    geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
    
    geom_point(aes(y = annual_salary, color = job_title),
               position = position_jitter(width = 0.15), size = 1, alpha = 0.5) +
    
    geom_boxplot(alpha = 0.7, color="#222222", width=0.2, outlier.shape = NA) +
    
    labs(y = "Annual Salary",
         x = NULL,
         caption = "Moriah Taylor | Source: #TidyTuesday | Twitter: moriah_taylor58 | GitHub: moriahtaylor1") +
    guides(fill = FALSE, color = FALSE) +
    scale_y_continuous(limits = c(50000, 250000), labels=scales::comma) +
    scale_fill_ghibli_d("PonyoMedium") +
    scale_colour_ghibli_d("PonyoMedium") +
    coord_flip() + my_theme)

# save image
ggsave("salaries.png",
       plot = salaries_plot,
       device = agg_png(width = 7, height = 7, units = "in", res = 300))
