#load packages
library(tidytuesdayR) #tidy tuesday data
library(tidyverse) #wrangling
library(extrafont) #fonts
library(showtext) #fonts
library(ragg)  #save as png
library(ggthemes) #custom plot theme

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 21)
