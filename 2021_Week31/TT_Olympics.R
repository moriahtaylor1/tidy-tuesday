#load packages
library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(showtext) #add font
library(ragg) #ggsave

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
olympics <- tuesdata$olympics
regions <- tuesdata$regions