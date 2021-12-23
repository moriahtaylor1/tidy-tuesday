#load packages
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ragg)

#load data
tuesdata <- tidytuesdayR::tt_load('2021-12-14')
tracks <- tuesdata$studio_album_tracks

#get variables and tracks of interest
tracks_of_interest <- c("Wannabe", "Say You'll Be There", "2 Become 1", 
                        "Mama/Who Do You Think You Are", "Spice Up Your Life",
                        "Too Much", "Viva Forever", "Goodbye", "Holler/Let Love Lead The Way")

