library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(showtext) #add font
library(ragg) #ggsave
library(ggthemes) #plot themes
library(formattable)
library(kableExtra)

##WEEK 26 DATA: PARKS##
tuesdata <- tidytuesdayR::tt_load(2021, week = 26)
parks <- tuesdata$parks

##CODE SOURCED FROM BjnNowak ON GITHUB; BjnNowak ON TWITTER##
my_color_tile <- function() {
  return_col <- function(y) 
    #create color scale for tiles
    map_chr(y,function(x) case_when(x > 90  ~ "#0967c6",
                                    x > 80  ~ "#5a80cf",
                                    x > 70  ~ "#859bd8",
                                    x > 60  ~ "#abb6e1",
                                    x >= 50  ~ "#ced3e9",
                                    x >= 40  ~ "#f1f1f1",
                                    x >= 30  ~ "#e9c6c4",
                                    x >= 20  ~ "#dd9d99",
                                    x >= 10  ~ "#ce7370",
                                    x >= 0  ~ "#bb4749"
    ))
  formatter("span", 
            style = function(y) style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              #create conditional for font color (white vs. black font)
              "color" = ifelse( return_col(y) %in% c("#0967c6", "#bb4749"),
                                csscolor("white"), csscolor("black")),
              "background-color" = return_col(y)
            )
  )
}

my_area_tile <- function() {
  return_col <- function(y) 
    #create color scale for tiles
    map_chr(y,function(x) case_when(x > 40  ~ "#0967c6",
                                    x > 30  ~ "#859bd8",
                                    x >= 20  ~ "#f1f1f1",
                                    x >= 10  ~ "#dd9d99",
                                    x >= 0  ~ "#bb4749"
    ))
  formatter("span", 
            style = function(y) style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              #create conditional for font color (white vs. black font)
              "color" = ifelse( return_col(y) %in% c("#0967c6", "#bb4749"),
                                csscolor("white"), csscolor("black")),
              "background-color" = return_col(y)
            )
  )
}

#vector of Ohio cities in dataset
ohio <- c("Cincinnati", "Cleveland", "Columbus", "Toledo")

#ohio chart
ohio_parks<-parks %>% 
  filter(year=='2020') %>%
  filter(city %in% ohio) %>%
  arrange(rank)%>%
  mutate(
    RANK=cell_spec(rank,"html", color="#dddddd", align="center",bold=F),
    CITY=cell_spec(city,"html", color="#dddddd", align="center",bold=F),
    Area=my_area_tile()(park_pct_city_points),
    Accessibility=my_color_tile()(pct_near_park_points),
    Spending=my_color_tile()(spend_per_resident_points),
    Basketball=my_color_tile()(basketball_points),
    DogPark=my_color_tile()(dogpark_points),
    Playground=my_color_tile()(playground_points),
    RecCenter=my_color_tile()(rec_sr_points),
    Restroom=my_color_tile()(restroom_points),
    Splashpad=my_color_tile()(splashground_points)
  )%>%
  select(RANK,CITY,Area,Accessibility,Spending,Basketball,Playground,Restroom)%>%
  kable(
    "html", escape = F,align=c("clcccccc"),
  ) %>%
  kable_material_dark(html_font = "Cambria") %>%
  row_spec(0, color = "white") %>%
  add_header_above(c("OHIO CITIES"=2, "Park Scores Per Category" = 6), color="white", font_size=20)%>%
  column_spec(2,width_min='4cm')%>%
  column_spec(3:8,width_min='3cm')%>%
  footnote(general = "| Source: #TidyTuesday | Twitter: @moriah_taylor58 | Inspired by: @BjnNowak",general_title = "",escape=F)
ohio_parks



