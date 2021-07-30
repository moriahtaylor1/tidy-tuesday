#load packages
library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(showtext) #add font
library(ragg) #ggsave
library(kableExtra) #kable tables
library(formattable) #formatter() function

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
olympics <- tuesdata$olympics
regions <- tuesdata$regions

##DATA CLEANING AND WRANGLING##
#reduce to medal winners
winners <- olympics[!is.na(olympics$medal), ]
#filter to recent years
winners_recent <- winners %>% filter(year>=2000)
#split into two olympics types: winter and summer
winners_winter <- winners_recent %>% filter(season=="Winter")
winners_summer <- winners_recent %>% filter(season=="Summer")

####find top 10 countries with medals per event for each olympics type####
##controlling for team sports where multiple medals are awarded
##WINTER##
medal_counts_winter <- winners_winter %>% 
                            group_by(noc, year, event, medal) %>%
                            summarise(n_medals = n())
#get number of events where team/participant won a medal
counts_per_event_winter <- medal_counts_winter %>% group_by(noc) %>% 
                              summarise(n_medal_events = n())
#get top 10
top_10_winter <- counts_per_event_winter %>% slice_max(n_medal_events, n=10)
#get list of country codes
top_countries_winter <- top_10_winter$noc
#filter dataset to these 10 countries
winter10 <- winners_winter %>% filter(noc %in% top_countries_winter)
##SUMMER##
medal_counts_summer <- winners_summer %>% 
  group_by(noc, year, event, medal) %>%
  summarise(n_medals = n())
#get number of events where team/participant won a medal
counts_per_event_summer <- medal_counts_summer %>% group_by(noc) %>% 
  summarise(n_medal_events = n())
#get top 10
top_10_summer <- counts_per_event_summer %>% slice_max(n_medal_events, n=10)
#get list of country codes
top_countries_summer <- top_10_summer$noc
#filter dataset to these 10 countries (tie for 10th place)
summer10 <- winners_summer %>% filter(noc %in% top_countries_summer)

####turn data into table structure####
##WINTER##
#get counts per medal
winter10_counts <- winter10 %>% group_by(noc) %>% count(medal)
#pivot
winter_pivot <- winter10_counts %>% pivot_wider(names_from=medal, values_from=n)
#change colname of regions dataset to be lowercase
names(regions) <- c("noc", "region", "notes")
#merge winter data with region information
winter_merge <- merge(winter_pivot, regions, by="noc")
#subset to necessary information
winter_table <- winter_merge[, c(5,3,4,2)]
#add total column
winter_table$Total <- winter_table$Gold + winter_table$Silver + winter_table$Bronze
#sort by total
winter_table <- winter_table %>% arrange(-Total)
##SUMMER##
summer10_counts <- summer10 %>% group_by(noc) %>% count(medal)
#pivot
summer_pivot <- summer10_counts %>% pivot_wider(names_from=medal, values_from=n)
#change colname of regions dataset to be lowercase
names(regions) <- c("noc", "region", "notes")
#merge summer data with region information
summer_merge <- merge(summer_pivot, regions, by="noc")
#subset to necessary information
summer_table <- summer_merge[, c(5,3,4,2)]
#add total column
summer_table$Total <- summer_table$Gold + summer_table$Silver + summer_table$Bronze
#sort by total
summer_table <- summer_table %>% arrange(-Total)

####create functions for each color tile####
gold_tile <- function() {
  formatter("span", 
            style = style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              "color" = csscolor("black"),
              "background-color" = "#be9625"
            )
  )
}

silver_tile <- function() {
    formatter("span", 
            style = style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              "color" = csscolor("black"),
              "background-color" = "#9F9F9F"
            )
  )
}

bronze_tile <- function() {
  formatter("span", 
            style = style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              "color" = csscolor("black"),
              "background-color" = "#9D836A"
            )
  )
}

total_tile <- function() {
  formatter("span", 
            style = function(y) style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              "color" = csscolor("black"),
              "background-color" = "#B5C8ED"
            )
  )
}

####create kables####
##WINTER##
winter_kable <- winter_table %>%
                mutate(COUNTRY=cell_spec(region,"html", color="black", align="left",bold=F),
                        BRONZE=bronze_tile()(Bronze),
                        SILVER=silver_tile()(Silver),
                        GOLD=gold_tile()(Gold),
                        TOTAL=total_tile()(Total))%>%
                select(COUNTRY, GOLD, SILVER, BRONZE, TOTAL)%>%
                kable(
                  "html", escape = F,align=c("lcccc"),
                ) %>%
                kable_minimal(html_font = "Cambria") %>%
                row_spec(0, color = "black") %>%
                add_header_above(c("Winter Olympic Medal Events" = 5), color="black", font_size=20)%>%
                column_spec(2:5,width_min='3cm')
winter_kable

##SUMMER##
summer_kable <- summer_table %>%
  mutate(COUNTRY=cell_spec(region,"html", color="black", align="left",bold=F),
         BRONZE=bronze_tile()(Bronze),
         SILVER=silver_tile()(Silver),
         GOLD=gold_tile()(Gold),
         TOTAL=total_tile()(Total))%>%
  select(COUNTRY, GOLD, SILVER, BRONZE, TOTAL)%>%
  kable(
    "html", escape = F,align=c("lcccc"),
  ) %>%
  kable_minimal(html_font = "Cambria") %>%
  row_spec(0, color = "black") %>%
  add_header_above(c("Summer Olympic Medal Events" = 5), color="black", font_size=20)%>%
  column_spec(2:5,width_min='3cm')

summer_kable