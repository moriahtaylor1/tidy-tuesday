#load packages
library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(showtext) #add font
library(ragg) #ggsave
library(ggtext) #element_markdown()

#read data in manually
circuits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/circuits.csv')
constructor_results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_results.csv')
constructor_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_standings.csv')
constructors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructors.csv')
driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv')
pit_stops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/pit_stops.csv')
qualifying <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/qualifying.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/seasons.csv')
status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/status.csv')

###Who is the winningest?###
#filter results table to winners
winners <- results %>% filter(position=="1")
##add info to winners table##
#get info from constructors table
constructors_sub <- constructors %>% select(constructorId, name, nationality)
names(constructors_sub) <- c("constructorId", "teamName", "teamNationality")
winners_info <- merge(winners, constructors_sub, by="constructorId", all.x=TRUE)
#get info from drivers table
drivers_sub <- drivers %>% select(driverId, nationality)
names(drivers_sub)[2] <- "driverNationality"
winners_info <- merge(winners_info, drivers_sub, by="driverId", all.x=TRUE)
#get info from races table
races_sub <- races %>% select(raceId, year)
winners_info <- merge(winners_info, races_sub, by="raceId", all.x=TRUE)
#filter out year 2021
winners_info$year <- as.numeric(winners_info$year)
winners_info <- winners_info %>% filter(year<2021)

##basic bar chart of teams##
winners_info %>% group_by(name) %>% summarise(num_winners=n()) %>%
  filter(num_winners>25) %>%
  ggplot(aes(y=name, x=num_winners)) +
  geom_col()
##basic bar chart of nationalities##
winners_info %>% group_by(nationality) %>% summarise(num_winners=n()) %>%
  ggplot(aes(y=nationality, x=num_winners)) +
  geom_col()

##PLOT STYLING##
#load font
font_add(family="regular", "Baloo2-Medium.ttf")
showtext_auto()

#theme
dark_theme <- theme(
  #titles
  plot.title=element_text(family="regular", hjust=0.5, size=45, color="white"),
  plot.title.position = "plot",
  plot.subtitle = element_text(family="regular", size=25, hjust=0.5, color="white"),
  plot.caption=element_text(family="regular", size=35, color="#333333", hjust=0.5),
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_text(size=30, family="regular", color="white"),
  axis.text.x = element_blank(),
  #no legend
  legend.position = "none")
#make a theme that includes a legend
dark_theme2 <- theme(
  #titles
  plot.title=element_text(family="regular", hjust=0.5, size=35, color="white"),
  plot.title.position = "plot",
  plot.subtitle = element_markdown(family="regular", size=35, hjust=0.5, color="white"),
  plot.caption=element_text(family="regular", size=35, color="#333333", hjust=0.5),
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_line(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text = element_text(size=25, family="regular", color="white"),
  #legend
  legend.position = "top",
  legend.background = element_rect(fill="black", color="black"),
  legend.box.background = element_rect(fill="black", color="black"),
  legend.text = element_text(family="regular", color="white", size=18),
  legend.title = element_blank(),
  legend.key = element_rect(fill="black"))

##TEAMS##
top8_teams <- winners_info %>% group_by(teamName) %>% summarise(num_winners=n()) %>% 
                filter(num_winners>25)
winners_team <- top8_teams %>% ggplot(aes(y=teamName, x=num_winners)) +
                        geom_col(fill=c("#333333", "darkred", "#333333", "#333333", "#333333", "#333333", "#333333", "#333333")) +
                        geom_text(aes(label=num_winners, family="regular"), hjust=1.1, size=10, color="white") +
                        labs(title="Number of Formula 1 Winners by Team",
                             subtitle="8 teams with the most wins",
                             caption="Moriah Taylor | @moriah_taylor58 | #TidyTuesday 2021 Week 37") +
                        coord_cartesian(expand=FALSE) + dark_theme

teams_by_year <- winners_info %>% group_by(year, teamName) %>% summarise(num_winners=n()) %>%
                          filter(teamName %in% top8_teams$teamName)
team_colors <- c("darkcyan", "darkred", "dodgerblue3", "sienna3", "darkorchid4", "hotpink4", "orangered4", "slateblue3")
winners_team_year <- teams_by_year %>% ggplot(aes(x=year, y=num_winners, color=teamName)) +
                        geom_line(size=1.2) + ylim(0,20) +
                        labs(subtitle="<i style='color:#cd6839'>Mercedes</i> dominating since return to Formula 1 in 2010",
                             caption="Moriah Taylor | @moriah_taylor58 | #TidyTuesday 2021 Week 37") +
                        scale_color_manual(values=team_colors) +
                        guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
                        coord_cartesian(expand=FALSE) +
                        dark_theme2

##TEAM NATIONALITY##
winners_team_nation <- winners_info %>% group_by(teamNationality) %>% summarise(num_winners=n()) %>%
                          ggplot(aes(y=teamNationality, x=num_winners)) +
                          xlim(0,515) +
                          geom_col(fill=c("#333333", "#333333", "seagreen4", "#333333", "#333333", "#333333", "#333333", "#333333", "#333333", "#333333")) +
                          geom_text(aes(label=num_winners, family="regular"), hjust=-0.1, size=10, color="white") +
                          labs(title="Number of Formula 1 Winners by Team Nationality",
                               caption="Moriah Taylor | @moriah_taylor58 | #TidyTuesday 2021 Week 37") +
                          coord_cartesian(expand=FALSE, clip="off") + dark_theme

##DRIVER NATIONALITY##
colors_vector <- c(rep("#333333", 6), "seagreen4", rep("#333333", 16))
wins_driver_nation <- winners_info %>% group_by(driverNationality) %>% summarise(num_winners=n()) %>%
                            ggplot(aes(y=driverNationality, x=num_winners)) +
                            xlim(0,340) +
                            geom_col(fill=colors_vector) +
                            geom_text(aes(label=num_winners, family="regular"), hjust=-0.1, size=10, color="white") +
                            labs(title="Number of Formula 1 Wins by Driver Nationality",
                                 caption="Moriah Taylor | @moriah_taylor58 | #TidyTuesday 2021 Week 37") +
                            coord_cartesian(expand=FALSE, clip="off") + dark_theme

##SAVE PLOTS##
ggsave("WinnersByTeam.png",
       plot=winners_team,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))
ggsave("WinnersByTeamByYear.png",
       plot=winners_team_year,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))
ggsave("WinnersByTeamNationality.png",
       plot=winners_team_nation,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))
ggsave("WinsByDriverNationality.png",
       plot=wins_driver_nation,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))

