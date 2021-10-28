##load packages##
library(tidytuesdayR) #tidytuesday
library(tidyverse) #data wrangling
library(showtext) #custom font
library(ragg) #ggsave
library(formattable) #table

##load data##
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

##data wrangling##
subset_race <- race %>% select(race_year_id, date, distance)
#bind length of race to racer listing
rankings_w_raceinfo <- ultra_rankings %>% left_join(subset_race, by="race_year_id")
#use length to calculate pace in km/hour (length/secs/3600)
rankings_w_raceinfo <- rankings_w_raceinfo %>% mutate(time_in_hours = time_in_seconds/3600)
rankings_w_raceinfo <- rankings_w_raceinfo %>% mutate(pace = round(distance/time_in_hours,1))
#change names to be title case and nationality to be caps
rankings_w_raceinfo <- rankings_w_raceinfo %>% mutate(runner = str_to_title(runner),
                                                      nationality = toupper(nationality))

#subset to female racers
female_racers <- rankings_w_raceinfo %>% filter(gender=="W")

#change runner column to Runner
names(female_racers)[3] <- "Runner"
#get summary statistics for female racers
female_race_counts <- female_racers %>% group_by(Runner) %>% summarise(Nationality = last(nationality),
                                                                       Races = n(),
                                                                       Top100_Finishes= length(which(rank<=100)),
                                                                       Avg_Pace = round(mean(pace, na.rm=TRUE), 2))

#get top 10 female racers with the most races ran
top10_females <- female_race_counts %>% slice_max(order_by=Races, n=10)

##create table##
formattable(top10_females, align=c("l","c","l","c","c"), list(
  Races = normalize_bar("lightgray", 0.6),
  Top100_Finishes = color_tile("cornflowerblue", "pink"),
  Avg_Pace = formatter("span",
                       style = x ~ style(color=ifelse(rank(-x)<=3, "cornflowerblue", "black"),
                                         font.weight=ifelse(rank(-x)<=3, "bold", "regular")))
))


##plot cumulative distance run over time##
#get list of top10 female runners by number of races ran
top10_females_list <- top10_females$Runner
#subset all runners to this top10
top10_allraces <- rankings_w_raceinfo %>% filter(runner %in% top10_females_list)
#create variable for cumulative distance ran
top10_cumulative <- top10_allraces %>% arrange(date) %>% group_by(runner) %>%
  mutate(cum_dist=cumsum(distance))
#load font
font_add(family = "regular", "Spartan-Regular.ttf")
showtext_auto()
##create stacked area plot##
cum_plot <- top10_cumulative %>% ggplot(aes(x=date, y=cum_dist, color=factor(runner))) +
              geom_line(size=0.8) +
              xlab("") + ylab("Cumulative Distance Ran (km)") +
              labs(title="Cumulative Distance Ran by Female Runners with the Most Races",
                   caption="Moriah Taylor | #TidyTuesday | @moriah_taylor58") +
              scale_color_manual(values=c("cornflowerblue","#068762","#907fa9","#09BAC7","pink",
                                          "#9cbf60","#BE5B19","#BE9419","#3C127C","#910B50")) +
              theme_minimal() +
              theme(legend.title=element_blank(),
                    legend.text=element_text(family="regular"),
                    plot.title=element_text(family="regular"),
                    plot.caption=element_text(family="regular"),
                    plot.title.position="plot",
                    plot.caption.position = "plot",
                    axis.text = element_text(family="regular"),
                    axis.title = element_text(family="regular"))
#save plot       
ggsave("Cumulative Distance Ran.png",
       cum_plot,
       device = agg_png(width = 5, height = 3.5, units = "in", res = 300),
       bg="white")
