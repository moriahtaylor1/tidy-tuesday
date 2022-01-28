#load packages
library(tidyverse) #data wrangling
library(showtext) #custom font
library(ragg) #agg_png()
library(ggrepel) #geom_label_repel()
 
#load data
ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

#add ratings, ranks, number of reviews, and year to details dataframe
ratings_sub <- ratings %>% select(id, rank, average, year, users_rated)
details_ratings <- details %>% left_join(ratings_sub, by="id")

#subset to top 100 games
top100_games <- details_ratings %>% filter(rank <= 100)

#create empty vectors for all mechanics and categories
mechanics <- c()
categories <- c()
#loop through data and append game mechanics and categories to vectors
for (i in 1:nrow(top100_games)){
  game_categories <- unlist(top100_games[i,12])
  #remove punctuation from skills
  no_apos <- gsub("\\'", "", game_categories)
  no_brack1 <- gsub("\\[", "", no_apos)
  no_brack2 <- gsub("\\]", "", no_brack1)
  #create list of skills
  categories_list <- str_split(no_brack2, ", ")
  #add each list item to external list
  for (item in categories_list){
    categories <- c(categories, item)
  }
  game_mechanics <- unlist(top100_games[i,13])
  #remove punctuation from skills
  no_apos <- gsub("\\'", "", game_mechanics)
  no_brack1 <- gsub("\\[", "", no_apos)
  no_brack2 <- gsub("\\]", "", no_brack1)
  #create list of skills
  mechanics_list <- str_split(no_brack2, ", ")
  #add each list item to external list
  for (item in mechanics_list){
    mechanics <- c(mechanics, item)
  }
}
#put counts into dataframes
mechanics_counts <- as.data.frame(table(mechanics))
categories_counts <- as.data.frame(table(categories))

#create indicators for game categories
top100_games <- top100_games %>% mutate(
  category = case_when(
    str_detect(boardgamecategory, "Economic") & str_detect(boardgamecategory, "Science Fiction") & str_detect(boardgamecategory, "Fantasy") ~ "Economic, Fantasy & Science Fiction",
    str_detect(boardgamecategory, "Economic") & str_detect(boardgamecategory, "Fantasy")~ "Economic & Fantasy",
    str_detect(boardgamecategory, "Economic") & str_detect(boardgamecategory, "Science Fiction") ~ "Economic & Science Fiction",
    str_detect(boardgamecategory, "Fantasy") & str_detect(boardgamecategory, "Science Fiction")~ "Fantasy & Science Fiction",
    str_detect(boardgamecategory, "Economic") ~ "Economic",
    str_detect(boardgamecategory, "Science Fiction") ~ "Science Fiction",
    str_detect(boardgamecategory, "Fantasy") ~ "Fantasy"
  )
)

#subset to games that have a value for category
games <- top100_games %>% filter(is.na(category)==FALSE)

#subset to games from 2015-Present with average rating above 8.25
games_sub <- games %>% filter(average>8.25 & year>2014)

#load fonts
font_add(family="regular", "Nunito-Regular.ttf")
font_add(family="bold", "ConcertOne-Regular.ttf")
showtext_auto()

#create plot
plot <- games_sub %>% ggplot(aes(x=year, y=average)) +
  geom_point(aes(color=category, fill=category), size=4, alpha=0.7) +
  scale_color_manual(values=c("#095CA0", "purple", "#1A831F", "red")) +
  scale_fill_manual(values=c("#095CA0", "purple", "#1A831F", "red")) +
  geom_label_repel(aes(label=primary)) +
  facet_wrap(~category, nrow=2) +
  ylim(8.15,8.85) +
  labs(title="Board Game Ratings (2015-2020)",
       subtitle="Board games shown have an average score above 8.25 and fall into the category of Economic, Economic & Science Fiction, Fantasy, or Science Fiction.",
       caption="\nMoriah Taylor | @moriah_taylor58 | #TidyTuesday") +
  ylab("Average Rating") +
  #theme
  theme(
    #titles
    plot.title = element_text(color="white", family="bold", size=45),
    plot.subtitle = element_text(color="white", family="regular", size=16),
    plot.caption = element_text(color="#333333", family="regular", size=22, hjust=0.5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #background
    plot.background = element_rect(fill="black"),
    panel.background = element_rect(fill="black", color="#333333"),
    panel.grid = element_line(color="#333333"),
    #axes
    axis.text = element_text(color="white", family="regular", size=18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color="white", family="regular", size=26),
    axis.ticks = element_blank(),
    #strip
    strip.background = element_rect(fill="black"),
    strip.text = element_text(color="white", family="bold", size=35),
    #legend
    legend.position = 'none'
  )

#save plot
ggsave("Board Games Scatterplot.png",
       plot,
       device = agg_png(width = 5, height = 5, units = "in", res = 300))