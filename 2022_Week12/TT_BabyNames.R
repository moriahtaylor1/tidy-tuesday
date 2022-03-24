#load packages
library(tidyverse)
library(ragg)
library(showtext)
library(gghighlight)
#notin function
`%notin%` <- Negate(`%in%`)

#load data
babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

#clean data
babynames_clean <- babynames %>% filter(name!=c("Infant", "Unknown", "Baby"))

#separate into boys and girls
girls <- babynames_clean %>% filter(sex=="F")
boys <- babynames_clean %>% filter(sex=="M")

#perform an inner join to get separate counts for boys and girls
join <- inner_join(girls, boys, by=c("year"="year", "name"="name"), 
                    suffix = c("girls", "boys"))

#create arbitrary cutoff for gender neutral names
gender_neutral <- join %>% mutate(n = ngirls+nboys) %>%
  filter(n>200 & abs(ngirls-nboys)<=100)

#total number of babies with gender neutral names
gender_neutral_totals <- gender_neutral %>% group_by(name) %>% summarise(total_n = sum(n))

#create list of top 15 neutral names
top_neutral_names <- c("Jessie", "Peyton", "Marion", "Casey", "Leslie",
                       "Justice", "Shea", "Devyn", "Jan", "Pat", 
                       "Ashton", "Rene", "Jackie", "Lynn", "Riley")

#filter babynames df to top neutral baby names
top_neutrals <- join %>% filter(name %in% top_neutral_names)

#load font
font_add(family="regular", "Spartan-Regular.ttf")
font_add(family="bold", "Nunito-Bold.ttf")
font_add(family="title", 'FrederickatheGreat-Regular.ttf')
showtext_auto()

#create visual
top_neutrals_facet <- top_neutrals %>% mutate(total_prop = propgirls + propboys) %>%
  ggplot(aes(x=year)) +
  geom_area(aes(y=total_prop), color="#c9c9c9", fill="#c9c9c9") +
  geom_line(aes(y=propgirls), color="red", size=1) +
  geom_line(aes(y=propboys), color="blue", size=1) +
  facet_wrap(~name, nrow=3) +
  coord_cartesian(expand=FALSE) +
  labs(y = "Total Proportion of All Names",
       title = "Top 15 Gender Neutral Baby Names of All Time",
       subtitle = "in the United States",
       caption = "Moriah Taylor | @moriah_taylor58 | #TidyTuesday") +
  theme(
    #titles
    plot.title = element_text(hjust=0.5, family="bold", size=65),
    plot.subtitle = element_text(hjust=0.5, color="#444444", family="bold", size=30),
    plot.caption = element_text(hjust=0.5, color="#c9c9c9", family="regular", size=25),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #background
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white", color="black"),
    panel.grid = element_blank(),
    #axes
    axis.title.x = element_blank(),
    axis.title.y = element_text(family="bold", size=35),
    axis.ticks.x = element_line(size=0.5),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(family="bold", size=15),
    axis.text.y = element_text(family="regular", size=15),
    #strip
    strip.background = element_rect(fill="black"),
    strip.text = element_text(color="white", family="title", size=40)
  )

#save visual
ggsave("neutral_names_facet.png",
       plot = top_neutrals_facet,
       device = agg_png(width = 9, height = 5, units = "in", res = 300))