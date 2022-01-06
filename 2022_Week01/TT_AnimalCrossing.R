#load packages
library(tidyverse) #data cleaning and wrangling
library(tidytuesdayR) #get data
library(ragg) #save visuals
library(showtext) #use custom text in visuals
library(DescTools) #Zodiac() function

#load all datasets
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

#convert birthday to date
villagers$birthday <- as.Date(villagers$birthday, format="%m-%d")
#evaluate birthday dates to zodiac signs
villagers$sun_sign <- Zodiac(villagers$birthday)
#put sun signs in order by factoring the column
villagers$sun_sign <- factor(villagers$sun_sign,
                             levels = c("Aries", "Taurus", "Gemini", "Cancer", "Leo",
                                        "Virgo", 'Libra', "Scorpio", "Sagittarius", "Capricorn",
                                        "Aquarius", "Pisces"))

#load fonts
font_add(family="title", "AnimalCrossing.ttf")
font_add(family="regular", "Nunito-Bold.ttf")
showtext_auto()

#visualize frequency of sun signs
zodiac_plot <- villagers %>% group_by(sun_sign) %>% summarise(count = n()) %>%
  ggplot(aes(x=count, y=sun_sign)) + geom_col(fill="#069e78", width=0.6) + 
  geom_text(aes(label=count), family="regular", size=12, hjust=1.2, color="#f5c338") +
  labs(title="Animal Crossing",
       subtitle="Zodiac Signs of Villagers\n",
       caption = "\nMoriah Taylor | @moriah_taylor58 | #TidyTuesday") +
  scale_y_discrete(limit=rev) +
  coord_cartesian(expand=FALSE, clip="off") +
  theme(
    #titles
    plot.title = element_text(family="title", hjust=0.5, color="white", size=100),
    plot.subtitle = element_text(family="regular", hjust=0.5, color="white", size=40),
    plot.caption = element_text(family="regular", hjust=0.5, color="#446e79", size=32),
    #title positions
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #background
    plot.margin = margin(0.5,0.5,0.5,0.5,unit="in"),
    plot.background = element_rect(fill="#73bcce"),
    panel.background = element_rect(fill="#73bcce"),
    panel.grid = element_blank(),
    #axes
    axis.text.y = element_text(family="title", color="white", size=40),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

#save plot
ggsave("villager_zodiac.png",
       plot=zodiac_plot,
       device = agg_png(width = 5, height = 8, units = "in", res = 300))



