library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(showtext)
library(extrafont)
library(ragg)
library(htmltools)
library(ggtext)

#load data
netflix <- read.csv("Netflix_Titles.csv")

#subset to movies
movies <- subset(netflix, type=="Movie")

#see counts
movies %>% count(director, sort=TRUE)

#store counts into dataframe
directors <- c("RAUL CAMPOS, JAN SUTER", "MARCOS RABOY", "JAY KARAS","CATHY GARCIA-MOLINA", "JAY CHAPMAN", "MARTIN SCORSESE", "STEVEN SPIELBERG", "QUENTIN TARANTINO")
n <- c(18, 16, 14, 13, 12, 12, 10, 8)
directors_df <- data.frame(directors, n)



#fonts
font_add(family = "bold", "AlegreyaSans-Bold.ttf")
font_add(family = "regular", "AlegreyaSans-Regular.ttf")
font_add(family = "title", "netflix_font.otf")
showtext_auto()

#theme
plot_theme <- theme(
  # titles
  plot.title = element_text(family = "title", size = 60, color = "white", hjust=0.5, vjust=0.5),
  plot.subtitle = element_markdown(family="regular", size = 30, hjust=1, vjust=0.5, lineheight = 0.5),
  plot.caption = element_text(family = "regular", size = 42, color = "darkgray", hjust = 1),
  
  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black", color="white", size=2),
  
  # axis
  axis.title.y = element_blank(),
  axis.text.y = element_text(family="bold", size=28, color="white"),
  axis.text.x = element_blank(),
  axis.ticks.x.bottom = element_blank(),
  
  #no legend
  legend.position = "none"
)


(netflix_plot <- ggplot(data=directors_df, aes(x=directors, y=n)) + 
    geom_bar(stat="identity", fill="#E50914") + scale_y_continuous(expand = c(0,0.3)) +
  geom_text(aes(label=n, family="bold"), hjust=1.5, size=15, color="black") +
  labs(title="Who's Who at Netflix",
       subtitle = "<p style=color:white>Most movie buffs are familiar with names such as Scorsese, Spielberg, and Tarantino. So who are these Netflix movie <br>directors with over a dozen accreditations? Campos/Suter, Raboy, Karas and Chapman are all producers of Netflix comedy specials<br>- although Karas is much more well-known for his work producing TV shows such as Superstore, Parks & Rec, and The Unicorn.</p>",
       caption = "[ Moriah Taylor  |  #TidyTuesday Week 17  |  Twitter: moriah_taylor58  |  GitHub: moriahtaylor1 ]") +
    scale_x_discrete(limits=directors_df$directors) + xlab("") + ylab("") +
  coord_flip() + plot_theme)

ggsave("TT_Netflix.png",
       plot = netflix_plot,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))
