library(tidyverse)
library(tidytuesdayR)
library(viridis)
library(ggalt)

sauces <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv')

#create names which will serve as titles/labels of facet plot
sauces <- sauces %>% mutate(season_name=paste("Season", season))

#create factor variable to organize facets
sauces$season_name2 <- factor(sauces$season, levels = seq(1,21,1),
                  labels = unique(sauces$season_name))

#create bar plot
sauces_plot <- sauces %>% filter(sauce_number<6) %>% ggplot(aes(x=sauce_number, y=scoville)) +
  geom_col(aes(fill=scoville)) + scale_fill_viridis(option="plasma") +
  facet_wrap(~season_name2, nrow=4) +
  theme(
    #background
    panel.border=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
    #axes
    axis.ticks = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    #legend
    legend.position = "none",
    #strip(facet label)
    strip.background = element_rect(fill="black"),
    strip.text = element_text(color="white", size=22)
  )

#save plot
ggsave("sauces_plot.png",
       plot = sauces_plot,
       device = agg_png(width = 6, height = 4, units = "in", res = 300))

