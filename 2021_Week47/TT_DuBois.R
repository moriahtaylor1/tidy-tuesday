library(tidyverse)
library(showtext)
library(ragg)

##Proportion of Freemen and Slaves Among American Negroes##
years <- c(1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870)
percs <- c(0.08, 0.11, 0.135, 0.13, 0.14, 0.13, 0.12, 0.11, 1)
props_df <- as.data.frame(cbind(years, percs))
colnames(props_df) <- c("year", "perc")

props_plot <- props_df %>% ggplot(aes(year, perc)) +
  geom_area(fill="#2e8759") +
  scale_x_continuous(breaks=c(1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870)) +
  coord_cartesian(expand=FALSE) +
  theme(
    axis.title = element_blank(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill="#e9ddd1"),
    plot.margin = margin(0.5, 0.5, 3, 0.5, unit="in")
  )

ggsave("proportions_free_enslaved.png",
       props_plot,
       device = agg_png(width = 5, height = 7, units = "in", res = 300))

years2 <- c(1750, 1760, 1770, 1780, 1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890)
nums <- c(220000, 310000, 462000, 562000, 757208, 1002037, 1377808, 1771656, 2328642, 2873648, 3638808, 4441830, 4880009, 6580793, 7470040)
pop_df <- as.data.frame(cbind(years2, nums))
names(pop_df) <- c("year", "population")
pop_df$year <- factor(pop_df$year)

font_add(family="bold", "AdventPro-Bold.ttf")
font_add(family="regular", "SignikaNegative-Light.ttf")
showtext_auto()

pop_plot <- pop_df %>% ggplot(aes(x=year, y=population)) +
  geom_col(width=0.5, fill="#ca002a") +
  scale_x_discrete(limits=rev, labels=paste(rev(pop_df$year), "-", rev(scales::comma(pop_df$population)), sep=" ")) +
  labs(title = "Increase of the Negro population in the United States of America.",
       subtitle = "Done by Atlanta University.\n") +
  coord_flip() +
  theme(
    #titles
    plot.title = element_text(family="regular", size=40, hjust=0.5, color="black"),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="regular", size=22, hjust=0.5, color="black"),
    #axes
    axis.title = element_blank(),
    axis.text.y = element_text(family="bold", size=30, color="black"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    #background
    plot.background = element_rect(fill="#e9ddd1"),
    panel.background = element_rect(fill="#e9ddd1"),
    panel.grid = element_blank(),
    plot.margin = margin(0.5,0.5,0.5,0.5,unit="in")
  )

ggsave("growing_population.png",
       pop_plot,
       device = agg_png(width = 5, height = 7, units = "in", res = 300))
