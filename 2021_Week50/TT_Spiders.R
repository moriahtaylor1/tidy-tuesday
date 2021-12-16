#load packages
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ragg)

#load data
tuesdata <- tidytuesdayR::tt_load('2021-12-07')
spiders <- tuesdata$spiders

#create indicator if spider is in United States
spiders$usa <- str_detect(spiders$distribution, "USA")
#filter to US spiders
us_spiders <- spiders %>% filter(usa==TRUE)

#find top 10 most common families in US
top_10_families <- us_spiders %>% group_by(family) %>% summarise(count = n()) %>% slice_max(count, n=10)
#store in list
top_10_families_list <- top_10_families$family

#filter to top 10 families
us_spiders_top10 <- us_spiders %>% filter(family %in% top_10_families_list)

#get counts of family per year
spider_counts_us_top10 <- us_spiders_top10 %>% group_by(family, year) %>% summarise(n = n())

#load font
font_add(family = "regular", "KoHo-Regular.ttf")
font_add(family = "bold", "KoHo-Bold.ttf")
showtext_auto()

#create heat map
spider_heatmap <- spider_counts_us_top10 %>% ggplot(aes(x=year, y=family)) + geom_tile(aes(fill=n)) +
  scale_fill_gradient(low="#2f0543", high="#c470eb") +
  scale_y_discrete(limit=rev) +
  coord_cartesian(expand=FALSE) +
  guides(fill=guide_colorbar(barwidth=unit(20, 'lines'), barheight=unit(0.5, 'lines'))) +
  labs(title="Number of Species per Family in United States (1757-2021)",
       subtitle="Top 10 Most Common Families Shown",
       caption = "Moriah Taylor | @moriah_taylor58 | #TidyTuesday Week 50") +
  theme(
    #titles
    plot.title = element_text(color="white", family="bold", hjust=0.5, size=45),
    plot.title.position = "plot",
    plot.subtitle = element_text(color="white", family="regular", hjust=0.5, size=30),
    plot.caption = element_text(color="darkgrey", family="regular", hjust=0.5, size=25),
    plot.caption.position = "plot",
    #axes
    axis.text.y = element_text(color="white", family="bold", size=35),
    axis.text.x = element_text(color="white", family="regular", size=35),
    axis.title = element_blank(),
    #background
    plot.background = element_rect(fill="black"),
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank(),
    #legend
    legend.text = element_text(color="white", family="regular", size=25),
    legend.background = element_rect(fill="black"),
    legend.box.background = element_rect(fill="black"),
    legend.title = element_blank(),
    legend.position = "top"
  )

#save plot       
ggsave("Spider Families Heat Map.png",
       spider_heatmap,
       device = agg_png(width = 8, height = 6, units = "in", res = 300))


