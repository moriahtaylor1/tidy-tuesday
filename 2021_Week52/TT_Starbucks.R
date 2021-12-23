library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ragg)

tuesdata <- tidytuesdayR::tt_load('2021-12-21')
starbucks <- tuesdata$starbucks

fraps <- starbucks %>% filter(str_detect(product_name, "Frappuccino"))

tall_fraps_nowhip <- fraps %>% filter(size=="tall" & whip==0)

tall_fraps_nowhip$milk <- factor(tall_fraps_nowhip$milk)

#load font
font_add(family = "regular", "NotoSans-Regular.ttf")
font_add(family = "bold", "NotoSans-Bold.ttf")
showtext_auto()

fraps_plot <- tall_fraps_nowhip %>% mutate(milk = fct_relevel(milk, "1", "2", "5", "3", "4")) %>%
  ggplot(aes(y=calories, x=caffeine_mg, color=as.factor(milk))) +
  geom_point(size=3) + 
  labs(title="Starbucks Frappuccinos",
       subtitle="Caffeine, Calories & Milk",
       caption="Moriah Taylor | @moriahtaylor58 | #TidyTuesday",
       color="Type of Milk") +
  xlab("Caffeine (mg)") + ylab("Calories") +
  ylim(c(0,250)) +
  coord_cartesian(clip="off") +
  scale_color_manual(values=c('#83b989', '#067313', '#022e08', '#643E1C', '#2882B1'),
                     labels=c("Non-fat Dairy", "2% Fat Dairy", "Whole Fat Dairy", "Soy", "Coconut")) +
  theme(
    #labels
    plot.title = element_text(color="#322607", family="bold", size=35, hjust=0.5),
    plot.subtitle = element_text(color="#322607", family="bold", size=25, hjust=0.5),
    plot.caption = element_text(color="#322607", family="regular", size=18, hjust=0.5),
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    
    #plot and panel
    plot.background = element_rect(fill="#D1C6AA"),
    panel.background = element_rect(fill="#D1C6AA"),
    panel.grid = element_blank(),
    
    #axes
    axis.title = element_text(family="bold", color="#322607", size=25),
    axis.text = element_text(family="regular", color="#322607", size=16),
    
    #legend
    legend.background = element_rect(fill="#ede8dd", color="#322607"),
    legend.box.background = element_rect(fill="#ede8dd"),
    legend.key = element_rect(fill="#ede8dd", color=NA),
    legend.text = element_text(color="#322607", family="regular", size=18),
    legend.title = element_text(color="#322607", family="bold", size=25),
    legend.justification = c("right", "top")
  )

ggsave("Starbucks_Fraps.png",
       fraps_plot,
       device = agg_png(width = 6, height = 4, units = "in", res = 300))



