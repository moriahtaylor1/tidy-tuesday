#load packages
library(tidyverse) #data wrangling
library(showtext) #custom fonts
library(ggbeeswarm) #beeswarm plotting with ggplot
library(ggtext) #element_markdown
library(ragg) #agg_png()

#read in data manually
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

#use case when to fix typo
stressor <- stressor %>% mutate(stressor = case_when(
  stressor=="Varroa mites" ~ "Varroa Mites",
  stressor=="Other pests/parasites" ~ "Other Pests/Pesticides",
  stressor=="Disesases" ~ "Diseases",
  stressor=="Pesticides" ~ "Pesticides",
  stressor=="Other" ~ "Other",
  stressor=="Unknown" ~ "Unknown"
))

#factorize months
stressor$months <- factor(stressor$months, levels = c("January-March",
                                                      "April-June",
                                                      "July-September",
                                                      "October-December"))

#filter to year 2020
stressor2020 <- stressor %>% filter(year==2020)

#reduce to specific stressors
stressor2020_red <- stressor2020 %>% filter(stressor %in% c("Varroa Mites", "Pesticides", "Diseases", "Other Pests/Pesticides"))

#factorize stressors
stressor2020_red$stressor <- factor(stressor2020_red$stressor, levels=c("Diseases",
                                                                        "Pesticides",
                                                                        "Varroa Mites",
                                                                        "Other Pests/Pesticides"))

#load font
font_add(family="regular", "SignikaNegative-Regular.ttf")
showtext_auto()

#make plot
bee_plot <- ggplot(stressor2020_red, aes(x=stressor, y=stress_pct, color=stressor)) +
  geom_beeswarm(size=0.5) +
  facet_wrap(~months, nrow=1) +
  scale_color_manual(values = c("#86b918", "#c76009", "#9258b1", "#3ab5b3")) +
  labs(title="Bee Colony Stressors in the U.S.",
       subtitle="Percentage of colonies affected in each state by <strong style='color:#86b918'>Diseases</strong>, <strong style='color:#c76009'>Pesticides</strong>, <strong style='color:#9258b1'>Varroa Mites</strong>, and <strong style='color:#3ab5b3'>Other Pests/Pesticides</strong>.",
       caption="Moriah Taylor | @moriah_taylor58 | #TidyTuesday") +
  ylab("% Colonies Affected") +
  coord_cartesian(clip="off") +
  theme(
    #plot
    plot.title = element_text(color="white", family="regular", size=65),
    plot.subtitle = element_markdown(color="white", family="regular", size=40),
    plot.caption = element_text(color="grey", family="regular", size=30, hjust=0.5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #background
    plot.background = element_rect(fill="black"),
    panel.background = element_rect(fill="black", color="white"),
    panel.grid = element_blank(),
    #axes
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="white", family="regular", size=30),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color="white", family="regular", size=40),
    axis.ticks = element_blank(),
    #strip
    strip.background = element_rect(fill="white"),
    strip.text = element_text(color="black", family="regular", size=50),
    #legend
    legend.position = "none"
  )

ggsave("Stressor_Beeswarm.png",
       plot=bee_plot,
       device = agg_png(width = 9, height = 4, units = "in", res = 300))