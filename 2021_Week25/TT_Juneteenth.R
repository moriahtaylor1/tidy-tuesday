library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(ggtext) 
library(showtext) #add font
library(ragg) #ggsave
library(ggthemes) #plot themes
library(tm) #text parsing
library(Biostrings) #count letters

##WEEK 24 DATA: "AFRICAN-AMERICAN ACHIEVEMENTS"##
tuesdata24 <- tidytuesdayR::tt_load(2020, week = 24)
firsts <- tuesdata24$firsts
science <- tuesdata24$science
##WEEK 25 DATA: "AMERICAN SLAVERY AND JUNETEENTH"##
tuesdata25 <- tidytuesdayR::tt_load(2020, week = 25)
blackpast <- tuesdata25$blackpast
slave_routes <- tuesdata25$slave_routes
census <- tuesdata25$census
african_names <- tuesdata25$african_names

##CENSUS DATA##
census_total <- census %>% filter(region=="USA Total")
#pivot data
census_pivot <- census_total %>% select(year, white, black, black_free, black_slaves) %>%
                  pivot_longer(!year, names_to="pop_group", values_to="pop")
#filter to black free and black enslaved populations
census_black <- census_pivot %>% filter(pop_group %in% c("black_free", "black_slaves"))

#initial plot
(black_census_line1 <- ggplot(census_black) + geom_line(aes(x=year, y=pop, group=pop_group, color=pop_group)))
  
#add font
font_add(family = "regular", "Merriweather-Regular.ttf")
showtext_auto()
#plot theme
line_theme <- theme(
  #titles
  plot.title=element_markdown(family="regular", vjust=0.5,
                              hjust=0.5, size=60, color="white"),
  plot.caption=element_text(family="regular", size=28, color="#cccccc",
                            vjust=-4, hjust=0.5),
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title.x = element_text(size=50, family="regular", color="white"),
  axis.title.y = element_blank(),
  axis.text.y = element_text(size=35, family="regular", color="white"),
  axis.text.x = element_text(size=35, family="regular", color="white"),
  #no legend
  legend.position = "top",
  legend.title = element_blank(),
  legend.text = element_text(size=35, family="regular", color="white"),
  legend.background = element_rect(fill='black'),
  legend.box.background = element_rect(fill='black'))

census_colors = c("#D47E11", "#7A4E26")
#stylized plot
(black_census_line <- ggplot(census_black) + 
                        #add lines
                        geom_line(aes(x=year, y=pop, group=pop_group, color=pop_group)) +
                        #add points
                        geom_point(aes(x=year, y=pop, group=pop_group, color=pop_group), shape=15) + 
                        scale_x_continuous(breaks = seq(1790, 1870, 10)) +
                        scale_y_continuous(labels = scales::comma, limits=c(0, 5000000),
                                           expand=c(0,0)) +
                        scale_color_manual(values=census_colors, labels = c("Free", "Enslaved"), name="") + 
                        line_theme +
                        labs(title="Population of Black Americans  1790-1870",
                             caption="Source: #TidyTuesday | Moriah Taylor | Twitter: moriah_taylor58 | GitHub: moriahtaylor1",
                             x="Year"))

ggsave("census.png",
       plot=black_census_line,
       device = agg_png(width = 8, height = 6, units = "in", res = 300))

##FIRSTS##
#counts
firsts_categories <- firsts %>% count(category)
#initial bar plot
(firsts_bar1 <- firsts %>% ggplot(aes(x=category)) +
                geom_bar())

bar_theme <- theme(
  #titles
  plot.title=element_markdown(family="regular", vjust=0.5,
                              hjust=0, size=50, color="white"),
  plot.subtitle=element_markdown(family="regular", vjust=0.5,
                              hjust=0, size=50, color="white"),
  plot.caption=element_text(family="regular", size=28, color="#cccccc",
                            vjust=-4, hjust=0.5),
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size=35, family="regular", color="white"),
  #no legend
  legend.position="none")

(firsts_bar <- firsts_categories %>% ggplot(aes(x=category, y=n, fill=category)) +
                  geom_bar(stat="identity") + 
                  scale_fill_manual(values=c("#d47e11", "gray30", "gray30", "gray30", "gray30", "gray30", "gray30", "gray30")) +
                  geom_text(aes(label=n, family="regular"), 
                            position=position_dodge(width=0.9), hjust=1.2, size=10, color="white") +
                  coord_flip() + bar_theme + 
                  labs(title="Most Common Category for African-American",
                       subtitle="Achievements is <i style='color:#d47e11'>Arts & Entertainment</i>",
                       caption="Source: #TidyTuesday | Moriah Taylor | Twitter: moriah_taylor58 | GitHub: moriahtaylor1"
                       ))

ggsave("accomplishment_categories.png",
       plot=firsts_bar,
       device = agg_png(width = 8, height = 6, units = "in", res = 300))

##AFRICAN_NAMES##
name_counts <- african_names %>% count(name)

letters <- paste(name_counts$name, collapse="")
letters_clean <- tolower(removePunctuation(letters))
letters_split <- strsplit(letters_clean, split="")
letters_table <- as.data.frame(table(letters_split))
top15_letters <- letters_table %>% slice_max(Freq, n=15)

bar_theme2 <- theme(
  #titles
  plot.title=element_markdown(family="regular", vjust=0.5,
                              hjust=0, size=75, color="white"),
  plot.subtitle=element_markdown(family="regular", vjust=0.5,
                                 hjust=0, size=75, color="white"),
  plot.caption=element_text(family="regular", size=28, color="#cccccc",
                            vjust=-4, hjust=0.5),
  #background
  panel.border=element_blank(),
  panel.grid.major = element_line(color="gray10"),
  panel.grid.minor = element_line(color="gray10"),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks = element_blank(),
  axis.line.y = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank(),
  axis.text.x = element_text(size=70, family="regular", color="white"),
  axis.text.y = element_text(size=35, family="regular", color="white"),
  #no legend
  legend.position="none")

letters_bar <- top15_letters %>% ggplot(aes(x=letters_split, y=Freq)) +
                  geom_bar(stat="identity", fill="#7A4E26") + 
                  scale_y_continuous(labels = scales::comma, limits=c(0,80000), expand=c(0,0)) + 
                  labs(title="Letter Frequency in Over 90,000 Recorded Names of",
                       subtitle="Africans Freed from Slave Ships",
                       caption="Source: #TidyTuesday | Moriah Taylor | Twitter: moriah_taylor58 | GitHub: moriahtaylor1"
                  ) +
                  bar_theme2

ggsave("letter_freq.png",
       plot=letters_bar,
       device = agg_png(width = 10, height = 6, units = "in", res = 300))



