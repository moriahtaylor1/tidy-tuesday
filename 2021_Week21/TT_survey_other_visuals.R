#load packages
library(tidytuesdayR) #tidy tuesday data
library(tidyverse) #wrangling
library(extrafont) #fonts
library(showtext) #fonts
library(ragg)  #save as png
library(ggthemes) #custom plot theme
library(ghibli)   #studio ghibli palettes
library(tidytext) #text mining
library(wordcloud)  #word cloud plot
library(viridis)  #cb-friendly palettes
library(extrafont)  #font
library(showtext)  #font 


#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 21)
salaries <- tuesdata$survey

#subset to computing and tech industry
tech <- salaries %>% filter(industry=="Computing or Tech")


##WORD CLOUD PLOT##
job_title_free_wf <- tech %>%
  mutate(job_comment = as.character(additional_context_on_job_title)) %>%
  unnest_tokens(output = job_word,
                input = additional_context_on_job_title) %>%
  filter(!(is.na(job_word)),
         is.na(as.numeric(job_word)),
         !(job_word %in% stop_words$word)) %>%
  group_by(job_word) %>%
  summarise(n = n()) %>%
  filter(n < 100) %>%
  ungroup()

#colors from discrete rocket palette from viridis package
rocket_pal = c("#faebdd", "#f69c73", "#e83f3f", '#a11a5b', "#4c1d4b")


(job_wf_plot <- job_title_free_wf %>%
  with(wordcloud(words = job_word, freq = n, max.words = 100,
                 random.order = FALSE, random.color = FALSE,
                 color = rocket_pal)))


##STACKED BAR CHART##
#filter to jobs in the US (many variations of country name)
us <- c("US", "United States", "usa", "us", "Unite States", "United States", "United States of America",
        "united states", "U.S", "U.S.", "United states", "U.S.A.", "United States Of America", "Uniyed states",
        "Usa")
us_tech <- tech %>% filter(country %in% us)

#create salary bins
us_tech2 <- us_tech %>% mutate(
                              salary_bin = case_when(
                                annual_salary < 25000 ~ "<25k",
                                annual_salary < 50000 ~ "25k-50k",
                                annual_salary < 100000 ~ "50k-100k",
                                annual_salary < 200000 ~ "100k-200k",
                                annual_salary < 500000 ~ "200k-500k",
                                annual_salary > 500000 ~ ">500k",
                              )
)

#turn bins into factors
us_tech2$salary_bin <- factor(us_tech2$salary_bin, levels = c("<25k", "25k-50k", "50k-100k",
                                                              "100k-200k", "200k-500k", ">500k"))

#load font
font_add(family = "regular", "Merriweather-Regular.ttf")
showtext_auto()

#my plot theme
my_theme <- theme(
  # titles
  plot.title = element_text(family = "regular", size = 35, color = "white", hjust=0, vjust=0.5),
  plot.caption = element_text(family = "regular", size = 18, color = "#555555", hjust = 0),
  
  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  
  # axis
  axis.title = element_text(family="regular", size=24, color="white"),
  axis.text = element_text(family="regular", size=18, color="white"),
  axis.line.x = element_line(color="white", size=1),
  axis.line.y = element_line(color="white", size=1),
  
  #legend
  legend.position = c(0.75, 0.5),
  legend.title = element_text(family="regular", color="white", size=24),
  legend.text = element_text(family="regular", color="white", size=18),
  legend.background = element_rect(fill = "black", color="white")
)

#barchart
(barchart <- ggplot(us_tech2, aes(x=salary_bin, fill = how_old_are_you)) +
    geom_bar() +
    labs(x = "\nSalary",
         y = "Number of Responses",
         title = "Majority of respondents in tech \nmake between 100k and 200k",
         fill = "Age Group",
         caption = "\nMoriah Taylor | Source: #TidyTuesday | Twitter: moriah_taylor58 | GitHub: moriahtaylor1")+ 
    scale_fill_viridis_d(direction=-1) +
    my_theme)

# save image
ggsave("salaries_by_age.png",
       plot = barchart,
       device = agg_png(width = 8, height = 5, units = "in", res = 300))