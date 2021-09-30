#load packages
library(tidyverse) #wrangling
library(tidytuesdayR) #tidytuesday
library(showtext) #add font
library(ragg) #ggsave
library(stringr) #nlp
library(ggtext) #element_markdown
library(ggbump) #geom_bump

#load data
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

nber <- left_join(papers, paper_authors) %>% 
            left_join(authors) %>% 
            left_join(paper_programs) %>% 
            left_join(programs) %>% 
            mutate(
              catalogue_group = str_sub(paper, 1, 1),
              catalogue_group = case_when(
                catalogue_group == "h" ~ "Historical",
                catalogue_group == "t" ~ "Technical",
                catalogue_group == "w" ~ "General"
              ),
              .after = paper
            ) 

energy <- nber %>% filter(program=="EEE")

energy$title_lower <- tolower(energy$title)

energy_clean <- energy %>% group_by(paper) %>% summarise(year = last(year), 
                                                         title = last(title_lower))

energy_clean2000 <- energy_clean %>% filter(year>=2000)

energy_clean2000 <- energy_clean2000 %>% mutate(presidency = case_when(
  year <= 2008 ~ "W. Bush",
  year <= 2016 ~ "Obama",
  year <= 2020 ~ "Trump",
  year > 2020 ~ "Biden"
))
  
years <- seq(2000,2021,1)
year_gw_counts <- c()
year_cc_counts <- c()

for (j in 1:length(years)){
  year_rows <- energy_clean2000 %>% filter(year==years[j])
  gw_count <- 0
  cc_count <- 0
  for (i in 1:nrow(year_rows)){
    paper_title <- year_rows[i,3]
    if (str_detect(as.character(paper_title), "global warming")){
      gw_count <- gw_count + 1
    }
    else if (str_detect(as.character(paper_title), "climate change")){
      cc_count <- cc_count + 1
    }
  }
  year_gw_counts[j] <- gw_count
  year_cc_counts[j] <- cc_count
}

term_freq <- as.data.frame(cbind(years, year_gw_counts, year_cc_counts))

term_freq_pivot <- term_freq %>% pivot_longer(term_freq, 
                                              cols=c("year_gw_counts", "year_cc_counts"),
                                              names_to = "Term")


#load font
font_add(family = "regular", "Oswald-Regular.ttf")
showtext_auto()

plot_theme <- theme(
  # titles
  plot.title = element_markdown(family = "regular", size = 40, color = "black", hjust=0.5, vjust=0.5),
  plot.subtitle = element_markdown(family = "regular", size = 30, color = "black", hjust = 0.5),
  plot.caption = element_text(family = "regular", size = 26, color = "#cccccc", hjust = 0.5),
  
  # panel and plot background
  panel.grid.major = element_line(color="#cccccc"),
  panel.grid.minor = element_line(color="#cccccc"),
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white"),
  
  # axis
  axis.title = element_blank(),
  axis.text.y = element_text(family="regular", size=18, color="black"),
  axis.text.x = element_text(family="regular", size=25, color="black"),
  axis.line = element_line(),
  
  #no legend
  legend.position = "none"
)

#red rectangles
red1 <- data.frame(xmin1=2000, xmax1=2008, ymin1=0, ymax1=7)
red2 <- data.frame(xmin2=2016, xmax2=2020, ymin2=0, ymax2=7)

#blue rectangles
blue1 <- data.frame(xmin3=2008, xmax3=2016, ymin3=0, ymax3=7)
blue2 <- data.frame(xmin4=2020, xmax4=2021, ymin4=0, ymax4=7)


term_freq_plot <- term_freq_pivot %>% ggplot(aes(x=years, y=value, color=Term)) +
                      geom_rect(data=red1, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1),
                                fill="#940608",
                                alpha=0.2,
                                inherit.aes=FALSE) +
                      geom_rect(data=red2, aes(xmin=xmin2, xmax=xmax2, ymin=ymin2, ymax=ymax2),
                                fill="#940608",
                                alpha=0.2,
                                inherit.aes=FALSE) +
                      geom_rect(data=blue1, aes(xmin=xmin3, xmax=xmax3, ymin=ymin3, ymax=ymax3),
                                fill="#08438C",
                                alpha=0.2,
                                inherit.aes=FALSE) +
                      geom_rect(data=blue2, aes(xmin=xmin4, xmax=xmax4, ymin=ymin4, ymax=ymax4),
                                fill="#08438C",
                                alpha=0.2,
                                inherit.aes=FALSE) +
                      geom_bump(size=1) + 
                      geom_point(size=1.2) +
                      scale_color_manual(values=c("#660694", "#05732D")) +
                      labs(title="Frequency of Terms <span style='color:#05732D'>Global Warming</span> and <span style='color:#660694'>Climate Change</span> in the Titles of NBER Papers",
                           subtitle="during <span style='color:#940608'>Republican</span> and <span style='color:#08438C'>Democratic</span> presidencies",
                           caption="Moriah Taylor | #TidyTuesday | @moriah_taylor58") +
                      coord_cartesian(expand=FALSE, clip="off") +
                      plot_theme

ggsave("term_freq.png",
       plot = term_freq_plot,
       device = agg_png(width = 6, height = 4, units = "in", res = 300))






