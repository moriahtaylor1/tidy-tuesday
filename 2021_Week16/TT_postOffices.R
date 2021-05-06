#packages
library(tidyverse)
library(dplyr)
library(tidytuesdayR)
library(ggplot2)
library(extrafont)
library(showtext)
library(sysfonts)
library(ggthemes)
library(ragg)

#fonts
font_add(family = "bold", "AlegreyaSans-Bold.ttf")
font_add(family = "regular", "Merriweather-Regular.ttf")
font_add(family = "light", "AlegreyaSans-Light.ttf")
showtext_auto()
loadfonts(device = "win")

#load in post office data
post <- tidytuesdayR::tt_load(2021, week=16)
post_offices <- post$post_offices

#some cleaning
post_offices <- post_offices[post_offices$discontinued < 2020, ]
post_offices <- post_offices[post_offices$established > 1600, ]
for (r in 1:nrow(post_offices)){
  if (is.na(post_offices[r,10])){
    post_offices[r,10] <- 2005
  }
}

#creating a year group
post_offices$year_group <- ""
for (r in 1:nrow(post_offices)){
  est <- post_offices[r,9]
  dis <- post_offices[r,10]
  if (isTRUE(est <= 1800 & dis>1800)){
    post_offices[r,30] <- "1800"
  }
  else if (isTRUE(est <= 1850 & dis>1850)){
    post_offices[r,30] <- "1850"
  }
  else if (isTRUE(est <= 1900 & dis>1900)){
    post_offices[r,30] <- "1900"
  }
  else if (isTRUE(est <= 1950 & dis>1950)){
    post_offices[r,30] <- "1950"
  }
}


post_offices_csv <- post_offices[,c(1, 4, 30)]
write.csv(post_offices_csv, "export_post_offices.csv", row.names=FALSE)

year_groups <- c("1800", "1850", "1900", "1950", "2000")
selected_offices <- post_offices_csv %>% filter(year_group %in% year_groups)
tidy_offices <- selected_offices %>% group_by(year_group) %>% group_by(state) %>% 
  mutate(office_count = length(unique(name)))

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


theme_niwot <- function(){
  theme_bw() +
    theme(axis.text.x = element_text(size = 16, color="white"),
          axis.text.y = 
          axis.title = element_text(size = 18),
          axis.line.x = element_line(color="white"),
          axis.line.y = element_line(color="white"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 18, vjust = 1, hjust = 0),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.95, 0.15),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 2, linetype = "blank"))
}

my_theme <- theme(
  # titles
  plot.title = element_text(family = "bold", size = 50, color = "white", hjust=1, vjust=0.5),
  plot.subtitle = element_text(family = "bold", size = 46, color = "white", hjust=1, vjust=0.5),
  plot.caption = element_text(family = "light", size = 40, color = "white", hjust = 0.5),
  
  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  
  # axis
  axis.title = element_blank(),
  axis.text.y = element_text(family="bold", size=40, color="white"),
  axis.text.x = element_text(family="regular", size=40, color="white"),
  axis.ticks = element_blank(),
  axis.line.x = element_line(color="white", size=1),
  axis.line.y = element_line(color="white", size=1),
  
  # no legend
  legend.position = "none"
)

(distribution <-
    ggplot(data = tidy_offices,
           aes(x = year_group, y = office_count, fill = year_group)) +
    geom_flat_violin(position = position_nudge(x = 0.2, y = -10), alpha = 0.9)
  +
    geom_point(aes(y = office_count, color = year_group),
               position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
    geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
    labs(title="Number of U.S. Post Offices per State Rises in the 1800s",
         subtitle="only to fall after the turn of the 20th Century",
         caption = "\n [  #TidyTuesday Week 16  |  Moriah Taylor  |  Twitter: moriah_taylor58  |  GitHub: moriahtaylor 1  ]") +
    ylab("") + xlab("") +
    #guides(fill = FALSE, color = FALSE) +
    scale_y_continuous(limits = c(0, 4260)) +
    scale_fill_manual(values = c("#4C3183", "#00768C",  "#0098FF", "#9F9FED")) +
    scale_colour_manual(values = c("#4C3183", "#00768C",  "#0098FF", "#9F9FED")) +
    coord_flip() + my_theme #flip axes
)

ggsave("TT_postOffices.png",
  plot = distribution,
  device = agg_png(width = 7, height = 5, units = "in", res = 300))