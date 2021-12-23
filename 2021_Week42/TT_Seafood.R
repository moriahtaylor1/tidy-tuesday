#load packages
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(janitor)
library(ragg)
library(stringr)
library(ggthemes)
library(ggtext)

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 42)
#look at capture vs aquaculture
capture_vs_farm <- tuesdata$`capture-fisheries-vs-aquaculture`
#clean column names
capture_vs_farm <- capture_vs_farm %>% clean_names()

#filter data down
income_fishing <- capture_vs_farm %>% filter(str_detect(entity, "income"))
income_cats <- income_fishing %>% filter(entity=="High income" | entity=="Low & middle income") 

#rename columns
names(income_cats)[4:5] <- c("farming", "capture")

#pivot
income_cats_pivot <- income_cats %>% select(entity, year, farming, capture) %>%
  pivot_longer(c(farming, capture), names_to = "method", values_to = "production_metric_tons")

#create new column
income_cats_pivot$income_method <- paste(income_cats_pivot$entity, income_cats_pivot$method, sep="-")


plot <- income_cats_pivot %>% ggplot(aes(x=year, y=production_metric_tons, color=income_method)) +
            geom_line(size=1.2, aes(linetype=method)) +
            labs(title="Farming versus Capture Fishing",
                 subtitle="for <strong style='color:#480483'>High Income</strong> Countries and <strong style='color:#B85500'>Low & Middle Income</strong> Countries",
                 caption="<i>(Dark solid lines indicate capture fishing and light dashed lines indicate fish farming)</i>") +
            xlab("") + ylab("Production (Metric Tons)
                            ") +
            scale_color_manual(values=c("#480483", "#8A52BC", "#B85500", "orange")) +
            scale_y_continuous(labels=scales::comma) +
            coord_cartesian(expand=FALSE) +
            theme_economist() +
            theme(plot.title=element_markdown(size=24, hjust=0, face="plain"),
                  plot.title.position="plot",
                  plot.subtitle=element_markdown(size=18, hjust=0, face="plain"),
                  plot.caption=element_markdown(size=14, hjust=1),
                  legend.position="none")

ggsave("farming_capture_income.png",
       plot=plot,
       device = agg_png(width = 8, height = 5, units = "in", res = 300))


