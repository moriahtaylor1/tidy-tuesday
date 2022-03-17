library(tidyverse)
library(ragg)
library(showtext)
library(gghighlight)

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

#list of top 10 receiving and sending countries
top10_list <- c("DE", "PL", "FR", "UK", "ES", "HU", "TR", "RO", "CZ", "SK")

#filter
erasmus_top10 <- erasmus %>% filter(receiving_country_code %in% top10_list)

#create city&country labels
erasmus_top10 <- erasmus_top10 %>% mutate(
  receiving_city_country = paste0(receiving_city, ", ", receiving_country_code),
  sending_city_country = paste0(sending_city, ", ", sending_country_code),
  year = as.numeric(str_sub(academic_year, 6, 9))
)

iso<-read_delim("https://raw.githubusercontent.com/BjnNowak/TidyTuesday/main/data/iso.csv",delim=';')

names(iso)[1] <- "receiving_country_code"

erasmus_w_names <- erasmus_top10 %>% left_join(iso, by="receiving_country_code")

font_add(family="regular", "RobotoSlab.ttf")
showtext_auto()

plot <- erasmus_w_names %>% group_by(year, country_name) %>% summarise(n=sum(participants)) %>%
  ggplot(aes(x=factor(year), y=n, fill=country_name)) +
  geom_col() +
  coord_flip(expand=FALSE) +
  labs(title="ERASMUS Receiving Countries by Year",
       caption="Moriah Taylor | @moriah_taylor58 | #TidyTuesday",
       y="Number of Students") +
scale_x_discrete(limits=rev) +
  scale_fill_manual(values=(c("#A5B79A", "#B57028", "#701F16", "#131D20", "#46619E", "#557016", "#ac9ab7", "#286db5", "#78b7ca", "#9e4661"))) +
  theme(
    #titles
    plot.title = element_text(family="regular", hjust=0.5, size=60),
    plot.caption = element_text(family="regular", color="#444444", hjust=0.5, size=28),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #background
    plot.background = element_rect(fill="#cccccc"),
    panel.background = element_rect(fill="#cccccc"),
    panel.grid = element_blank(),
    #axes
    axis.title.y=element_blank(),
    axis.title.x=element_text(family="regular", size=50),
    axis.text = element_text(family="regular", size=40),
    axis.ticks = element_blank(),
    #legend
    legend.position = "right",
    legend.text = element_text(family="regular", size=30),
    legend.title = element_blank(),
    legend.background = element_rect(fill="#cccccc", color="#cccccc"),
    legend.box.background = element_rect(fill='#cccccc', color="#cccccc"),
    legend.key = element_rect(fill="#cccccc")
  )

ggsave("erasmus_receiving.png",
       plot,
       device = agg_png(width = 6, height = 4, units = "in", res = 300))



