library(tidyverse)
library(tidytuesdayR)
library(ggplot2)
library(ggtext)
library(showtext)
library(extrafont)
library(ragg)


#add fonts
font_add(family = "regular", "Rubik-VariableFont_wght.ttf")
font_add(family = "title", "BebasNeue-Regular.ttf")
showtext_auto()


my_theme <- theme(
                #titles
                plot.title=element_text(family="title", vjust=0.5,
                                          hjust=0.5, size=65, color="white"),
                plot.caption=element_text(family="regular", size=35, color="#cccccc",
                                          vjust=0.5, hjust=0.5),
                #background
                panel.border=element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm"),
                #axes
                axis.ticks.length=unit(0.15, "cm"),
                axis.ticks = element_line(color="#cccccc"),
                axis.line = element_blank(),
                axis.title.x = element_text(size=35, family="regular", color="#cccccc"),
                axis.title.y = element_text(size=30, family="regular", color="#cccccc"),
                axis.text = element_text(size=20, family="regular", color="#cccccc"),
                #legend
                legend.position = "top",
                legend.direction = "horizontal",
                legend.background = element_rect(fill="black", color="black"),
                legend.text = element_text(size=25, family="regular", color="#cccccc"))

#load data
summary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')

avg_viewers <- summary %>% 
  select(season, viewers_mean) %>%    #select cols of interest
  #create variables
  mutate(id=row_number(),   #use row id for positioning
         season=season,
         start=lag(viewers_mean),    #lag() function for time series
         end=viewers_mean,
         dif=viewers_mean-lag(viewers_mean),
         change_type=case_when(    #increase/decrease/no change between data points
           dif>0 ~"INCREASE",
           dif==0 ~ "NO CHANGE",
           dif<0 ~ "DECREASE"
         ))

avg_viewers_s1 <- avg_viewers %>% filter(season==1)    #first season
avg_viewers_s38 <- avg_viewers %>% filter(season==38)    #last season
#seasons with no change
#have to create segments in graph or else don't show up
avg_viewers_s28 <- avg_viewers %>% filter(season==28)
avg_viewers_s30 <- avg_viewers %>% filter(season==30)
avg_viewers_s32 <- avg_viewers %>% filter(season==32)
avg_viewers_s34 <- avg_viewers %>% filter(season==34)
avg_viewers_s36 <- avg_viewers %>% filter(season==36)

(survivor_waterfall <- avg_viewers %>%
  ggplot() +
  #change bars
  geom_rect(data=avg_viewers,
            aes(x=season,
                fill=change_type,
                xmin = id - 0.45, 
                xmax = id + 0.45,
                ymin = end,
                ymax = start)) +
  #first bar
  geom_rect(data=avg_viewers_s1,
            aes(x=season,
                xmin = id - 0.45, 
                xmax = id + 0.45,
                ymin = 0,
                ymax = viewers_mean), fill="#577590") +
  #no change segment - season 28
  geom_segment(data=avg_viewers_s28,
               aes(x = id - 0.45,
                   xend = id + 0.45,
                   y = start,
                   yend = end
                   ), color = "#555555") +
  #no change segment - season 30
  geom_segment(data=avg_viewers_s30,
               aes(x = id - 0.45,
                   xend = id + 0.45,
                   y = start,
                   yend = end
               ), color = "#555555") +
  #no change segment - season 32
  geom_segment(data=avg_viewers_s32,
               aes(x = id - 0.45,
                   xend = id + 0.45,
                   y = start,
                   yend = end
               ), color = "#555555") +
  #no change segment - season 34
  geom_segment(data=avg_viewers_s34,
               aes(x = id - 0.45,
                   xend = id + 0.45,
                   y = start,
                   yend = end
               ), color = "#555555") +
  #no change segment - season 36
  geom_segment(data=avg_viewers_s36,
               aes(x = id - 0.45,
                   xend = id + 0.45,
                   y = start,
                   yend = end
               ), color = "#555555") +
  #last bar
  geom_rect(data=avg_viewers_s38,
            aes(x=season,
                xmin = id - 0.45, 
                xmax = id + 0.45,
                ymin = 0,
                ymax = viewers_mean), fill="#577590") +
  #color scale
  scale_fill_manual(values = c("#f3722c", "#43aa8b", "#555555"), na.translate=FALSE) +
  scale_x_continuous(breaks=c(1,10,20,30,38)) +
  my_theme +
  #labels
  labs(x="SEASON\n", y="AVERAGE VIEWERS (IN MILLIONS)",
       caption="Source: survivoR package | Moriah Taylor | Twitter: moriah_taylor58 | GitHub: moriahtaylor1",
       title="Survivor TV Show: Average Viewers",
       fill=""))
  
ggsave("survivor_waterfall.png",
       plot=survivor_waterfall,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))
