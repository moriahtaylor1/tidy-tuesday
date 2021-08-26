#load packages
library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(showtext) #add font
library(ragg) #ggsave
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2021, week = 35)
lemurs <- tuesdata$lemur_data
tax <- tuesdata$taxonomy

#filtering to adult lemur
adults <- lemurs %>% filter(age_category=="adult")
#summarise weight
weights <- adults %>% arrange(weight_date) %>% group_by(dlc_id) %>%
              summarise(name=last(name),
                        sex=last(sex),
                        taxon=last(taxon),
                        age = last(age_at_wt_y),
                        weight = last(weight_g))

#count each species
species_counts <- weights %>% count(taxon)

#filter to top 3 most common species
top3 <- weights %>% filter(taxon %in% c("MMUR", "LCAT", "CMED"))


#simple plot
top3 %>% ggplot(aes(x=weight, group=sex, fill=sex)) +
  geom_density(alpha=0.4) +
  facet_wrap(~taxon)

#find similar-sized lemurs
avg_weights <- weights %>% group_by(taxon) %>% summarise(avg_weight = mean(weight))

similar9 <- weights %>% filter(taxon %in% c("DMAD", "EALB", "ECOL", "EFLA", "EFUL", "EMAC",
                                            "ERUF", "EUL", "LCAT"))

#simple plot
similar9 %>% ggplot(aes(x=weight, group=sex, fill=sex)) +
  geom_density(alpha=0.4) +
  facet_wrap(~taxon)

#perform merge for common names
similar_weights <- merge(similar9, tax, by="taxon")

#load font
font_add(family = "regular", "Nunito-Regular.ttf")
showtext_auto()

#my theme
plot_theme <- theme(
  # titles
  plot.title = element_markdown(family = "regular", size = 55, color = "black", hjust=0.5, vjust=0.5),
  plot.subtitle = element_text(family = "regular", size = 40, color = "black", hjust = 0.5),
  plot.caption = element_text(family = "regular", size = 25, color = "black", hjust = 0.5),
  
  # panel and plot background
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#cccccc"),
  plot.background = element_rect(fill = "#cccccc"),
  
  # axis
  axis.title.x = element_blank(),
  axis.text.x = element_text(family="regular", size=16),
  axis.text.y = element_blank(),
  axis.ticks.x.bottom = element_line(color="black"),
  axis.ticks.y = element_blank(),
  axis.line = element_blank(),
  
  #multi-plot text titles
  strip.text = element_markdown(family="regular", size=35, color="#cccccc"),
  strip.background = element_rect(fill="black"),
  
  #no legend
  legend.position = "none"
)

similar_weights$common_name <- factor(similar_weights$common_name, levels=unique(similar_weights$common_name),
                                      labels=c("Aye-Aye",
                                      "White-fronted Brown Lemur",
                                      "Collared Brown Lemur",
                                      "Blue-eyed Black Lemur",
                                      "Common Brown Lemur",
                                      "Black Lemur",
                                      "Red-fronted Brown Lemur",
                                      "Hybrid",
                                      "Ring-tailed Lemur"))

#plot with common names
weights_plot <- similar_weights %>% ggplot(aes(x=weight, group=sex, fill=sex)) +
                    geom_density(alpha=0.4) + 
                    scale_fill_manual(values=c("#B76CC5", "#1072B7")) +
                    labs(title="Weight(g) of Adult <i style='color:#1072B7'>Male</i> and <i style='color:#B76CC5'>Female</i> Lemurs",
                         subtitle="Data Source: Duke Lemur Center",
                         caption="Moriah Taylor | #TidyTuesday 2021 Week 35 | @moriah_taylor58") +
                    ylab("") + xlab("") +
                    facet_wrap(~common_name) + plot_theme

ggsave("facet_weight.png",
       plot = weights_plot,
       device = agg_png(width = 9, height = 7, units = "in", res = 300))

#filled version
weights_plot <- similar_weights %>% ggplot(aes(x=weight, group=sex, fill=sex)) +
  geom_density(alpha=0.4, position="fill") + 
  scale_fill_manual(values=c("#B76CC5", "#1072B7")) +
  labs(title="Weight(g) of Adult <i style='color:#1072B7'>Male</i> and <i style='color:#B76CC5'>Female</i> Lemurs",
       subtitle="Data Source: Duke Lemur Center",
       caption="Moriah Taylor | #TidyTuesday 2021 Week 35 | @moriah_taylor58") +
  ylab("") + xlab("") +
  facet_wrap(~common_name) + plot_theme

ggsave("facet_weight_filled.png",
       plot = weights_plot,
       device = agg_png(width = 9, height = 7, units = "in", res = 300))