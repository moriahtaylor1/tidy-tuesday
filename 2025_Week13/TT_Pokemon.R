library(tidyverse)
library(gganimate)
library(ggnewscale)
library(ragg)
library(gifski)
library(showtext) #custom fonts
library(ggtext) #element_markdown

#load data
pokedex <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')
pokeapi_species <- readr::read_csv('https://raw.githubusercontent.com/phalt/pokeapi/refs/heads/master/data/v2/csv/pokemon_species.csv')

#subset to desired columns
species_sub <- pokeapi_species %>% select(id, evolves_from_species_id, evolution_chain_id)

pokemon <- pokedex %>% 
  #merge with pokedex data
  left_join(species_sub, by="id") %>%
  #create total stat
  mutate(total=hp+attack+defense+special_attack+special_defense+speed)

#divide into first 3 generations
pokemon_gen1 <- pokemon %>% filter(generation_id==1)
pokemon_gen2 <- pokemon %>% filter(generation_id==2)
pokemon_gen3 <- pokemon %>% filter(generation_id==3)

#extract primary colors from images of Shiny pokemon
pokemon_gen1$shiny_color1 <- rep("", nrow(pokemon_gen1))
pokemon_gen1$shiny_color2 <- rep("", nrow(pokemon_gen1))
for (i in 1:nrow(pokemon_gen1)){
  url <- paste0("https://img.pokemondb.net/sprites/home/shiny/2x/", pokemon_gen1[i,2][[1]], ".jpg")
  img <- try(jpeg::readJPEG(readBin(url, "raw", 1e6)))
  img_df <- data.frame(red = c(img[,,1]), green = c(img[,,2]), blue = c(img[,,3]))
  km <- kmeans(img_df, 10)
  pal <- do.call("rgb", as.data.frame(km$centers[order(-km$size),][3:4,]))
  pokemon_gen1[i,26][[1]] <- pal[[1]]
  pokemon_gen1[i,27][[1]] <- pal[[2]]
}

#get counts of evolution lines
gen1_ev_counts <- pokemon_gen1 %>% group_by(evolution_chain_id) %>% summarise(n=n())

## SLOPE CHART ----
#get evolution chains with 2 or 3 stages
gen1_ev_23stage <- gen1_ev_counts %>% filter(n==2|n==3)
pokemon_gen1_23stage <- pokemon_gen1 %>% 
  filter(evolution_chain_id %in% gen1_ev_23stage$evolution_chain_id) %>% 
  group_by(evolution_chain_id) %>%
  summarise(
    ev1_color = first(color_1),
    ev1_name = str_to_upper(first(pokemon)),
    final_name = str_to_upper(last(pokemon)),
    ev1_total = first(total),
    final_total = last(total)
  ) %>% 
  mutate(total_diff=final_total-ev1_total) %>%
  filter(total_diff!=0) %>%
  arrange(ev1_total, total_diff)

slope_totals_plot <- ggplot(data=pokemon_gen1_23stage) +
  geom_segment(aes(x=1, xend=2, y=ev1_total, yend=final_total, color=ev1_color)) +
  geom_point(aes(x=1, y=ev1_total, color=ev1_color)) +
  geom_point(aes(x=2, y=final_total, color=ev1_color)) +
  scale_color_identity() +
  xlim(c(0.75,2.25)) +
  theme(
    #background
    panel.background = element_rect(fill="black", color=NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="black", color=NA),
    #axes
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
  )

ggsave("Images/Pokemon Gen 1 Evolutions Slope.png",
       slope_totals_plot,
       device = agg_png(width = 5, height = 8, units = "in", res = 300))

##gganimate gif
#load font
font_add(family="gameboy", "Gameboy.ttf")
showtext_auto()

slope_totals_gif <- ggplot(data=pokemon_gen1_23stage) +
  geom_segment(aes(x=1, xend=2, y=ev1_total, yend=final_total, color=ev1_color)) +
  geom_point(aes(x=1, y=ev1_total, color=ev1_color)) +
  geom_point(aes(x=2, y=final_total, color=ev1_color)) +
  scale_color_identity() +
  xlim(c(0.75,2.25)) +
  labs(title="{closest_state}") +
  theme(
    #titles
    title = element_markdown(hjust=0.5, color="gray", family="gameboy", size=14, vjust=2),
    #background
    panel.background = element_rect(fill="black", color=NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="black", color=NA),
    #axes
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
  ) +
  transition_states(ev1_name, transition_length=3, state_length=1)

animate(slope_totals_gif, duration=13.5, fps=4, width=500, height=500, renderer=gifski_renderer())

anim_save("Images/Pokemon Gen 1 Evolutions Slope GIF.gif")


## DATA ART ----
#get evolution chains with 3 stages
gen1_ev_3stage <- gen1_ev_counts %>% filter(n==3)
pokemon_gen1_3stage <- pokemon_gen1 %>% 
  filter(evolution_chain_id %in% gen1_ev_3stage$evolution_chain_id) %>% 
  group_by(evolution_chain_id) %>%
  summarise(
    ev1_name = first(pokemon),
    ev1_color = first(color_1),
    ev1_total = first(total),
    ev2_color = nth(color_2, 3),
    ev2_total = nth(total, 2),
    ev3_color = last(color_f),
    ev3_total = last(total)
  ) %>%
  filter(is.na(ev3_color)==F) %>%
  mutate(n=row_number())

ev_totals_plot <- ggplot(data=pokemon_gen1_3stage) +
  geom_point(aes(x=n, y=n, size=ev3_total*10, fill=ev3_color), color="black", shape=21, stroke=0.1) +
  geom_point(aes(x=n, y=n, size=ev2_total*10, fill=ev2_color), color="black", shape=21, stroke=0.1) +
  geom_point(aes(x=n, y=n, size=ev1_total*10, fill=ev1_color), color="black", shape=21, stroke=0.1) +
  scale_fill_identity() +
  #facet_wrap(~ev1_name, scales="free") +
  theme(
    #background
    panel.background = element_rect(fill="black", color=NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="black", color=NA),
    #axes
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    #strips
    strip.text = element_blank(),
    strip.background = element_rect(fill="black"),
    #legend
    legend.position = "none",
  )
ggsave("Images/Pokemon Gen 1 Evolution Totals 2.png",
       ev_totals_plot,
       device = agg_png(width = 3, height = 5, units = "in", res = 300))

## EEVEELUTIONS ----


## SHINY ----