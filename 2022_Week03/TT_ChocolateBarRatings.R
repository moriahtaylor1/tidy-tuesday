#load packages
library(tidyverse)
library(showtext)
library(ragg)

#load data
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')


##CLEAN DATA##
#add comma to end of ingredients list (for parsing)
chocolate$ingredients <- paste0(chocolate$ingredients, ",")

#create indicator columns for ingredients
chocolate <- chocolate %>% mutate(
  beans = case_when(
    str_detect(ingredients, "B,") ~ 1,
    TRUE ~ 0
  ),
  sugar = case_when(
    str_detect(ingredients, "S,") ~ 1,
    TRUE ~ 0
  ),
  sweetener = case_when(
    str_detect(ingredients, "S\\*,") ~ 1,
    TRUE ~ 0
  ),
  cocoa = case_when(
    str_detect(ingredients, "C,") ~ 1,
    TRUE ~ 0
  ),
  vanilla = case_when(
    str_detect(ingredients, "V,") ~ 1,
    TRUE ~ 0
  ),
  lecithin = case_when(
    str_detect(ingredients, "L,") ~ 1,
    TRUE ~ 0
  ),
  salt = case_when(
    str_detect(ingredients, "Sa,") ~ 1,
    TRUE ~ 0
  )
)

#convert cocoa percentage to a number
chocolate$cocoa_percent <- as.integer(substring(chocolate$cocoa_percent, 1, 2))

##WRANGLE DATA##
#get countries with 20+ chocolate bar ratings
bean_origin_count <- chocolate %>% group_by(country_of_bean_origin) %>% count() %>% filter(n>=20)
#put countries in list
country_list <- bean_origin_count$country_of_bean_origin
#use list to subset data (exclude ratings whose country of bean origin is "Blend")
chocolate_subset <- chocolate %>% filter(country_of_bean_origin %in% country_list &
                                           country_of_bean_origin != "Blend") 
#put countries into lists that can specify region
north_amer <- c("Mexico", "U.S.A.")
central_amer <- c("Ecuador", "Nicaragua", "Belize", "Guatemala", "Costa Rica")
caribbean <- c("Dominican Republic", "Papua New Guinea", "Haiti", "Honduras", "Jamaica")
south_amer <- c("Venezuela", "Peru", "Bolivia", "Colombia", "Brazil")
africa <- c("Madagascar", "Tanzania", "Trinidad", "Ghana")
asia <- c("Vietnam", "India", "Philippines", "Indonesia")
#use lists to create Region variable using case_when and %in%
chocolate_subset <- chocolate_subset %>% mutate(Region = case_when(
  country_of_bean_origin %in% north_amer ~ "North America",
  country_of_bean_origin %in% central_amer ~ "Central America",
  country_of_bean_origin %in% carribean ~ "Caribbean",
  country_of_bean_origin %in% south_amer ~ "South America",
  country_of_bean_origin %in% africa ~ "Africa",
  country_of_bean_origin %in% asia ~ "Asia"
))

#get counts of each ingredient for each region
region_ingredient_counts <- chocolate_subset %>% group_by(Region) %>% 
  summarise(Beans = round(sum(beans)/n(),4)*100,
            Cocoa = round(sum(cocoa)/n(),4)*100,
            Sweetener = round(sum(sweetener)/n(),4)*100,
            Lecithin = round(sum(lecithin)/n(),4)*100,
            Salt = round(sum(salt)/n(),4)*100,
            Sugar = round(sum(sugar)/n(),4)*100)

#pivot counts
region_ingredients_pivot <- region_ingredient_counts %>% pivot_longer(
  cols = -c("Region"),
  names_to = "Ingredient",
  values_to = "Percentage"
)

#load fonts
font_add(family="regular", "Nunito-Bold.ttf")
font_add(family="bold", "ConcertOne-Regular.ttf")
showtext_auto()

#create plot
chocolate_plot <- region_ingredients_pivot %>% ggplot(aes(x=Percentage, y=Region)) +
  geom_col(fill="#432206", width=0.5) +
  geom_text(aes(label=paste0(Percentage, "%")), hjust=1.15, color="#D1C6AA", family="regular", size=8) +
  scale_y_discrete(limit=rev) +
  coord_cartesian(expand=FALSE) +
  facet_wrap(~Ingredient, nrow=2, scales = "free_x") +
  labs(title = "Chocolate Bar Ingredients",
       subtitle = "Percentage of chocolate bars in each region that list the ingredient",
       caption = "Moriah Taylor | @moriah_taylor58 | #TidyTuesday") +
  theme(
    #title
    plot.title = element_text(color="#643E1C", hjust=0.5, family="bold", size=45),
    plot.subtitle = element_text(color="#643E1C", hjust=0.5, family="regular", size=30),
    plot.caption = element_text(color="#7b6451", family="regular", hjust=0.5, size=20),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    #background
    plot.background = element_rect(fill="#D1C6AA"),
    panel.background = element_rect(fill="#D1C6AA"),
    panel.grid = element_blank(),
    
    #axes
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="#432206", family="regular", size=20),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    #strip
    strip.background = element_rect(fill="#D1C6AA"),
    strip.text = element_text(color="black", family="bold", size=35)
  )

#save plot
ggsave("Chocolate_Ingredients.png",
       chocolate_plot,
       device = agg_png(width = 6, height = 6, units = "in", res = 300))
