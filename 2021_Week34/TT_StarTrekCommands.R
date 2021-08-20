#load packages
library(tidyverse)
library(tidytuesdayR) #tidytuesday
library(ggplot2) #plots
library(showtext) #add font
library(ragg) #ggsave
library(stringr) #str_detect() 
library(stringi) #stri_paste()
library(tm) #text-mining tools

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 34)
computer <- tuesdata$computer

colnames(computer)[2] <- "speaking_char" 

#Idea 1: stacked bar chart with main characters and domains
#main characters: Picard, Riker, Geordi, Data, Troi, Crusher, Worf, Yar
#filter out Mrs. Troi
computer2 <- computer %>% filter(speaking_char != "Mrs. Troi")
#use string detect to identify different versions of main character's speaking roles
computer2 <- computer2 %>% mutate(main_char = case_when(
                                  str_detect(speaking_char, "Picard") ~ "Picard",
                                  str_detect(speaking_char, "Riker") ~ "Riker",
                                  str_detect(speaking_char, "Geordi") ~ "Geordi",
                                  str_detect(speaking_char, "Data") ~ "Data",
                                  str_detect(speaking_char, "Troi") ~ "Troi",
                                  str_detect(speaking_char, "Beverly") ~ "Beverly",
                                  str_detect(speaking_char, "Worf") ~ "Worf"))

#filter down to main characters (remove those not identified as main characters in case_when)
main_characters <- computer2[!is.na(computer2$main_char),]
#remove rows with NA value for domain
main_characters <- main_characters[!is.na(main_characters$domain),]
#remove rows with misspelling (do as I say and not as I do - DONT DO THIS IT'S LAZY)
main_characters <- main_characters[!(main_characters$domain=="Iot"),]

##PLOTTING##
#load fonts
font_add(family="regular", "Baloo2-Medium.ttf")
showtext_auto()

#themes
my_theme <- theme(
  #titles
  plot.title=element_text(family="regular", hjust=0.5, size=60, color="white"),
  plot.title.position = "plot",
  plot.caption=element_text(family="regular", size=35, color="#555a72", hjust=0.5),
  plot.caption.position = "plot",
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#0C1335"),
  plot.background = element_rect(fill = "#0C1335"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_text(size=35, family="regular", color="white"),
  axis.text.x = element_blank(),
  #no legend
  legend.position = "right",
  legend.title = element_blank(),
  legend.background = element_rect(fill = "#0C1335", color = "#0C1335"),
  legend.key = element_rect(fill="#0C1335", color="#0C1335"),
  legend.text = element_text(size=30, family="regular", color="white"))

#custom colors: orange, yellow-green, yellow, blue-green, blue, turquoise, purple, pink
brights <- c("#F48F05", "#8FEA0E", "#F2F113", "#0EEA9B", "#265ACF", "#05C7EE", "#9C0EE0", "#E23E7C")

prop_bar <- main_characters %>% group_by(main_char, domain) %>%
                summarise(n=n()) %>%
                ggplot(aes(x=main_char, y=n, fill=domain)) +
                    geom_bar(position="fill", stat="identity") +
                labs(title="Speech Command Categories by Character",
                     caption="Moriah Taylor | @moriah_taylor58 | #TidyTuesday Week 34") +
                scale_fill_manual(values=brights) +
                coord_flip() + my_theme

ggsave("speech_commands_bychar.png",
       plot=prop_bar,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))


#Idea 2: most frequent words
all_words <- stri_paste(computer$interaction, collapse=" ")
all_words <- tolower(all_words)
all_words <- removeNumbers(all_words)
all_words <- removePunctuation(all_words)
all_words <- removeWords(all_words, stopwords("english"))
all_words <- stripWhitespace(all_words)

split_words <- strsplit(all_words, split=" ")
words_df <- as.data.frame(split_words[1])
names(words_df) <- "word"

word_counts <- words_df %>% count(word) %>% arrange(-n)
counts_slice <- word_counts %>% slice_max(n, n=10)



