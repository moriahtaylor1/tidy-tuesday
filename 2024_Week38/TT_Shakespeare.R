# 0. load packages and write %notin% function --------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(tm)
library(showtext)
library(ragg)
library(igraph)
library(ggraph)
`%notin%` <- Negate(`%in%`)

# 1. load data --------------------------------------------------------
hamlet <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/hamlet.csv')
macbeth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/macbeth.csv')
romeo_juliet <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-17/romeo_juliet.csv')

# 2. basic cleaning of data --------------------------------------------------------
hamlet_clean <- hamlet %>% filter(character!="[stage direction]") %>% select(act, line_number, dialogue)
macbeth_clean <- macbeth %>% filter(character!="[stage direction]") %>% select(act, line_number, dialogue)
romeo_juliet_clean <- romeo_juliet %>% filter(character!="[stage direction]") %>% select(act, line_number, dialogue)

# 3. create tokens and remove stop words --------------------------------------------------------
##create shakespeare stop words set
ss_stop_words <- c("thee", "thy", "hath", "thou", "tis")
hamlet_chars <- removePunctuation(tolower(unique(hamlet$character)))
macbeth_chars <- removePunctuation(tolower(unique(macbeth$character)))
romeo_juliet_chars <- removePunctuation(tolower(unique(romeo_juliet$character)))

##create tokens, remove stop words, remove character names
hamlet_tokens <- hamlet_clean %>% 
  unnest_tokens(word, dialogue) %>% 
  anti_join(stop_words) %>% 
  filter(word %notin% ss_stop_words) %>%
  filter(str_detect(word, str_c(hamlet_chars, collapse="|"))==FALSE)
macbeth_tokens <- macbeth_clean %>% 
  unnest_tokens(word, dialogue) %>% 
  anti_join(stop_words) %>% 
  filter(word %notin% ss_stop_words) %>%
  filter(str_detect(word, str_c(macbeth_chars, collapse="|"))==FALSE)
romeo_juliet_tokens <- romeo_juliet_clean %>% 
  unnest_tokens(word, dialogue) %>% 
  anti_join(stop_words) %>% 
  filter(word %notin% ss_stop_words) %>%
  filter(str_detect(word, str_c(romeo_juliet_chars, collapse="|"))==FALSE)

# 4. get frequency of words -----------------------------------------------
hamlet_freq <- hamlet_tokens %>% 
  group_by(word) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  mutate(work="Hamlet")
macbeth_freq <- macbeth_tokens %>% 
  group_by(word) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>%
  mutate(work="Macbeth")
romeo_juliet_freq <- romeo_juliet_tokens %>% 
  group_by(word) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  mutate(work="Romeo and Juliet")

# 5. combine frequency datasets -------------------------------------------
shakespeare_freq <- rbind(hamlet_freq, macbeth_freq, romeo_juliet_freq)

# 6. get total words and join to freq df ----------------------------------
total_words <- shakespeare_freq %>% 
  group_by(work) %>%
  summarise(total=sum(n))

shakespeare_freq_totals <- shakespeare_freq %>% left_join(total_words, by="work")


# 7. analyze tf-idf -------------------------------------------------------
##get tf-idf of words
shakespeare_tf_idf <- shakespeare_freq %>%
  bind_tf_idf(word, work, n)

##plot
###add fonts
font_add(family = "cursive", "CedarvilleCursive-Regular.ttf")
font_add(family = "regular", "SignikaNegative-Regular.ttf")
showtext_auto()
###create plot theme
theme1 <- theme(
  #titles
  plot.title=element_text(family="cursive", vjust=1, hjust=0.5, size=60, color="white"),
  plot.subtitle=element_text(family="regular", vjust=1, hjust=0.5, size=45, color="white"),
  plot.caption=element_text(family="regular", size=40, color="gray70", vjust=-2, hjust=0.5),
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.2, 0.2, 0.2, 0.2, "in"),
  #axes
  axis.ticks = element_blank(),
  axis.line.x = element_line(),
  axis.line.y = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_text(family="regular", color="gray90", size=25),
  axis.text.x = element_blank(),
  #legend
  legend.position = "none",
  #strip(facet label)
  strip.background = element_rect(fill="gray90"),
  strip.text = element_text(family="regular", color="black", size=38)
)
###create color palette
ss_colors <- c("#4C7FA4", "#830E58", "#AD7392")
###create plot
ss_tf_idf_plot <- shakespeare_tf_idf %>%
  group_by(work) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(x = tf_idf, y = fct_reorder(word, tf_idf), fill = work)) +
  scale_fill_manual(values=ss_colors) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~work, ncol = 3, scales = "free") +
  labs(title="Shakespeare Dialogue",
       subtitle="15 Most Unique Words",
       caption="Moriah Taylor | #TidyTuesday 2024 Week 38 | @moriah_taylor58") +
  theme1
###save plot
ggsave("shakespeare_tf_idf_15.png",
       plot=ss_tf_idf_plot,
       device = agg_png(width = 8, height = 8, units = "in", res = 300))

# 8. visualize n-grams ----------------------------------------------------
##add play names to clean dfs
hamlet_clean2 <- hamlet_clean %>% mutate(work="Hamlet")
macbeth_clean2 <- macbeth_clean %>% mutate(work="Macbeth")
romeo_juliet_clean2 <- romeo_juliet_clean %>% mutate(work="Romeo and Juliet")
##combine dataframes
shakespeare_all <- rbind(hamlet_clean2, macbeth_clean2, romeo_juliet_clean2)
##create bigrams (n=2)
shakespeare_bigrams <- shakespeare_all %>%
  unnest_tokens(bigram, dialogue, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))
##separate bigrams
bigrams_separated <- shakespeare_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
##filter out stop words from df
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
##new bigram counts
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
##create igraph
bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()
##convert igraph to ggraph object
set.seed(2017)
bigram_final_graph <- ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(color="#C286EF") +
  geom_node_point(color="#C286EF") +
  geom_node_text(aes(label = name), vjust = 2, hjust = 0.5, color="black") +
  labs(title="Shakespeare Bigrams",
       caption="Moriah Taylor | #TidyTuesday 2024 Week 38 | @moriah_taylor58") +
  theme(
    plot.title=element_text(family="cursive", vjust=1, hjust=0.5, size=45, color="black"),
    plot.caption=element_text(family="regular", size=30, color="gray30", hjust=0.5),
    plot.background=element_rect(fill="white"),
    panel.background=element_rect(fill="white")
  )
##save bigram graph
ggsave("shakespeare_bigrams.png",
       plot=bigram_final_graph,
       device = agg_png(width = 4, height = 4, units = "in", res = 300))
