# .......................................
# .......................................
# This script runs a text analysis to produce the 
# word clouds with the open responses indicating the 
# reasons for selecting or rejecting the maize concepts 
# presented in the tricot incomplete blocks
library("tidyverse")
library("magrittr")
library("tidytext")
library("tm")
library("wordcloud")
library("igraph")
library("ggraph")
library("patchwork")

# .......................................
# .......................................
# .......................................
# Organize the data ####
# read the data from both countries. 
# this is a csv file previously treated from the original 
# .json file with the farmers' responses on the preferred concepts
# put them together
dat = read.csv("data/tricot-data-ethiopiagrass.csv")
words = read.csv("data/agri-words-library.csv")

# .......................................
# .......................................
# Data for text analysis ####
dat$word = gsub("[[:punct:]]", "", dat$bestwhy)

dat$word = tolower(dat$word)

dat$word = gsub("[^[:alnum:]]", " ", dat$word)
dat$word = gsub("[[:digit:]]+", " ", dat$word)

dat$word

dat$id = 1:nrow(dat)

for (i in seq_along(words$input)) {
  dat$word = gsub(words$input[i], words$standardised[i], dat$word)
}

dat$word = removeWords(dat$word, stopwords("en"))
dat$preference = "best"

# # words with less than 2 characters 
keep = nchar(dat$word) > 2 & dat$word != ""
 
dat$word[!keep] = ""

d = 
  dat %>% 
  group_by(preference) %>% 
  unnest_tokens("word", word) 


d

top = 20

dat$bestwhy[grep("good", dat$word)]

# Term Frequency ####
dat_idf1 = d %>% 
  count(word) %>% 
  top_n(top, n) %>% 
  bind_tf_idf(word, preference, n) %>% 
  arrange(desc(tf))

dat_idf1[1:20,]

p = 
  dat_idf1 %>%
  group_by(preference) %>% 
  top_n(top, tf_idf) %>%
  arrange(desc(tf)) %>% 
  slice(1:top) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf)) %>% 
  ggplot(aes(y = word, x = tf, fill = preference)) +  
  geom_col(show.legend = FALSE) + 
  theme_classic() +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Term", 
       x = "TF-IDF",
       title = "") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(color = "grey20", size = 15),
        legend.title = element_blank(),
        strip.background.x = element_blank(),
        strip.placement = "outside")

p

ggsave("output/tf-idf.pdf", plot = p, width = 15, height = 15, units = "cm")


d = 
  dat_idf1 %>%
  group_by(preference) %>% 
  top_n(top, tf_idf) %>%
  arrange(desc(tf)) %>% 
  slice(1:top)

write.csv(d, "output/tf-idf-top20.csv", row.names = FALSE)

# matching will be done by hand, with this file and the 
# data/tricot-data-ethiopiagrass.csv

