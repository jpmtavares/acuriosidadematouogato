##############################################################
#                                                            #
#               Gutenberg project War&Peace                  #
#                                                            #
##############################################################
#https://ropensci.org/tutorials/gutenbergr_tutorial/

library(gutenbergr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)

#_______________________________________________________
# filter gutenberg by Tolstoy
#_______________________________________________________
tolstoy<-gutenberg_works(str_detect(author, "Tolstoy"))
#_______________________________________________________
# get War and Peace with books, chapters and page numbers
#_______________________________________________________
warpeace<-gutenberg_download(2600) %>%
  mutate(book = cumsum(str_detect(text, regex("^book .*:",
                                              ignore_case = TRUE))),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE))),
         linenumber = row_number())

#_______________________________________________________
# get useful words
#_______________________________________________________
data(stop_words)
words <- warpeace %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_replace(word, "\\’", "\\'")) %>%
  anti_join(stop_words) %>% #stop words are words that are not useful for analysis, typically extremely common words
  mutate(word = str_replace(word, "\\'s", ""))
#_______________________________________________________
# sentiment analysis
#_______________________________________________________
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
# sentiment
sentiment <- words %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(book <= 10 & book != 0) %>%
  group_by(sentiment) %>%
  count

# sentiment by book
sentiment_book <- words %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(book, sentiment) %>%
  count %>%
  group_by(book) %>%
  mutate(color_book = factor(ifelse(n==max(n), 1, ifelse(n==min(n), 3, 2))),
         perc = (n/sum(n))*100) %>%
  group_by(sentiment) %>%
  mutate(color_sentiment = factor(ifelse(perc==max(perc), 1, ifelse(perc==min(perc), 3, 2)))) %>%
  filter(book <= 10 & book != 0)

sentiment_book$sentiment<-factor(sentiment_book$sentiment, levels=unique(sort(sentiment_book$sentiment, decreasing = TRUE)))

# sentiment by chapter
sentiment_chapter <- words %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(chapter, sentiment) %>%
  count %>%
  group_by(chapter) %>%
  mutate(color_chapter = factor(ifelse(n==max(n), 1, ifelse(n==min(n), 3, 2))),
         perc = (n/sum(n))*100) %>%
  group_by(sentiment) %>%
  mutate(color_sentiment = factor(ifelse(perc==max(perc), 1, ifelse(perc==min(perc), 3, 2)))) %>%
  filter(chapter <= 229 & chapter != 0) %>%
  left_join(words[,c(2:3)]) %>%
  unique

#filter by positive and negative
pn <- sentiment_chapter %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  mutate(n = ifelse(sentiment=="positive", n, -n)) %>%
  mutate(color_chapter = ifelse(sentiment == "negative" & color_chapter == 1, 3, color_chapter)) %>%
  filter(book <= 10 & book != 0)

# predominant feelings
max <- sentiment_chapter %>%
  filter(color_chapter == 1) %>%
  count(sentiment)
min <- sentiment_chapter %>%
  filter(color_chapter == 3) %>%
  count(sentiment)

#_______________________________________________________
# tag words
#_______________________________________________________
#unipostag_types <- c("ADJ" = "adjective", "ADP" = "adposition", "ADV" = "adverb", "AUX" = "auxiliary",
#                     "CONJ" = "coordinating conjunction", "DET" = "determiner", "INTJ" = "interjection",
#                     "NOUN" = "noun", "NUM" = "numeral", "PART" = "particle", "PRON" = "pronoun",
#                     "PROPN" = "proper noun", "PUNCT" = "punctuation", "SCONJ" = "subordinating conjunction",
#                     "SYM" = "symbol", "VERB" = "verb", "X" = "other")
#unipostagger <- rdr_model(language = "English", annotation = "UniversalPOS")

#words_tags <- rdr_pos(unipostagger, words$word)
#words_tags$pos <- unipostag_types[words_tags$pos]

#words_t<-inner_join(words, words_tags[,c(3:4)], by = c("word" = "token")) %>%
#  unique()
#_______________________________________________________
# count words
#_______________________________________________________
word_counts <- words %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  slice(1:10) %>%
  filter(book <= 10 & book != 0) %>%
  mutate(word = reorder(word, n))

ggplot(word_counts, aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  facet_grid(. ~ book)

#_______________________________________________________
# ggplot
#_______________________________________________________
ggplot(sentiment, aes(sentiment, n, fill = sentiment)) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c('#ea6767', '#ffaacc', '#c8b7be', '#575761', '#fce376', 'tan1', 
                             '#5fd3bc', 'slategray2', '#6671a3', '#afafe9')) + #'#ea6767')) +
  ylab("number of words") +
  theme(legend.position="none",
        strip.background = element_rect(fill = "tan1"),
        strip.text.x = element_text(size = 12, colour = "white", face = "bold"), 
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 12))


ggplot(sentiment_book, aes(sentiment, n, fill = color_book)) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c('slategray2', '#575761', '#575761')) +#'#ea6767')) +
  ylab("number of words")+
  coord_flip() + 
  facet_wrap( ~ book, ncol = 5) +
  theme(legend.position="none",
        strip.background = element_rect(fill = "tan1"),
        strip.text.x = element_text(size = 12, colour = "white", face = "bold"), 
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 12))

ggplot(pn, aes(chapter, n, fill = color_chapter)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c('#5fd3bc', '#575761', 'tan1')) +#'#ea6767')) +
  scale_x_continuous(breaks = seq(0, 230, 10)) +
  ylab("number of words") + #g5926
  facet_wrap( ~ book, ncol = 5, scales='free') +
  theme(legend.position="none",
        strip.background = element_rect(fill = "tan1"),
        strip.text.x = element_text(size = 12, colour = "white", face = "bold"), 
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))
