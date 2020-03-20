require(pdftools)
require(ggthemes)
require(tidyverse)

Sys.setlocale("LC_ALL", "UTF-8")
setwd("C:/Users/gmv/Dropbox/MIT/2020 1/15.539 - Acct Seminar")

# Importng
text <- pdf_text("TAR-2020-0129.pdf")
paper_analysis <- as.data.frame(text)

# Cleaning symbols
paper_analysis$text <- gsub("\\{","",paper_analysis$text) 
paper_analysis$text <- gsub("\\}","",paper_analysis$text) 
paper_analysis$text <- gsub("\\\\","",paper_analysis$text) 
paper_analysis$text <- gsub("backslash","",paper_analysis$text) 

# Unnest
doc_words <- paper_analysis %>% 
  unnest_tokens(word, text) %>% 
  ungroup()

# What are the stop words:
mystopwords <- data_frame(word = c("in", "on", "and", "to", "at", "for", "there","$","of","the", "fumble", "do", "does", "with", "that", "is", "as", "are", "a", "0", "0.0", "0.00", "0.000", "i", "1", "or", "this", "if", "2017", "2016", "2018", "2019", "2020"))
doc_words <- anti_join(doc_words, mystopwords, by = "word")
doc_words$count <- rep(1,nrow(doc_words)) # make new column

# Group by number of words
total_words <- doc_words %>% 
  group_by(word) %>% 
  summarize(total = sum(count))

# Counting total number of words
tnw <- sum(total_words$total)
total_words$tnw <- rep(tnw,nrow(total_words)) # make new column
total_words$ratio <- total_words$total/total_words$tnw

# Plotting
total_words %>% 
  top_n(50) %>%
  ggplot(aes(word, total)) +
  geom_col() +
  labs(x = NULL, y = "Total word usage") +
  coord_flip() # +
  # theme_economist(base_size = 11, base_family = "sans",
    #                    gray_bg = TRUE, horizontal = TRUE)

# Generating bigrams
bigrams <- doc_words %>%
  unnest_tokens(bigram, word, token = "ngrams", n = 2)

# Counting bigrams:
bigram_count <- bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtering bigrams
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Counting
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")


# Trigrams:
trigrams <- total_words %>%
  unnest_tokens(trigram, word, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


bigram_graph <- bigram_counts %>%
  filter(n > 15) %>%
  graph_from_data_frame()
