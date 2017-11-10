# text data mining
install.packages("tidytext")
library(tidytext)
library(pdftools)
library(dplyr)
library(janeaustenr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(igraph)
library(ggraph)
# import companies credit bureau information, stored in PDF format
file_list <- list.files("data")
pdf_list <- file_list[grepl(".pdf",file_list)]

pdf_text(paste("data/", pdf_list[1],sep = "")) %>% 
  strsplit("\n")-> document_text

# dataframe from PDF


data.frame(gsub(x =pdf_list[1],pattern = ".pdf", replacement = ""), document_text, stringsAsFactors = FALSE) -> document

document <- as_tibble(document)
colnames(document) <- c("company", "text")
document %>% 
  filter(!grepl(c("date of foundation"),text)) %>% 
  filter(!grepl(c( "industry"),text)) %>% 
  filter(!grepl(c( "share holders"),text))-> comments

information <- document %>% 
  filter(grepl(("date of foundation+"),text)|grepl(( "industry+"),text)|grepl(( "share holders+"),text)) 


# ON COMMENTS  

comments %>% 
  group_by(company) %>%
  mutate(line_number = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word,text)-> comments_tidy

  
#sentiment analysis  
  
lexicon <- get_sentiments("bing")

comments_tidy %>% 
  inner_join(lexicon) %>% 
  count(company, index = line_number %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

comments_tidy %>% 
  inner_join(lexicon) %>% 
  count(company, index = line_number %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = -negative)-> comments_sentiment

ggplot(comments_sentiment, aes(index, sentiment, fill = company)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~company, ncol = 2, scales = "free_x")

# word clouds


comments_tidy %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# ngram analysis 
comments %>% 
  group_by(company) %>%
  mutate(line_number = row_number()) %>% 
  ungroup() %>% 
unnest_tokens(bigram, text, token = "ngrams", n = 2) -> bigram_comments

bigram_comments %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


## ON INFORMATION

# descriptive: most common industries

# network analysis on  shareholders
