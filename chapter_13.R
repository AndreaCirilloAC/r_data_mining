# text data mining
install.packages("tidytext")
library(tidytext)
library(pdftools)
library(dplyr)
library(janeaustenr)
library(tm)
# import companies credit bureau information, stored in PDF format
file_list <- list.files("data")
pdf_list <- file_list[grepl(".pdf",file_list)]

pdf_text("data/Carnival Coffee.pdf") %>% 
  strsplit("\n")->document_name

pdf_list <- data.frame((document_name),rep(substitute(document_name),length(document_name)))

VCorpus(document_name)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

# dataframe from PDF


# topic analysis on notes 

# descriptive: most common industries

# network analysis on  shareholders
