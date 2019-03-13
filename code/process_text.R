## Process text using tidy text methods
source("code/scrape_answers.R")

## Tokenize and stem
quora_tokens <- unnest_tokens(tweets, word, answer_text, token = "words") %>%
  filter(!(word %in% tm::stopwords())) %>%
  mutate(stem = wordStem(word)) %>%
  group_by(timestamp, text, url, answer_author, stem) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(!is.na(stem))

## Cast to DTM
quora_dtm.tf <- cast_dtm(quora_tokens, document = url, term = stem, value = n)
quora_dtm.tfidf <- cast_dtm(quora_tokens, document = url, term = stem, value = n, weighting = tm::weightTfIdf)
