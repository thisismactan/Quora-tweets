## LDA with varying numbers of topics



#### 10 topics ####
set.seed(2019)
quora_LDA.10 <- LDA(quora_dtm.tf, k = 10)

terms_LDA.10 <- tidy(quora_LDA.10, matrix = "beta") %>%
  filter(grepl("[[:alnum:]]", term))

terms_LDA.10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  facet_wrap(~topic, scales = "free") +
  geom_col(show.legend = FALSE) +
  coord_flip()
  
