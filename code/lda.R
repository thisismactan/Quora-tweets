## LDA with varying numbers of topics
source("code/process_text.R")

#### 6 topics ####
set.seed(2019)
quora_LDA.6 <- LDA(quora_dtm.tf, k = 6)

terms_LDA.6 <- tidy(quora_LDA.6, matrix = "beta") %>%
  filter(grepl("[[:alnum:]]", term), !(term %in% c("t", "u"))) 

top_terms_LDA.6 <- terms_LDA.6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, beta) %>%
  mutate(plot_order = row_number())

ggplot(top_terms_LDA.6, aes(x = plot_order, y = beta, fill = factor(topic))) +
  facet_wrap(~topic, scales = "free") +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_continuous(breaks = top_terms_LDA.6$plot_order, labels = top_terms_LDA.6$term, expand = c(0,0))

topics_LDA.6 <- tidy(quora_LDA.10, matrix = "gamma")

topics_LDA.6 %>%
  mutate(gamma = round(gamma, 4)) %>%
  spread(topic, gamma) %>%
  View()

#### 12 topics ####
set.seed(2019)
quora_LDA.12 <- LDA(quora_dtm.tf, k = 12)

terms_LDA.12 <- tidy(quora_LDA.12, matrix = "beta") %>%
  filter(grepl("[[:alnum:]]", term), !(term %in% c("t", "u"))) 

top_terms_LDA.12 <- terms_LDA.12 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, beta) %>%
  mutate(plot_order = row_number())

ggplot(top_terms_LDA.12, aes(x = plot_order, y = beta, fill = factor(topic))) +
  facet_wrap(~topic, scales = "free") +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_continuous(breaks = top_terms_LDA.12$plot_order, labels = top_terms_LDA.12$term, expand = c(0,0))
  
topics_LDA.12 <- tidy(quora_LDA.12, matrix = "gamma")

topics_LDA.12 %>%
  mutate(gamma = round(gamma, 4)) %>%
  spread(topic, gamma) %>%
  View()

#### 24 topics ####
set.seed(2019)
quora_LDA.24 <- LDA(quora_dtm.tf, k = 24)

terms_LDA.24 <- tidy(quora_LDA.24, matrix = "beta") %>%
  filter(grepl("[[:alnum:]]", term), !(term %in% c("t", "u"))) 

top_terms_LDA.24 <- terms_LDA.24 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, beta) %>%
  mutate(plot_order = row_number())

ggplot(top_terms_LDA.24, aes(x = plot_order, y = beta, fill = factor(topic))) +
  facet_wrap(~topic, scales = "free") +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_continuous(breaks = top_terms_LDA.24$plot_order, labels = top_terms_LDA.24$term, expand = c(0,0))

topics_LDA.24 <- tidy(quora_LDA.24, matrix = "gamma")

topics_LDA.24 %>%
  mutate(gamma = round(gamma, 4)) %>%
  spread(topic, gamma) %>%
  View()

#### 48 topics ####
set.seed(2019)
quora_LDA.48 <- LDA(quora_dtm.tf, k = 48)

terms_LDA.48 <- tidy(quora_LDA.48, matrix = "beta") %>%
  filter(grepl("[[:alnum:]]", term), !(term %in% c("t", "u"))) 

top_terms_LDA.48 <- terms_LDA.48 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, beta) %>%
  mutate(plot_order = row_number())

ggplot(top_terms_LDA.48, aes(x = plot_order, y = beta, fill = factor(topic))) +
  facet_wrap(~topic, scales = "free") +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_continuous(breaks = top_terms_LDA.48$plot_order, labels = top_terms_LDA.48$term, expand = c(0,0))

topics_LDA.48 <- tidy(quora_LDA.48, matrix = "gamma")

topics_LDA.48 %>%
  mutate(gamma = round(gamma, 4)) %>%
  spread(topic, gamma) %>%
  View()