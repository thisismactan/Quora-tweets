## Scrape answer text/data
source("code/get_tweets.R")

answer_authors <- answer_texts <- rep(NA, nrow(tweets))
urls <- tweets$url

## Scrape author and text
author_xpath <- '//*[@class="feed_item_answer_user"]'
text_xpath <- '//*[contains(@class, "AnswerPageAnswer")]'

## Loop through tweet URLs and scrape
for(i in 1:nrow(tweets)) {
  answer_url <- urls[i]
  answer <- httr::GET(answer_url) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    xml2::read_html()
  
  # Author
  answer_author <- rvest::html_nodes(answer, xpath = author_xpath) %>%
    html_text()
  if(length(answer_author) == 1) {
    answer_authors[i] <- rvest::html_nodes(answer, xpath = author_xpath) %>%
      html_text() %>%
      str_split(",") %>%
      sapply(trimws) %>%
      as.vector() %>%
      head(1)
  }
  
  # Text
  answer_text <- rvest::html_nodes(answer, xpath = text_xpath) %>%
    html_text()
  if(length(answer_text) == 1) {
    answer_texts[i] <- answer_text %>%
      str_replace_all("\\n", " ") %>%
      str_replace_all("[\\s]+", " ") %>%
      str_split("Footnotes 1|Views View") %>%
      unlist() %>%
      head(1)
  }
  
  if(i %% 20 == 0) {
    cat("Answers scraped:", i, "\n")
  }
}

## Bind to data frame
tweets <- tweets %>%
  mutate(answer_author = answer_authors,
         answer_text = answer_texts)

write.csv(tweets, file = paste0("dump/quora_feed_", lubridate::today(), ".csv"), row.names = FALSE)
