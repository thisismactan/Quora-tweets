## Get tweets
source("code/library.R")
source("api_info.R") # Create an "api_info.R" file; use to create token object "token"

quora_tweets <- get_timeline("Quora", n = 3200)

tweets <- quora_tweets %>%
  dplyr::select(timestamp = created_at, text, url = urls_expanded_url) %>%
  slice(which(quora_tweets$urls_url %>% sapply(length) == 1)) %>%
  mutate(url = unlist(url)) %>%
  filter(grepl("\\/answer\\/", url))
