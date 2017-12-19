library(stringr)
library(dplyr)
library(tidyr)
library(devtools)
library(tidytext)
library(scales)
library(ggplot2)

text.df <- readRDS(file = "tweets_df1.rds")
text.df2 <- readRDS(file = "tweets_df2.rds")
text.df3 <- readRDS(file = "tweets_df3.rds")
text.df4 <- readRDS(file = "tweets_df4.rds")
text.df5 <- readRDS(file = "tweets_df5.rds")
text.df6 <- readRDS(file = "tweets_df6.rds")
text.df7 <- readRDS(file = "tweets_df7.rds")

total1 <- bind_rows(text.df, text.df2, text.df3, text.df4, text.df5, text.df6, text.df7)
## Data Cleaning
total.v1 <- total1 %>% 
  separate(col = text, into = c("text","html"), sep = 'https', remove = T) 
total1.df<- total.v1[,-3]
total1.unnest <- unnest_tokens(tbl=total1.df, output=words, input=text)
total1.unnest <- as_tibble(total1.unnest)
total1.unnest <- total1.unnest %>%
  anti_join(stop_words,by=c("words"="word")) %>%
  count(words, sort = T) %>%
  mutate(words = reorder(words, n))

# add date to data for plots and shiny app
total <- bind_rows(text.df.tidy, text.df2.tidy, text.df3.tidy, text.df4.tidy, text.df5.tidy, text.df6.tidy, text.df7.tidy)
total1 <- mutate(total, date = "first_week")
total2 <- mutate(text.df7.tidy, date = "last_week")

# barplot and plotly
# reduce data size to better fit the plots
total1_reduced <- filter(total1.unnest, n>780)
total2_reduced <- filter(total2, n >780)
saveRDS(total1_reduced, file = "total1_reduced.rds")
saveRDS(total2_reduced, file = "total2_reduced.rds")

## clean data for sentiment analysis and word clouds
total1.sentiment <- total1.date %>%
  inner_join(get_sentiments("bing"),by=c("words"="word")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
saveRDS(total1.sentiment, file = "total1.sentiment.rds")

total1.sentiment2 <- total1.unnest %>%
  inner_join(get_sentiments("bing"), by=c("words"="word"))
saveRDS(total1.sentiment2, file = "total1.sentiment2.rds")