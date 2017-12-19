library(rsconnect)
library(devtools)
library(twitteR)
library(tidytext)
library(dplyr)
library(ggplot2)

api_key <- 	"guJGKWz5DSvjaNbN0ZYjxIrZ0"
api_secret <- "ts7BBudBzM0k1L0lK7ALJA43cD1MCVI1ByRaf2fTCYx6gWc5zu"
access_token <- "2438046235-FkN0WL4fTdYZbJTvQGU5FgBh2ADAVgPPMiNK4jU"
access_token_secret <- "MboGN3eQohuOfJ4Y9JTIRl6jKPCfsoLuQZxo5L2lXydv6"



setup_twitter_oauth(api_key, 
                    api_secret, 
                    access_token, 
                    access_token_secret)

tweets <- searchTwitter('taxbill', since = "2017-12-01", until = "2017-12-02", n=1000)
tweets.df <- twListToDF(tweets)
write.csv(tweets.df, "tweets1202")
tweets2 <- searchTwitter('taxbill', since = "2017-12-02", until = "2017-12-03", n=1000)
tweets.df2 <- twListToDF(tweets2)
write.csv(tweets.df, "tweets1203")
tweets3 <- searchTwitter('taxbill', since = "2017-12-03", until = "2017-12-04", n=1000)
tweets.df3 <- twListToDF(tweets3)
tweets4 <- searchTwitter('taxbill', since = "2017-12-04", until = "2017-12-05", n=1000)
tweets.df4 <- twListToDF(tweets4)
tweets5 <- searchTwitter('taxbill', since = "2017-12-05", until = "2017-12-06", n=1000)
tweets.df5 <- twListToDF(tweets5)
tweets6 <- searchTwitter('taxbill', since = "2017-12-06", until = "2017-12-07", n=1000)
tweets.df6 <- twListToDF(tweets6)
tweets7 <- searchTwitter('taxbill', since = "2017-12-07", until = "2017-12-08", n=1000)
tweets.df7 <- twListToDF(tweets7)

tweets8 <- searchTwitter('taxbill', since = "2017-12-08", until = "2017-12-18", n=10000)
tweets.df8 <- twListToDF(tweets8)

text.df <- tweets.df %>% 
  select(screenName, text, favoriteCount, retweetCount)
text.df2 <- tweets.df2 %>%
  select(screenName, text, favoriteCount, retweetCount)
text.df3 <- tweets.df3 %>%
  select(screenName, text, favoriteCount, retweetCount)
text.df4 <- tweets.df4 %>%
  select(screenName, text, favoriteCount, retweetCount)
text.df5 <- tweets.df5 %>%
  select(screenName, text, favoriteCount, retweetCount)
text.df7 <- tweets.df7 %>%
  select(screenName, text, favoriteCount, retweetCount)
text.df8 <- tweets.df8 %>%
  select(screenName, text, favoriteCount, retweetCount)


saveRDS(text.df, file = "tweets_df1.rds")
saveRDS(text.df2, file = "tweets_df2.rds")
saveRDS(text.df3, file = "tweets_df3.rds")
saveRDS(text.df4, file = "tweets_df4.rds")
saveRDS(text.df5, file = "tweets_df5.rds")
saveRDS(text.df7, file = "tweets_df6.rds")
saveRDS(text.df8, file = "tweets_df7.rds")

tweets9 <- searchTwitter('taxbill', since = "2017-12-08", until = "2017-12-18", n=20000)
tweets.df9 <- twListToDF(tweets9)
text.df8.map <- tweets.df9 %>%
  select(longitude, latitude)
text.df8.map <- na.omit(text.df8.map)
text.df8.map$longitude <- as.numeric(text.df8.map$longitude)
text.df8.map$latitude <- as.numeric(text.df8.map$latitude)
saveRDS(text.df8.map, file = "text.df8.map.rds")

