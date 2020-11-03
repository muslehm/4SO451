library(tidyverse) 
library (rtweet)

wd <- getwd()
setwd(wd)

#create an access token 
#use your developer account to fill in the values. Go to "Details" in your app. Click on the "keys and tokens" button, copy and paste the values to code below:

twitter_token <- create_token(
  app = "",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "")

#create a query
#check https://developer.twitter.com/en/docs/labs/filtered-stream/guides/search-queries for more info on how to build queries that correspond you needs
query <- "#tal3at OR #طالعات has:mentions"

#full-archive search (up to 5000 tweets per month)
tweets_s <- search_fullarchive (query, n=5000, env_name = "", fromDate = "201909010000", toDate = "201911302359", token = twitter_token)


all_tweets <- rbind(tweets, tweets_s)
my.tweets <- data.frame(lapply(all_tweets, as.character), stringsAsFactors=FALSE)

#df <- read.csv('tal3at.csv', header = TRUE)
#tal3at <- rbind(df, tweets1)
write.csv(my.tweets,'oct_tweets.csv', row.names = FALSE)
new_Tweets <- 
df <- read.csv('../Data/tal3at_Sep25Oct7(2).csv', header = TRUE)
df1 <- rbind(df, tweets1, tweets2)
df1 <- unique(df1)
my.tweets <- data.frame(lapply(df1, as.character), stringsAsFactors=FALSE)
write.csv(my.tweets,'tal3at_Sep25Oct7(2)_f.csv', row.names = FALSE)