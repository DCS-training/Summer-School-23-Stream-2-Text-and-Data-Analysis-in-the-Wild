#Library
library(syuzhet)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(gganimate)
#Import data
SentimentScotland <- read_csv("day3/SentimentAnalysis/data/scotland_data.csv")
#Import data
SentimentScotland <- read_csv("day3/SentimentAnalysis/data/scotland_data.csv")

#Export text
SentimentScotlandText <-SentimentScotland$selftext
sentiment_scores <- get_nrc_sentiment(SentimentScotlandText, lang="english")
head(sentiment_scores)
summary(sentiment_scores)
barplot(
  colSums(prop.table(sentiment_scores[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Scotland Sentiment",
  sub = "Analysis by Me",
  xlab="emotions", ylab = NULL)




