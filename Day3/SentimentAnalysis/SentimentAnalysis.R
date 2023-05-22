#Library
library(syuzhet)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(gganimate)
#Import data
SentimentScotland <- read_csv("data/scotland_data.csv")
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

#Merge back with the original data set
WithSentiment<- cbind(sentiment_scores,SentimentScotland)
str(WithSentiment)
#Transorm POSIX CT in Date
WithSentiment$Date<-as.Date(as.POSIXct(WithSentiment$created_utc, 'GMT'))

WithSentiment$MonthYear <- format(as.Date(WithSentiment$Date, format="%Y-%m-%d"),"%Y-%m")

#rework the dataset to be able to plot bar plot
ToPlot <-WithSentiment[,c(1:8,16)]
LongPlot<-ToPlot %>%
  pivot_longer(
    cols = !MonthYear,
    names_to = "Sentiment",
    values_to = "count"
  )

ByMonthYear<-LongPlot%>%
  group_by(MonthYear,Sentiment)%>%
  summarise(count=round(mean(count)))

# Make a ggplot, but add frame=year: one image per year
ggplot(ByMonthYear, aes(x=Sentiment, y=count, fill=Sentiment)) +
  geom_bar(stat='identity', colour="black") +
  theme_bw() +
  coord_flip()+
  geom_text(aes(label = count, hjust=0))+
  # gganimate specific bits:
  transition_states(
    MonthYear,
    transition_length = 30,
    state_length = 30
  ) +
  ease_aes('sine-in-out')+
  labs(title = 'Month-Year : {closest_state}')
# Save at gif:
anim_save("Day3/SentimentAnalysis/outputs/288-animated-barplot-transition.gif")


