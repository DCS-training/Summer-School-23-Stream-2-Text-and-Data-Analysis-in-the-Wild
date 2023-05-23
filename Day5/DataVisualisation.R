# Animated Graphs: World Cloud and Sentiment Analysis 

# Visualise the Text Analysis world cloud



# Visualise the time series of the Sentiment Analysis -----------------
# Merge back with the original data set
WithSentiment<- cbind(sentiment_scores,SentimentScotland)
str(WithSentiment)
# Transform POSIX CT in Date
WithSentiment$Date<-as.Date(as.POSIXct(WithSentiment$created_utc, 'GMT'))

WithSentiment$MonthYear <- format(as.Date(WithSentiment$Date, format="%Y-%m-%d"),"%Y-%m")

# Rework the data set to be able to plot bar plot
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