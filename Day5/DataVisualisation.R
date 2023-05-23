# Animated Graphs: World Frequency and Sentiment Analysis #############

# Visualise Word Frequency=========================
library(tidyverse)
library(gganimate)
library(gifski)
library(av)
library(textstem)

# Most Recurrent words each month ==============
#Create a new column that will have Month and year -------------------
uk_data_clean$MonthYear <- format(as.Date(uk_data_clean$dates, format="%Y-%m-%d"),"%Y-%m")
#Group articles by month -------------------
uk_data_clean_G<-uk_data_clean%>%
  group_by(MonthYear)%>%
  summarize(clean_text=paste0(clean_text, collapse="\r\n"), titles=paste0(titles,collapse="\r\n"))
# Preprocess the text data -------------------
processed_df <- uk_data_clean_G %>%
  mutate(clean_text = tolower(clean_text)) %>%
  unnest_tokens(word, clean_text) %>%
  mutate(word = lemmatize_words(word))%>%
  filter(!word %in% stop_words$word)
# Filter for the top recurring keywords overall -------------------
top_keywords <- processed_df %>%
  count(word) %>%
  top_n(20, n) %>%
  arrange(desc(n))
# Count the keywords by date -------------------
keyword_by_date <- processed_df %>%
  count(MonthYear, word) %>%
  inner_join(top_keywords, by = "word")%>%
  mutate(Ntotal=n.y, Nmonth=n.x)%>%
  arrange(desc(Ntotal))

# Create the stacked bar chart  -------------------
animation<-ggplot(keyword_by_date, aes(x = word, y = Nmonth, fill = word)) +
  geom_col(colour="black") +
  theme_minimal() +
  coord_flip()+
  theme(legend.position = "none",axis.title=element_text(size=16,face="bold"), axis.text = element_text(size=12),plot.title = element_text(size=22, face="bold")) +
  labs(x = "Keyword", y = "Count", fill = "Keyword") +
  scale_fill_viridis_d(option = "plasma", direction = -1)+
  transition_states(
    states = MonthYear,
    transition_length = 2,
    state_length = 1) +
  enter_fade() +
  exit_fade()+
  labs(title = 'Month-Year : {closest_state}')
# Animate and Save 
animate(animation, height = 1000, width =800,fps = 1.5)
anim_save("Day5/outputs/keyword_animation.gif")



# Visualise the time series of the Sentiment Analysis========================
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
animationSent<-ggplot(ByMonthYear, aes(x=Sentiment, y=count, fill=Sentiment)) +
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

# Animate and Save 
animate(animationSent, height = 1000, width =800,fps = 1.5)
anim_save("Day5/outputs/Sentiment_animation.gif")