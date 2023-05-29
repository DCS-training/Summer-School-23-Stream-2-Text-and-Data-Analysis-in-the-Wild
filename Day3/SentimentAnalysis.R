#Library
library(syuzhet)
library(ggplot2)
library(vader)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(gganimate)

#Research questions:
#1. What trends (if any) can we observe about sentiment about the CoL in each dataset?
#2. Based on the dataset only, is the CoL crisis getting better or worse in 2023? How do things differ in Scotland vs. the UK?

#PART 1: Overall sentiment (Scotland vs. UK)
#Import data:
SentimentScotland <- read_csv("day3/SentimentAnalysis/data/scotland_data.csv")
SentimentUK <- read_csv("day3/SentimentAnalysis/data/scotland_data.csv")

#Calculate sentiment scores with VADER:
scot_scores <- SentimentScotland$selftext %>% lapply(get_vader)
uk_scores <- SentimentUK$selftext %>% lapply(get_vader)
#Add compound sentiment score to the dataframe:
SentimentScotland <- SentimentScotland %>% mutate(
  compound = scot_scores %>% sapply(function(v) { as.numeric(v["compound"]) }))
SentimentUK <- SentimentUK %>% mutate(
  compound = uk_scores %>% sapply(function(v) { as.numeric(v["compound"]) }))

#Statistical overview: mean score and range of scores for each dataset
mean(SentimentUK$compound)
range(SentimentUK$compound)

mean(SentimentScotland$compound)
range(SentimentScotland$compound)

#Print the titles of the top negative and top positive posts for each dataset
SentimentScotland<-SentimentScotland[order(SentimentScotland$compound, decreasing = TRUE), ]
SentimentUK<-SentimentUK[order(SentimentUK$compound, decreasing = TRUE), ]

head(SentimentScotland$title, 1)
tail(SentimentScotland$title, 1)

head(SentimentUK$title, 1)
tail(SentimentUK$title, 1)

#Plot the sentiment scores by date:
#Standardsise date format + remove timestamp
SentimentScotland$created_utc <- as.POSIXct(SentimentScotland$created_utc, format = "%Y-%m-%d %H:%M:%S")
SentimentUK$created_utc <- as.POSIXct(SentimentUK$created_utc, format = "%Y-%m-%d %H:%M:%S")
#Create plots:
#a. Scotland
ggplot(SentimentScotland, aes(x = created_utc, y = compound)) +
  geom_line(color = "black") +
  geom_point(color = "black", size = 2) +
  scale_fill_gradientn(colours = rainbow(length(unique(SentimentScotland$compound)))) +
  labs(title='r/Scotland VADER Compound Sentiment', x = "Post Date", y = "Sentiment Score") +
  theme_minimal()
#b. UK
ggplot(SentimentUK, aes(x = created_utc, y = compound)) +
  geom_line(color = "black") +
  geom_point(color = "black", size = 2) +
  scale_fill_gradientn(colours = rainbow(length(unique(SentimentScotland$compound)))) +
  labs(title='r/UK VADER Compound Sentiment', x = "Post Date", y = "Sentiment Score") +
  theme_minimal() +
  scale_x_datetime(labels = date_format("%b %Y"))


#Discuss the stats, top tiles, and plots with your table. What do/don't they show us? What might the pros and cons of our datasets be?


#PART 2: Methods for filtering the data 
#Subset the dataframe by posts from a certain date (year = 2023, data=Scotland):
df_23 <- SentimentScotland[year(SentimentScotland$created_utc) %in% 2023, ]
#Plot the 2023 posts:
ggplot(df_23, aes(x = created_utc, y = compound)) +
  geom_line(color = "black") +
  geom_point(color = "black", size = 2) +
  scale_fill_gradientn(colours = rainbow(length(unique(SentimentScotland$compound)))) +
  labs(title= 'r/Scotland Posts on the CoL (2023)', x = "Month", y = "Sentiment Score") +
  theme_minimal()
#Filter by sentiment score (compound <= -0.5, data=UK):
#subset the dataframe and standardise the date:
UK_negative <- subset(SentimentUK, compound <= -0.5)
UK_negative$created_utc <- as.Date(UK_negative$created_utc, format = "%Y-%m-%d")
#create a line plot:
ggplot(UK_negative, aes(x = created_utc, y = compound)) +
  geom_line(color = "black") +
  geom_point(color = "black", size = 2) +
  scale_fill_gradientn(colours = rainbow(length(unique(SentimentUK$compound)))) +
  labs(title= 'r/UK Posts on the CoL', x = "Date", y = "Negativity Score") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")

#With your table, create a plot for a subset of your choice (e.g. r/UK posts from 2022, r/Scotland posts with compound >=0.5, etc). Discuss your findings
#Then, share your plot with the class.

#3. PART 3: Compare the top positive and negative posts in r/Scotland
#Subset the top 25 positive and top 25 negative posts:
top_positive <- sorted_df[1:25, ]
top_negative <- sorted_df[(nrow(sorted_df)-24):nrow(sorted_df), ]
#Clean and tokenize the subset positive data:
pos_subset <- gsub("http\\S+\\s*", "", top_positive)
pos_subset <- tolower(pos_subset)
pos_subset <- tokens(pos_subset)
pos_subset <- tokens_remove(pos_subset, stopwords("en"))
pos_subset <- tokens_subset[pos_subset$types >= 3]
pos_subset <- tokens_select(pos_subset, stopwords("en"), selection = "remove")
pos_subset <- tokens_select(pos_subset, min_nchar = 3)
# Generate the word cloud
wordcloud(pos_subset, random.order=FALSE, max.words=100, colors=brewer.pal(n=5, name = "Dark2"))
#Repeat for the subset negative data:
neg_subset <- gsub("http\\S+\\s*", "", top_negative)
neg_subset <- tolower(neg_subset)
neg_subset <- tokens(neg_subset)
neg_subset <- tokens_remove(neg_subset, stopwords("en"))
neg_subset <- tokens_subset[neg_subset$types >= 3]
neg_subset <- tokens_select(neg_subset, stopwords("en"), selection = "remove")
neg_subset <- tokens_select(neg_subset, min_nchar = 3)
# Generate the word cloud
wordcloud(neg_subset, random.order=FALSE, max.words=100, colors=brewer.pal(n=5, name = "Set2"))

#Bonus (if time): with your table, repeat the process for the r/UK datset. Then, discuss your findings.

#Wrap-up discussion:
#1. What are the pros and cons of VADER Sentiment as a method?
#2. Can we answer our research questions? What evidence do we still have? What might we still need?



