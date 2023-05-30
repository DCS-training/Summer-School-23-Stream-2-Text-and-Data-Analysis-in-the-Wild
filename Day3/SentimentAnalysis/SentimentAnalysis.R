############### SENTIMENT ANALYSIS ##########################
# RESEARCH QUESTIONS #######
#1. What trends (if any) can we observe about sentiment about the CoL in each data set?
#2. Based on one data set only, is the CoL crisis getting better or worse in 2023? How do things differ in Scotland vs. the UK?

# 1. Getting Setup ====================
## 1.1. Libraries needed--------------
library(syuzhet)
library(vader)
library(tidyverse)
library(wordcloud)
library(tm)


# 2. Overall sentiment (Scotland vs. UK)===================
## 2.1. Import data -------------------
SentimentScotland <- read_csv("Day3/SentimentAnalysis/data/scotland_data.csv")
SentimentUK <- read_csv("Day3/SentimentAnalysis/data/uk_senti.csv")

## 2.2. Calculate sentiment scores with VADER --------
scot_scores <- SentimentScotland$selftext %>%
  lapply(get_vader)
uk_scores <- SentimentUK$selftext %>%
  lapply(get_vader)# lapply= apply across the data set

## 2.3. Add compound sentiment score to the data frame --------
# On Scotland data set
SentimentScotland <- SentimentScotland %>%
  mutate(
  compound = scot_scores %>%
    sapply(function(v) { as.numeric(v["compound"]) }))
# On UK data set 
SentimentUK <- SentimentUK %>%
  mutate(
  compound = uk_scores %>%
    sapply(function(v) { as.numeric(v["compound"]) }))

## 2.4. Statistical overview -------
# Mean score and range of scores for each data set
# On Uk Data
mean(SentimentUK$compound)
range(SentimentUK$compound)
# On Scotland Data
mean(SentimentScotland$compound)
range(SentimentScotland$compound)

# Print the titles of the top negative and top positive posts for each data set
# Create a new object for Scotland
SentimentScotland<-SentimentScotland[order(SentimentScotland$compound, decreasing = TRUE), ]
# Print head and tail
head(SentimentScotland$title, 1)
tail(SentimentScotland$title, 1)

# Create a new object for UK
SentimentUK<-SentimentUK[order(SentimentUK$compound, decreasing = TRUE), ]
# Print head and tail
head(SentimentUK$title, 1)
tail(SentimentUK$title, 1)

## 2.5. Look at the two together -----------
### 2.5.1. Merge them ---------------
UK<-data.frame(Sentiment=as.double(SentimentUK$compound), dataset= "UK")
Scotland<-data.frame(Sentiment=as.double(SentimentScotland$compound), dataset="Scotland")
FullDataSet<-rbind(UK, Scotland)

### 2.5.2. plot them -------------
ggplot(FullDataSet, aes(x=dataset,y=Sentiment, fill=dataset) )+
  geom_boxplot()+
  theme_bw()+
  geom_hline(yintercept = 0, linetype="dotted" )
  ggtitle("Sentiment in Scotland and and UK Redits")

# 3. Sentiment scores by date ========
## 3.1. Data Cleaning -----------
### 3.1.1. Standardise date format + remove time stamp 
# On the Scotland Data set -----------
SentimentScotland$Date <- as.Date(as.POSIXct(SentimentScotland$created_utc, 'GMT'))

SentimentScotland$MonthYear <- format(as.Date(SentimentScotland$Date, format="%Y-%m-%d"),"%Y-%m")  
  
# On the UK Data set
SentimentUK$Date <- as.Date(as.POSIXct(SentimentUK$created_utc, 'GMT'))

SentimentUK$MonthYear <- format(as.Date(SentimentUK$Date, format="%Y-%m-%d"),"%Y-%m")  

### 3.1.2. add it to the Scotland and Uk Data set ------
Scotland<- data.frame(Scotland,
                      MonthYear= SentimentScotland$MonthYear,
                      date=SentimentScotland$Date)

UK <-data.frame(UK,
                MonthYear= SentimentUK$MonthYear,
                date=SentimentUK$Date)

FullDataSet<-rbind(UK, Scotland)
### 3.1.3. Group by --------
# Scotland
ByMonthYearScotland<-Scotland%>%
  group_by(MonthYear)%>%
  summarise(meanSent=mean(Sentiment))
# Uk
ByMonthYearUK<-UK%>%
  group_by(MonthYear)%>%
  summarise(meanSent=mean(Sentiment))
# Total 
ByMonthYearTotal<-FullDataSet%>%
  group_by(MonthYear,dataset)%>%
  summarise(meanSent=mean(Sentiment))

## 3.2. Create plots ------------
### 3.2.1. Scotland
# Define my gradient 
gradient <- colorRampPalette(c("red", "green"))
ggplot(ByMonthYearScotland, aes(x = MonthYear, y = meanSent, colour=meanSent)) +
  geom_path(aes(group=1), colour="black", size=1)+
  geom_point(size = 7) +
  scale_colour_gradientn(colours = gradient(10)) +
  labs(title='r/Scotland VADER Compound Sentiment', x = "Post Date", y = "Sentiment Score") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### 3.2.2. UK ------------
# Define my gradient 
gradient <- colorRampPalette(c("red", "green"))
ggplot(ByMonthYearTotal, aes(x = MonthYear, y = meanSent, colour=meanSent)) +
  geom_path(aes(group=1), colour="black", size=1)+
  geom_point(size = 7) +
  scale_colour_gradientn(colours = gradient(10)) +
  labs(title='r/Scotland VADER Compound Sentiment', x = "Post Date", y = "Sentiment Score") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~dataset, ncol=1)

### 3.2.3. Full Data set ------------
# Define my gradient 
gradient <- colorRampPalette(c("red", "green"))
ggplot(ByMonthYearUK, aes(x = MonthYear, y = meanSent, colour=meanSent)) +
  geom_path(aes(group=1), colour="black", size=1)+
  geom_point(size = 7) +
  scale_colour_gradientn(colours = gradient(10)) +
  labs(title='r/Scotland VADER Compound Sentiment', x = "Post Date", y = "Sentiment Score") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Discuss the stats, top tiles, and plots with your table. What do/don't they show us? What might the pros and cons of our data sets be?


# 4. Methods for filtering the data ============
## 4.1. Subset the data frame by posts from a certain date (year = 2023, data=Scotland)------------
Scotland23<-Scotland %>%
  filter(str_detect(date, "2023"))


#Plot the 2023 posts:
# Define my gradient 
gradient <- colorRampPalette(c("red", "green"))
ggplot(Scotland23, aes(x = date, y =Sentiment, colour=Sentiment)) +
  geom_point(size = 7) +
  scale_colour_gradientn(colours = gradient(10)) +
  labs(title='r/Scotland VADER Compound Sentiment', x = "Post Date", y = "Sentiment Score") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#With your table, create a plot for a subset of your choice (e.g. r/UK posts from 2022, r/Scotland posts with compound >=0.5, etc). Discuss your findings
#Then, share your plot with the class.

# 5. Compare the top positive and negative posts ====
# in r/Scotland
## 5.1. Subset the top 25 positive and top 25 negative posts---------
top_positive <- SentimentScotland[1:25, ]
top_negative <- SentimentScotland[(nrow(SentimentScotland)-24):nrow(SentimentScotland), ]
## 5.2. Clean and tokenize ------
# First the subset positive data:
library(quanteda)
pos_subset <- gsub("http\\S+\\s*", "", top_positive)
pos_subset <- tolower(pos_subset)
pos_subset <- quanteda::tokens(pos_subset)
pos_subset <- tokens_remove(pos_subset, stopwords("en"))
pos_subset <- tokens_select(pos_subset, stopwords("en"), selection = "remove")
pos_subset <- tokens_select(pos_subset, min_nchar = 3)
## 5.3. Generate the word cloud -----
wordcloud(pos_subset,
          random.order=FALSE,
          max.words=100,
          colors=brewer.pal(n=5, name = "Dark2"))
#Repeat for the subset negative data:
neg_subset <- gsub("http\\S+\\s*", "", top_negative)
neg_subset <- tolower(neg_subset)
neg_subset <- quanteda::tokens(neg_subset)
neg_subset <- tokens_remove(neg_subset, stopwords("en"))
neg_subset <- tokens_select(neg_subset, stopwords("en"), selection = "remove")
neg_subset <- tokens_select(neg_subset, min_nchar = 3)
# Generate the word cloud
wordcloud(neg_subset, random.order=FALSE, max.words=100, colors=brewer.pal(n=5, name = "Set2"))

#Bonus (if time): with your table, repeat the process for the r/UK datset. Then, discuss your findings.

# 6. Wrap-up discussion==========
#1. What are the pros and cons of VADER Sentiment as a method?
#2. Can we answer our research questions? What evidence do we still have? What might we still need?



