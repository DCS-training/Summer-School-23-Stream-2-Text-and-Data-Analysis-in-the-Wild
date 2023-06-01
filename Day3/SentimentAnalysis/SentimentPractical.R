# Exercise 1: Filtering, analysing and plotting data  =============
#With your table, create a plot for a subset of your choice (e.g. r/UK posts from 2022, r/Scotland posts with compound >=0.5, etc). Discuss your findings
#Then, share your plot with the class.

# Exercise Subset 
UK23<-UK %>%
  filter(str_detect(date, "2023"))


#Plot the 2023 posts:
# Define my gradient 
gradient <- colorRampPalette(c("red", "green"))
ggplot(UK23, aes(x = date, y =Sentiment, colour=Sentiment)) +
  geom_point(size = 7) +
  scale_colour_gradientn(colours = gradient(10)) +
  labs(title='r/UK VADER Compound Sentiment', x = "Post Date", y = "Sentiment Score") +
  theme_minimal()+
  geom_hline(yintercept=0, linetype='dotted')

# Exercise 2: Comparing positive + negative results  =============
# With your table, repeat the process for the r/UK datset. Then, discuss your findings.
## 5.1. Subset the top 25 positive and top 25 negative posts---------
top_positive <- SentimentUK[1:25, ]
top_negative <- SentimentUK[(nrow(SentimentUK)-24):nrow(SentimentUK), ]
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
