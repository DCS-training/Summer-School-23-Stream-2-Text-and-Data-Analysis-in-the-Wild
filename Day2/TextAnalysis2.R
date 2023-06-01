############ TEXT ANALYSIS  ##################

# RESEARCH QUESTIONS ##############
# 1.What are some common themes that appear in the UK government's publications about the CoL? 
# 2.Comparing the UK and Scotland data sets, do we see any patterns/similarities/differences in themes?


# PART 2: TOPIC MODELLING ###########
# 1. Setting up ===================
## 1.1. Libraries needed -----------
install.packages("quanteda.textstats")
install.packages("syuzhet")
library(quanteda)
library(tidyverse)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(tm)
library(topicmodels)
library(syuzhet)
library(RColorBrewer)

## 1.2. Load the data------------
uk_data <- read_csv("Day1/WebScraping/outputs/UKNews.csv")
SC_data <- read_csv("Day1/WebScraping/outputs/ScotlandNews.csv")
## 1.3. Drop the first column that we do not need---------
uk_data<-uk_data[, 2:4]
SC_data<-SC_data[, 2:4]
## 1.4. Examine the data -----------
summary(uk_data)
summary(SC_data)
# 2. Clean the data ============
## 2.1. Clean the Uk data ----------
#change all text to strings; remove characters associated with line breaks, replacing them with a space:
uk_data_clean <- mutate_if(uk_data, 
                           is.character, 
                           str_replace_all, 
                           pattern = "\r?\n|\r", #Searching for
                           replacement = " ")#Replace results with

SC_data_clean <- mutate_if(SC_data, 
                           is.character, 
                           str_replace_all, 
                           pattern ="\r?\n|\r", #Searching for
                             replacement = " ")

## 2.2. Clean the Scotland data-------
# This will come handy afterwards but let's have a new data set that will merge Scotland and UK 
# First all need to rename UK columns and reorder them
ForMergingUK<-uk_data_clean %>% 
  rename(
    texts = clean_text)%>% 
  select(texts, dates, titles)

Merged_dataset <- rbind(ForMergingUK, SC_data_clean)

## 2.3. Extract the text column ------
# We'll work with the UK data first, and then you'll repeat the process with the Scotland data on your own later in this block.
# Subset the text column and save it as an object:
uk_data_clean_text<-uk_data_clean$clean_text

## 2.4. Create a tm Corpus -----------------
# Prepare the data for analysis, creating and cleaning a tm Corpus object:
uk_corpus <- VCorpus(VectorSource(uk_data_clean_text))# transform our data set in a corpus
uk_corpus <- tm_map (uk_corpus, content_transformer(tolower))# remove capitalised letters
uk_corpus <- tm_map (uk_corpus, removePunctuation)# remove punctuation
uk_corpus <- tm_map (uk_corpus, removeWords, stopwords('english')) # remove English stopwords
uk_corpus <- tm_map (uk_corpus, removeWords, c('s', 't', '@\\w+', 'http.+ |http.+$','amp')) # remove specific words/symbols
uk_corpus <- tm_map (uk_corpus, removeNumbers)# remove numbers
uk_corpus <- tm_map (uk_corpus, stripWhitespace) # remove multiple white spaces

# 3. Topic Modelling=================================
## 3.1. Create a document term matrix (dtm) of the corpus------
# A DTM is a mathematical matrix that describes the frequency of terms that occur in a collection of documents.  
# Rows correspond to documents in the collection and columns correspond to terms.
lda_dtm_uk <- DocumentTermMatrix(uk_corpus)
inspect(lda_dtm_uk) 

## 3.2. Explore Frequency ----------
# Print the terms in the data set that appear at least 100 times
findFreqTerms(lda_dtm_uk, 100) 

## 3.3. Explore association with Scotland and England -----------
# Print the terms associated with the keyword that have a correlation coefficient of >= 0.4:
# (A correlation coefficient shows the strength of the relationship between two items on a scale of -1 to 1)
findAssocs(lda_dtm_uk, "england", .4)
findAssocs(lda_dtm_uk, "scotland", .4)
# Later on, we will compare the results after performing the same analysis on the UK dataset.

### 3.3.1 Getting the full list of association -------
# We want to select top of one and lower other
# Create a new object containing our results
AssociationEngland<-data.frame(findAssocs(lda_dtm_uk, "england", .01))
AssociationEnglandCleaned<- data.frame(Term=rownames(AssociationEngland), ValueEngland=AssociationEngland[,1], Association= "England")

AssociationScotland<-data.frame(findAssocs(lda_dtm_uk, "scotland", .01))
AssociationScotlandCleaned<- data.frame(Term=rownames(AssociationScotland), ValueScotland=AssociationScotland[,1], Association="Scotland")

# To bring them together we are going to use merge (we are going to cover tomorrow how it work but for now)
Merged_datasets <- merge(AssociationScotlandCleaned, AssociationEnglandCleaned, by.x = 'Term', by.y = 'Term') 

VeryEnglish<-subset(Merged_datasets,ValueScotland<0.45 &ValueEngland>0.55)

Merged_datasets$Comparison <- ifelse(Merged_datasets$ValueScotland > 0.55 & Merged_datasets$ValueEngland < 0.45, "VeryScottish", 
                                     ifelse(Merged_datasets$ValueEngland > 0.55 & Merged_datasets$ValueScotland <= 0.45, "VeryEnglish", 
                                            "Communal"))

# Subset only the words that are very Scottish or very English
Extreme<-subset(Merged_datasets, Comparison!="Communal")

# Now I need to have one single value for each
Extreme$Value<-ifelse(Extreme$Comparison == "VeryScottish", Extreme$ValueScotland, Extreme$ValueEngland)

### 3.3.2. Visualise our results ---------------
ggplot(Extreme, aes(y=Term, x=Value, colour=Comparison))+
  geom_point(size=5)+
  theme_bw()
# What can we see in the graph? 

## 3.4. Create a term frequency matrix --------------
lda_mx_uk <- as.matrix(lda_dtm_uk)
term_freq_uk <- colSums(lda_mx_uk)
term_freq_uk <- sort(term_freq_uk, decreasing=TRUE)
term_freq_uk[0:30]


## 3.5. LDA topic modelling-------------------------------------
### 3.5.1. Create a matrix k 5--------------
#Create a matrix for LDA analsyis, defining the number of topics (k=5)
uk_lda <- LDA(lda_dtm_uk, k=5, control=list(seed=1234))
# Get topics and terms from the LDA analysis
uk_lda_topics<-as.matrix(topics(uk_lda))
uk_lda_terms <- as.matrix(terms(uk_lda,10))
# Print the top 10 terms associated with each topic:
uk_lda_terms[1:10,]

# Have a look at the output and discuss your thoughts with your table.

### 3.5.2. Create a matrix k 10--------------
# Let's try the LDA again, expanding the number of topics to 10
uk_lda <- LDA(lda_dtm_uk, k=10, control=list(seed=1234))
uk_lda_topics<-as.matrix(topics(uk_lda))
uk_lda_terms <- as.matrix(terms(uk_lda,10))
uk_lda_terms[1:10,]

# What can we observe about the effect of adding more topics? 
# With your table, come up with a label for each topic.  What can we learn about our data using LDA? 

### 3.5.3. Repeat on cleaner dataset --------------
# Let's remove some of the words appearing that aren't telling us much about the data, and re-run LDA:
uk_corpus_2 <- tm_map (uk_corpus, removeWords, c('will', 'can', 'cost', 'living', 'help', 'people', 'new', 'cma', 'million'))
lda_dtm_uk_2 <- DocumentTermMatrix(uk_corpus_2)
uk_lda_2 <- LDA(lda_dtm_uk_2, k=5, control=list(seed=1234))
uk_lda_topics_2<-as.matrix(topics(uk_lda_2))
uk_lda_terms_2 <- as.matrix(terms(uk_lda_2,10))
# Print the top 10 terms associated with each topic:
uk_lda_terms_2[1:10,]

# Discuss the results with your table. From a human perspective, did removing extra words improve the topic modelling analysis?

# Exercise 2 ========
# So far, we worked on the UK dataset. Let's have a look at the merged (UK+ Scotland) dataframe we created earlier in this lesson ("Merged_dataset")

# Sentiment-category analysis with syuzhet:-------------------------------------
SentimentScotlandText <-SC_data_clean$texts
sentiment_scores <- get_nrc_sentiment(SentimentScotlandText, lang="english")
head(sentiment_scores)
summary(sentiment_scores)
SentimentScotlandText


#Plot the sentiment category scores:
par(mar = c(5, 5, 4, 2) + 0.1) 
barplot(
  colSums(prop.table(sentiment_scores[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Sentiment by Category: Scotland Data",
  xlab="category", ylab = 'frequency')

# Exercise 3: Topic Modelling + Sentiment Categorisation using the Scotland news dataset  =============
#With your table, repeat this analysis for the UK data set. What can you conclude about sentiment categorisation?

#Wrap-up discussion:
#1. What are the pros and cons of the methods we have used in this block?
#2. What have we learned about our research questions? What do we still need to find out, and what sorts of analysis might be helpful?


