###############################################################
# CDCS Summer School 2023
# Text Analysis Part 2
# Tuesday, 6th June
###############################################################

# Research Questions =========================
# 1.Can we see a peak in the amount and length of articles on the cost of living in the last 3 years. Is this trend similar for both Scotland and general UK website
# 2.Can we see a difference in the wording about the cost of living between Scottish and general UK governments websites?

# Setting up ===================
# Library needed
library(quanteda)
library(tidyverse)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(tm)
library(topicmodels)

# Load the data and subset the text column
# Load the Uk data
uk_data <- read_csv("Day1/WebScraping/outputs/UKNews.csv")
# Drop the first column that we do not need
uk_data<-uk_data[, 2:4]
# Examine the data 
summary(uk_data)
# Clean it with Regex
uk_data_clean <- mutate_if(uk_data, #change if is character so titles and texts
                           is.character, 
                           str_replace_all, 
                           pattern = "\r?\n|\r", #What I am searching
                           replacement = " ")#What I am replacing with

# Which will insert only one space regardless whether the text contains \r\n, \n or \r.


# Load the Scotland data
SC_data <- read_csv("Day1/WebScraping/outputs/ScotlandNews.csv")
# Drop the first column that we do not need
SC_data<-SC_data[, 2:4]
# Examine the data 
summary(SC_data)
# Clean it with Regex
SC_data_clean <- mutate_if(SC_data, #change if is character so titles and texts
                           is.character, 
                           str_replace_all, 
                           pattern = "\r?\n|\r", #What I am searching
                           replacement = " ")#What I am replacing with

# Which will insert only one space regardless whether the text contains \r\n, \n or \r.

# select only text
uk_data_clean_text<-uk_data_clean$clean_text

# Cleaning the UK data set===========================
uk_corpus <- VCorpus(VectorSource(uk_data_clean_text))# transform our data set in a corpus
uk_corpus <- tm_map (uk_corpus, content_transformer(tolower))# remove capitalised letters
uk_corpus <- tm_map (uk_corpus, removePunctuation)# remove punctuation
uk_corpus <- tm_map (uk_corpus, removeWords, stopwords('english')) # remove English stopwords
uk_corpus <- tm_map (uk_corpus, removeWords, c('s', 't', '@\\w+', 'http.+ |http.+$','amp')) # remove specific words/symbols
uk_corpus <- tm_map (uk_corpus, removeNumbers)# remove numbers
uk_corpus <- tm_map (uk_corpus, stripWhitespace) # remove multiple white spaces

# Topic Modelling=================================
# Dtm of cleaned corpus
lda_dtm_uk <- DocumentTermMatrix(uk_corpus) #A document-term matrix is a mathematical matrix that describes the frequency of terms that occur in a collection of documents. In a document-term matrix, rows correspond to documents in the collection and columns correspond to terms.
inspect(lda_dtm_uk) 

# Check which terms appear at least 100 times
findFreqTerms(lda_dtm_uk, 100) 

# Print the terms associated with the keyword that have a correlation coefficient of >= 0.4:
findAssocs(lda_dtm_uk, "england", .4)
findAssocs(lda_dtm_uk, "scotland", .4)

# Create new object containing our results
AssociationEngland<-data.frame(findAssocs(lda_dtm_uk, "england", .4))
AssociationEnglandCleaned<- data.frame(Term=rownames(AssociationEngland), Value=AssociationEngland[,1], Association= "England")

AssociationScotland<-data.frame(findAssocs(lda_dtm_uk, "scotland", .4))
AssociationScotlandCleaned<- data.frame(Term=rownames(AssociationScotland), Value=AssociationScotland[,1], Association="Scotland")

AssociationTot <-rbind(AssociationEnglandCleaned, AssociationScotlandCleaned)

# Visualise our results
ggplot(AssociationTot, aes(y=Term, x=Value, colour=Association))+
  geom_point(size=5)+
  theme_bw()
# What can we see in the graph? 

# Create a term frequency matrix
lda_mx_uk <- as.matrix(lda_dtm_uk)
term_freq_uk <- colSums(lda_mx_uk)
term_freq_uk <- sort(term_freq_uk, decreasing=TRUE)
term_freq_uk[0:30]


# LDA topic model:-------------------------------------
uk_lda <- LDA(lda_dtm_uk, k=5, control=list(seed=1234))

summary(uk_lda)
#...ok, let's remove the empty rows in the dtm:
#rowTotals <-apply (lda_dtm,1,sum) #running this line takes time
#empty.rows<-lda_dtm[rowTotals==0,]$dimnames[1][[1]]  
#clean_corpus<-uk_corpus[-as.numeric(empty.rows)]
#lda_dtm <- DocumentTermMatrix(clean_corpus) I do not think we need this anymore since I managed to clean the dataframe at the start but did not want to cancel until checking with you 
#try the LDA topic model again:
#uk_lda <- LDA(lda_dtm, k=5, control=list(seed=1234))

uk_lda_topics<-as.matrix(topics(uk_lda))
uk_lda_terms <- as.matrix(terms(uk_lda,10))

# Print the top 10 terms associated with each topic:
uk_lda_terms[1:10,]

# Have a look at the output and discuss your thoughts with your table mates.

# Let's try the lda again, expanding the number of topics to 10
uk_lda <- LDA(lda_dtm_uk, k=10, control=list(seed=1234))
uk_lda_topics<-as.matrix(topics(uk_lda))
uk_lda_terms <- as.matrix(terms(uk_lda,10))
uk_lda_terms[1:10,]

# What can we observe about the effect of adding more topics? Can we identify possible topics?

# LSA Topic Modelling -----------------------------
# Let's try another form of topic modelling: LSA. we'll be using Quanteda here
uk_tokens <- tokens(uk_data_clean$clean_text, remove_symbols=TRUE, remove_url=TRUE, remove_punct=TRUE)
uk_tokens <- tokens_select(uk_tokens, min_nchar = 3)# Remove token under 3 characters
lsa_dfm_uk <- dfm(uk_tokens) # Document feature matrix
lsa_dfm_uk <- dfm_remove(lsa_dfm_uk, stopwords('english'))

# Run the Model
uk_lsa <- textmodel_lsa(lsa_dfm_uk)


# Use the Scotland data to test the model based on the uk data
scot_tokens <- tokens(SC_data_clean$texts, remove_symbols=TRUE, remove_url=TRUE, remove_punct=TRUE)
scot_tokens <- tokens_select(scot_tokens, min_nchar = 3)
scot_dfm <- dfm(scot_tokens)
scot_dfm <- dfm_remove(scot_dfm, stopwords('english'))

scot_lsa <- scot_dfm %>%
  dfm_match(featnames(lsa_dfm_uk))# Some Comment here 

# I cannot really understand what you are doing so not sure how to help with plotting 
newq <- predict(uk_lsa, newdata = scot_lsa)
newq$docs_newspace[,1:3]

result <- data.frame(Text = rownames(newq$docs_newspace), Dim1 = newq$docs_newspace[,1], Dim2 = newq$docs_newspace[,2])


# Create a scatter plot
ggplot(result, aes(x = Dim1, y = Dim2)) +
  geom_point() +
  labs(x = "LSA Dimension 1", y = "LSA Dimension 2") +
  theme_bw()+
  ggtitle("LSA Visualisation")


# Wrap-up discussion
