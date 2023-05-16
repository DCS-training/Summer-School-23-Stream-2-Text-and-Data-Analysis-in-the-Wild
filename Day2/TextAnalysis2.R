#CDCS Summer School 2023: Text Analysis (Part 2)
#Tuesday, 6th June
install.packages("quanteda.textstats")
install.packages("quanteda.textmodels")
install.packages("tm")

library(quanteda)
library(tidyverse)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(tm)


#load the data and subset the text column
uk_data <- read_csv("Day2/data/uk_data.csv", locale = locale(encoding = "WINDOWS-1252"))#using this to make sure that is correctly encoded

uk_data <- read.csv("Day2/data/uk_data.csv")

uk_text <- iconv(uk_data$clean_text, to = 'ASCII', sub= '')#Add something on w


scot_data <- read_csv("data/uk_data.csv", locale = locale(encoding = "WINDOWS-1252"))

#cleaning
uk_corpus <- VCorpus(VectorSource(uk_text))#transform our dataset in a corpus
uk_corpus <- tm_map (uk_corpus, content_transformer(tolower))
uk_corpus <- tm_map (uk_corpus, removePunctuation)
uk_corpus <- tm_map (uk_corpus, removeWords, stopwords('english'))
uk_corpus <- tm_map (uk_corpus, removeWords, c('s', 't', '@\\w+', 'http.+ |http.+$','amp'))
uk_corpus <- tm_map (uk_corpus, removeNumbers)
uk_corpus <- tm_map (uk_corpus, stripWhitespace)

#Topic
#dtm of cleaned corpus
lda_dtm <- DocumentTermMatrix(uk_corpus) 
inspect(lda_dtm) 



#check which terms appear at least 100 times
findFreqTerms(lda_dtm, 100) 

#print the terms associated with the keyword that have a correlation coefficient of >= 0.4:
findAssocs(lda_dtm, "england", .4)
findAssocs(lda_dtm, "scotland", .4)

#create a term frequency matrix
lda_mx <- as.matrix(lda_dtm)
term_freq <- colSums(lda_mx)
term_freq <- sort(term_freq, decreasing=TRUE)
term_freq[0:30]

library(topicmodels)
#LDA topic model:
uk_lda <- LDA(lda_dtm, k=5, control=list(seed=1234))


#...ok, let's remove the empty rows in the dtm:
rowTotals <-apply (lda_dtm,1,sum) #running this line takes time
empty.rows<-lda_dtm[rowTotals==0,]$dimnames[1][[1]]  
clean_corpus<-uk_corpus[-as.numeric(empty.rows)]
lda_dtm <- DocumentTermMatrix(clean_corpus)

#try the LDA topic model again:
uk_lda <- LDA(lda_dtm, k=5, control=list(seed=1234))
uk_lda_topics<-as.matrix(topics(uk_lda))
uk_lda_terms <- as.matrix(terms(uk_lda,10))

#print the top 10 terms associated with each topic:
uk_lda_terms[1:10,]

#have a look at the output and dicsuss your thoughts with your table mates.

#let's try the lda again, expanding the number of topics to 10
uk_lda <- LDA(lda_dtm, k=10, control=list(seed=1234))
uk_lda_topics<-as.matrix(topics(uk_lda))
uk_lda_terms <- as.matrix(terms(uk_lda,10))
uk_lda_terms[1:10,]

#what can we observe about the effect of adding more topics?

#let's try another form of topic modelling: LSA. we'll be using quanteda here
uk_tokens <- tokens(uk_data$clean_text, remove_symbols=TRUE, remove_url=TRUE, remove_punct=TRUE)
uk_tokens <- tokens_select(uk_tokens, min_nchar = 3)
lsa_dfm <- dfm(uk_tokens)
lsa_dfm <- dfm_remove(lsa_dfm, stopwords('english'))

uk_lsa <- textmodel_lsa(lsa_dfm)
scot_tokens <- tokens(scot_data$clean_text, remove_symbols=TRUE, remove_url=TRUE, remove_punct=TRUE)
scot_tokens <- tokens_select(scot_tokens, min_nchar = 3)
scot_dfm <- dfm(scot_tokens)
scot_dfm <- dfm_remove(scot_dfm, stopwords('english'))

scot_lsa <- scot_dfm %>% dfm_match(featnames(lsa_dfm))


newq <- predict(uk_lsa, newdata = scot_lsa)
newq$docs_newspace[,1:2]

#visualizations???

#wrap-up discussion


