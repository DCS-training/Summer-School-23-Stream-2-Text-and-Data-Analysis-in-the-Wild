#CDCS Summer School 2023: Text Analysis
#Tuesday, 6th June
library(quanteda)
library(quanteda.textplots)
library(quanteda.textmodels)
library(ggplot2)
library(lexicon)
library(tidyverse)
library (tm)

#load the Uk data
uk_data <- read_csv("Day1/WebScraping/outputs/UKNews.csv")
#Drop the first column that we do not need
uk_data<-uk_data[, 2:4]

#examine the data 
summary(uk_data)

#Look at the text of our articles
head(uk_data$clean_text)

# There are a lot of formatting (next line, next paragraph) that we want to get read of 

uk_data_clean <- mutate_if(uk_data, #change if is character so titles and texts
                         is.character, 
                         str_replace_all, 
                         pattern = "\r?\n|\r", #What I am searching
                         replacement = " ")#What I am replacing with

#which will insert only one space regardless whether the text contains \r\n, \n or \r.
# Let's check again 
head(uk_data_clean$clean_text)

#create a quanteda corpus of the 'article text' column from our data set:
article_text<-corpus(uk_data_clean, text_field='clean_text')

#some methods for extracting information about the corpus:
summary(article_text, 5)
ndoc(article_text) #number of documents in the corpus
nchar(article_text[1:10]) #number of characters in the first 10 documents of the corpus
ntoken(article_text[1:10]) #number of tokens in the first 10 documents

#tokenise the corpus 
article_tokens <- tokens(article_text, 
                         remove_symbols=TRUE, 
                         remove_url=TRUE, 
                         remove_punct=TRUE)

#remove tokens under 3 characters:
article_tokens <- tokens_select(article_tokens, min_nchar = 3)


#keyword search examples (using kwic aka "keyword in context")
kwic(article_tokens, "cost")
kwic(article_tokens, "cost", 3)
article_tokens %>% 
  kwic(pattern = phrase("cost of living"))
article_tokens %>%
  kwic(pattern=c("price", "bills", "payment"))

#convert to document-feature matrix (aka "dfm")
dfm_uk <- dfm(article_tokens)

#plot a wordcloud
textplot_wordcloud(dfm_uk, max_words=100, color='black')

#what observations do we have about the wordcloud? what should our next steps be?

#cleaning: lowercase
dfm_uk <- dfm_tolower(dfm_uk)

# Cleaning: stopword removal
dfm_nostop <- dfm_remove(dfm_uk, stopwords('english'))
topfeatures(dfm_nostop, 10)
topfeatures(dfm_uk, 10)


#Let's try the word cloud again:
textplot_wordcloud(dfm_nostop, rotation = 0.25,
                   max_words=50,
                   color = rev(RColorBrewer::brewer.pal(10, "Spectral")))

# What observations do we have?


# Add a remove custom stop words list 

customstopwords <- c("cost", "living", "2023")#I just remove some of the keywords that are not really telling me much

dfm_nostop_cost <- dfm_remove(dfm_uk, c(stopwords('english'), customstopwords))
topfeatures(dfm_nostop, 10)
topfeatures(dfm_nostop_cost, 10)



# Further steps for cleaning: stemming vs. lemmatization
# i. stemming
nostop_toks <- tokens_select(article_tokens, pattern = stopwords("en"), selection = "remove")
stem_toks <- tokens_wordstem(nostop_toks, language=quanteda_options('language_stemmer'))
stem_dfm <- dfm(stem_toks)

#let's see the top features
topfeatures(stem_dfm, 30)


# ii lemmatization
lemmas <- tokens_replace(nostop_toks, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
lemma_dfm <- dfm(lemmas)

topfeatures(stem_dfm, 20)
topfeatures(lemma_dfm, 20)

#what do you think about the results? what can we learn about how the computer "reads" in each example?

#make a word cloud of the lemmatized results:
textplot_wordcloud(lemma_dfm, rotation = 0.25,
                   max_words=50,
                   color = rev(RColorBrewer::brewer.pal(10, "Paired")))

#plot the top 20 words (non-lemmatized) in another way:
top_keys <- topfeatures (dfm_nostop, 20)
data.frame(list(term = names(top_keys), frequency = unname(top_keys))) %>% # Create a data.frame for ggplot
  ggplot(aes(x = reorder(term,-frequency), y = frequency)) + # Plotting with ggplot2
  geom_point() +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# Closing discussion: =============

# Now we compare what we have found for the general UK with what we can find in the Scottish news. Hint: Copy and paste the code we used so far below and adapt it to do the same steps in the Scottish Dataset 

# Some help to start with 
# Load the Scotland data
SC_data <- read_csv("Day1/WebScraping/outputs/ScotlandNews.csv")
#Drop the first column that we do not need
SC_data<-SC_data[, 2:4]

#examine the data 
summary(SC_data)

#Look at the text of our articles
head(SC_data$texts)

# There are a lot of formatting (next line, next paragraph) that we want to get read of 

SC_data_clean <- mutate_if(SC_data, #change if is character so titles and texts
                           is.character, 
                           str_replace_all, 
                           pattern = "\r?\n|\r", #What I am searching
                           replacement = " ")#What I am replacing with

#which will insert only one space regardless whether the text contains \r\n, \n or \r.
# We can use the same trick to uniform Scotland and Scottish. Spoiler alert there are a lot of these two words repeating 

SC_data_clean <- mutate_if(SC_data, #change if is character so titles and texts
                           is.character, 
                           str_replace_all, 
                           pattern = "[Ss]cottish", #What I am searching
                           replacement = "Scotland")#What I am replacing with



# Let's check again 
head(SC_data_clean$texts)

#create a quanteda corpus of the 'article text' column from our data set:
article_text_SC<-corpus(SC_data_clean, text_field='texts')

# Now up to you when you are done compare the world clouds and the top-keys results between Scotland and UK and discuss what you see with your table


#take a look at top_keys and discuss your thoughts about the object with your table.

#looking at the word clouds we've made so far (hint: you can scroll through the plots using the arrows in the top left corner of the window), 
#what can we answer about our research question? what is still unknown?
#what can keywords show us about a corpus? what can they not show us?
#discuss with your tables the pros and cons of the methods we've covered so far
#bonus points for coming up with a potential use case in the context of your research!

