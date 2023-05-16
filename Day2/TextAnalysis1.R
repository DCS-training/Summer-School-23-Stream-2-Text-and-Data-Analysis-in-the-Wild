#CDCS Summer School 2023: Text Analysis
#Tuesday, 6th June

library(quanteda)
library(quanteda.textplots)
library(quanteda.textmodels)
library(ggplot2)
library(lexicon)
library(tidyverse)

#load the data
uk_data <- read_csv("Day2/data/uk_data.csv")

#examine the data 
summary(uk_data)

head(uk_data, 1)

#create a quanteda corpus of the 'article text' column from our data set:
article_text<-corpus(uk_data, text_field='clean_text')

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

#cleaning: stopword removal
dfm_nostop <- dfm_remove(dfm_uk, stopwords('english'))
topfeatures(dfm_nostop, 10)
topfeatures(dfm_uk, 10)


#let's try the word cloud again:
textplot_wordcloud(dfm_nostop, rotation = 0.25, max_words=50, color = rev(RColorBrewer::brewer.pal(10, "Spectral")))

#what observations do we have?


#add a remove costum stopwords list 

#further steps for cleaning: stemming vs. lemmatization
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
textplot_wordcloud(lemma_dfm, rotation = 0.25, max_words=50, color = rev(RColorBrewer::brewer.pal(10, "Paired")))

#plot the top 20 words (non-lemmatized) in another way:
top_keys <- topfeatures (dfm_nostop, 20)
data.frame(list(term = names(top_keys), frequency = unname(top_keys))) %>% # Create a data.frame for ggplot
  ggplot(aes(x = reorder(term,-frequency), y = frequency)) + # Plotting with ggplot2
  geom_point() +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#closing discussion:

#take a look at top_keys and discuss your thoughts about the object with your table.

#looking at the word clouds we've made so far (hint: you can scroll through the plots using the arrows in the top left corner of the window), 
#what can we answer about our research question? what is still unknown?
#what can keywords show us about a corpus? what can they not show us?
#discuss with your tables the pros and cons of the methods we've covered so far
#bonus points for coming up with a potential use case in the context of your research!

