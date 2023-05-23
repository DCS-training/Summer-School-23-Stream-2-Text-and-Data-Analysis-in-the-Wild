# Exercise 1: Clean and Analyse the Scottish News  =============

# Now we compare what we have found for the general UK with what we can find in the Scottish news. 

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


#some methods for extracting information about the corpus:
summary(article_text_SC, 5)
ndoc(article_text_SC) #number of documents in the corpus
nchar(article_text_SC[1:10]) #number of characters in the first 10 documents of the corpus
ntoken(article_text_SC[1:10]) #number of tokens in the first 10 documents

#tokenise the corpus 
article_tokens_SC <- tokens(article_text_SC, 
                         remove_symbols=TRUE, 
                         remove_url=TRUE, 
                         remove_punct=TRUE)

#remove tokens under 3 characters:
article_tokens_SC <- tokens_select(article_tokens_SC, min_nchar = 3)


#keyword search examples (using kwic aka "keyword in context")
kwic(article_tokens_SC, "cost")
kwic(article_tokens_SC, "cost", 3)
article_tokens_SC %>% 
  kwic(pattern = phrase("cost of living"))
article_tokens_SC %>%
  kwic(pattern=c("price", "bills", "payment"))

#convert to document-feature matrix (aka "dfm")
dfm_SC <- dfm(article_tokens_SC)

#plot a wordcloud
textplot_wordcloud(dfm_SC, max_words=100, color='black')

#what observations do we have about the wordcloud? what should our next steps be?

#cleaning: lowercase
dfm_SC <- dfm_tolower(dfm_SC)

# Cleaning: stopword removal
dfm_nostop_SC <- dfm_remove(dfm_SC, stopwords('english'))
topfeatures(dfm_nostop_SC, 10)
topfeatures(dfm_SC, 10)


#Let's try the word cloud again:
textplot_wordcloud(dfm_nostop_SC, rotation = 0.25,
                   max_words=50,
                   color = rev(RColorBrewer::brewer.pal(10, "Spectral")))



# Further steps for cleaning: stemming vs. lemmatization
# i. stemming
nostop_toks_SC <- tokens_select(article_tokens_SC, pattern = stopwords("en"), selection = "remove")
stem_toks_SC <- tokens_wordstem(nostop_toks_SC, language=quanteda_options('language_stemmer'))
stem_dfm_SC <- dfm(stem_toks_SC)

#let's see the top features
topfeatures(stem_dfm_SC, 30)


# ii lemmatization
lemmas_SC <- tokens_replace(nostop_toks_SC, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
lemma_dfm_SC <- dfm(lemmas_SC)

topfeatures(stem_dfm_SC, 20)
topfeatures(lemma_dfm_SC, 20)

#what do you think about the results? what can we learn about how the computer "reads" in each example?

#make a word cloud of the lemmatized results:
textplot_wordcloud(lemma_dfm_SC, rotation = 0.25,
                   max_words=50,
                   color = rev(RColorBrewer::brewer.pal(10, "Paired")))

#plot the top 20 words (non-lemmatized) in another way:
top_keys_SC <- topfeatures (dfm_nostop_SC, 20)
data.frame(list(term = names(top_keys_SC), frequency = unname(top_keys_SC))) %>% # Create a data.frame for ggplot
  ggplot(aes(x = reorder(term,-frequency), y = frequency)) + # Plotting with ggplot2
  geom_point() +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
