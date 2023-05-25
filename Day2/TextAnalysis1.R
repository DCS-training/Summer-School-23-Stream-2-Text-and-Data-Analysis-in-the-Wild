###############################################################
# CDCS Summer School 2023
# Text Analysis Part 1
# Tuesday, 6th June
###############################################################

# Research Questions =========================
# 1.What can we observe about the number and length of articles on the cost of living in the last 3 years? How do the datasets compare?
# 2.Can we see a difference in the wording about the cost of living between the datasets?

# Setting up ===================
# Library needed
library(quanteda)
library(quanteda.textplots)
library(quanteda.textmodels)
library(ggplot2)
library(lexicon)
library(tidyverse)
library (tm)

# Load the Uk data
uk_data <- read_csv("Day1/WebScraping/outputs/UKNews.csv")
# Examine the data 
summary(uk_data)
# Drop the first column that we do not need
uk_data<-uk_data[, 2:4]

# Look at the text of our articles
head(uk_data$clean_text)
# There are a lot of formatting errors (next line, next paragraph) that we want to clean up
uk_data_clean <- mutate_if(uk_data, #change if is character so titles and texts
                         is.character, 
                         str_replace_all, 
                         pattern = "\r?\n|\r", #What I am searching
                         replacement = " ")#What I am replacing with

# Which will insert only one space regardless whether the text contains \r\n, \n or \r.
# Let's check again 
head(uk_data_clean$clean_text)

# Create a Quanteda corpus of the 'article text' column from our data set:
article_text<-corpus(uk_data_clean, text_field='clean_text')

# Some methods for extracting information about the corpus:
# Print doc in position 5 of the corpus
summary(article_text, 5)
# Check how many docs are in the corpus
ndoc(article_text) 
# Check number of characters in the first 10 documents of the corpus
nchar(article_text[1:10]) 
# Check number of tokens in the first 10 documents
ntoken(article_text[1:10]) 
# Create a new vector with tokens for all articles and store the vector as a new dataframe with three columns (Ntoken, Dataset, Date)
NtokenUK<-as.vector(ntoken(article_text))
TokenUK <-data.frame(Tokens=NtokenUK, Dataset="UK", Date=uk_data_clean$dates)
# Let's explore the number of articles published and their length to see if we can answer research question #1.
# First, let's extract Year and Month from the dates 
TokenUK$MonthYear <- format(as.Date(TokenUK$Date, format="%Y-%m-%d"),"%Y-%m")
# Now we can group the data by Month/Year and, for each group, count 1) how many articles were published and 2) how many tokens they contained
BreakoutUK<- TokenUK %>%
  group_by(MonthYear,Dataset)%>%
  summarize(NArticles=n(), MeanTokens=round(mean(Tokens)))
# Now we can plot the trends
ggplot(BreakoutUK, aes(x=MonthYear, y=NArticles))+ # Select data set and coordinates we are going to plot
  geom_point(aes(size=MeanTokens, fill=MeanTokens),shape=21, stroke=1.5, alpha=0.9, colour="black")+ # Which graph I want 
  labs(x = "Timeline", y = "Number of Articles", fill = "Mean of Tokens", size="Mean of Tokens", title="Number of Articles and Tokens in the UK Gov Website")+ # Rename labs and title
  geom_path(aes(group=1), colour="black", size=1)+ # Add a line that will connect the dots 
  scale_size_continuous(range = c(5, 15))+ # Resize the dots to be bigger
  geom_text(aes(label=MeanTokens))+ # Add the mean of tokens in the dots
  scale_fill_viridis_c(option = "plasma")+ # Change the colour coding
  theme_bw()+ # B/W Background
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+ # Rotate labels of x and move them slightly down. Plus move the position to the bottom 
  guides(size = "none") # Remove the Size from the Legend 

# Now we can tokenise the corpus 
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


# We can also create a custom list of words to remove from the corpus:

customstopwords <- c("cost", "living", "2023")#removed keywords that aren't telling us much

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

#what can we observe about stemming and lemmatization? which method (if any) is better for answering our research questions, and why?

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

# To start:
# Load the Scotland data
SC_data <- read_csv("Day1/WebScraping/outputs/ScotlandNews.csv")
#Drop the first column that we do not need
SC_data<-SC_data[, 2:4]

#examine the data 
summary(SC_data)

#Look at the text of our articles
head(SC_data$texts)

# Clean up the formatting annotations:

SC_data_clean <- mutate_if(SC_data, #change if is character so titles and texts
                           is.character, 
                           str_replace_all, 
                           pattern = "\r?\n|\r", #What I am searching
                           replacement = " ")#What I am replacing with

# Which will insert only one space regardless of whether the text contains \r\n, \n or \r.
# We can use the same trick to standardise references to Scotland (including "Scottish") to "Scotland". Spoiler alert there are a lot of these two words repeating 

SC_data_clean <- mutate_if(SC_data_clean,
                           is.character, 
                           str_replace_all, 
                           pattern = "[Ss]cottish", #searches for upper and lowercase to account for typos
                           replacement = "Scotland")#replace matches with "Scotland"



# Let's check the cleaned output:
head(SC_data_clean$texts)

#create a quanteda corpus of the 'article text' column from our data set:
article_text_SC<-corpus(SC_data_clean, text_field='texts')

#Wrap-up activity: comparisons & table discussion
#1. Follow the steps from the first lesson to analyse the Scotland data. When you are finished, compare the world clouds and the top-keys results between the Scotland and UK datasets.
#Discuss your findings with your table. 

#2. Looking at top_keys with your table along with the word clouds we've made so far (hint: you can scroll through the plots using the arrows in the top left corner of the window),
#what can keywords show us about a corpus? What do they not show us?

#3. Can we answer our research questions with the data we have? If not, what is still unknown? What information might we need to gather?

#4.Discuss the pros and cons of the text mining methods we've covered so far.
#bonus points for coming up with a potential use case in the context of your research!

