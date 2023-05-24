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

SC_data_clean <- mutate_if(SC_data_clean, #change if is character so titles and texts
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

# Let see the same for all of them and save it as new vector and then create a new dataframe with three columns (Ntoken, Dataset, Date)
NtokenSC<-as.vector(ntoken(article_text_SC))
TokenSC <-data.frame(Tokens=NtokenSC, Dataset="Scotland", Date=SC_data_clean$dates)
# The first thing we can do is look if there is an increase of number of articles and length of articles ---------------
# First let's extract Year and Month from the dates 
TokenSC$MonthYear <- format(as.Date(TokenSC$Date, format="%Y-%m-%d"),"%Y-%m")
# Now we can group by Month Year and count both how many articles and total number of token/month 
BreakoutSC<- TokenSC %>%
  group_by(MonthYear,Dataset)%>%
  summarize(NArticles=n(), MeanTokens=round(mean(Tokens)))
# Now we can plot the trends
ggplot(BreakoutSC, aes(x=MonthYear, y=NArticles))+ # Select data set and coordinates we are going to plot
  geom_point(aes(size=MeanTokens, fill=MeanTokens),shape=21, stroke=1.5, alpha=0.9, colour="black")+ # Which graph I want 
  labs(x = "Timeline", y = "Number of Articles", fill = "Mean of Tokens", size="Mean of Tokens", title="Number of Articles and Tokens in the Scottish Gov Website")+ # Rename labs and title
  geom_path(aes(group=1), colour="black", size=1)+ # Add a line that will connect the dots 
  scale_size_continuous(range = c(5, 15))+ # Resize the dots to be bigger
  geom_text(aes(label=MeanTokens))+ # Add the mean of tokens in the dots
  scale_fill_viridis_c(option = "plasma")+ # Change the colour coding
  theme_bw()+ # B/W Background
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+ # Rotate labels of x and move them slightly down. Plus move the position to the bottom 
  guides(size = "none") # Remove the Size from the Legend 

# Bring together the two breakout datasets
TotBreakout <-rbind(BreakoutSC,BreakoutUK)

# Now replot the two together
ggplot(TotBreakout, aes(x=MonthYear, y=NArticles))+ # Select data set and coordinates we are going to plot
  geom_point(aes(size=MeanTokens, fill=MeanTokens),shape=21, stroke=1.5, alpha=0.9, colour="black")+ # Which graph I want 
  labs(x = "Timeline", y = "Number of Articles", fill = "Mean of Tokens", size="Mean of Tokens", title="Number of Articles and Tokens in the Scotland and UK Gov Website")+ # Rename labs and title
  geom_path(aes(group=1),  size=1, colour="black")+ # Add a line that will connect the dots 
  scale_size_continuous(range = c(5, 15))+ # Resize the dots to be bigger
  geom_text(aes(label=MeanTokens))+ # Add the mean of tokens in the dots
  scale_fill_viridis_c(option = "plasma")+ # Change the colour coding
  theme_bw()+ # B/W Background
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+ # Rotate labels of x and move them slightly down. Plus move the position to the bottom 
  guides(size = "none")+# Remove the Size from the Legend 
  facet_wrap(~Dataset, nrow=2)



#tokenise the corpus 
article_tokens_SC <- tokens(article_text_SC, 
                         remove_symbols=TRUE, 
                         remove_url=TRUE, 
                         remove_punct=TRUE)

#remove tokens under 3 characters:
article_tokens_SC <- tokens_select(article_tokens_SC, min_nchar = 3)


Try <-data.frame(text = sapply(article_text_SC, as.character), stringsAsFactors = FALSE)

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
