# Libraries
library(tidyverse)
library(rvest)

# The first page
page1 <- 'https://www.gov.scot/news/'
# The first part of the urls
format <- 'https://www.gov.scot/news/?term=cost%20of%20living&cat=filter&page='
# the numbers for the rest of the pages (here we are using a sample so it's more managable later on)
nums <- 2:30 #
#nums <- 2:632 #uncomment this later
pages <- paste0(format, nums)
pages <- c(page1, pages)

# function for links
get.article.links <- function(x){
  links <- read_html(x) %>%
    html_nodes('.ds_search-result__link') %>%
    html_attr('href')
}

ArticleLinks <- map(pages, get.article.links)

head(ArticleLinks)

# Flatten links + add First bit of the link
ArticleLinksFlat <- ArticleLinks %>% 
  flatten()

head(ArticleLinksFlat)

# As we can see, 'https://www.gov.scot' is excluded from our links above, so just need to append each link with it.

base <- 'https://www.gov.scot'

ArticleLinksFlat <- paste0(base, ArticleLinksFlat)#Nb I am overwriting my previous variable

# Inspect this visually
head(ArticleLinksFlat)

length(ArticleLinksFlat)

# See what one entry looks like
ArticleLinksFlat[100]

# Create test set of links 
test <- head(ArticleLinksFlat)
test

# Write a function to get titles
get.title <- function(x){
  title <- read_html(x) %>% 
    html_node('.ds_page-header__title') %>%
    html_text()
}

# Test the function
ArticlesTest <- map(test, get.title)

# Run the test
ArticlesTest

# Write a function to get the date
get.date <- function(x){
  date <- read_html(x) %>%
    html_node('.ds_metadata__item:nth-child(1) .ds_metadata__value') %>%
    html_text()
}

# Write a function to get the text
get.text <- function(x){
  body <- read_html(x) %>%
    html_node('.ds_layout__content') %>%
    html_text()
}

# Link them together
map(test, get.date)

map(test, get.text)

# Now that we know they work, time to apply them to the full data set

# Note that a big issue with web scraping is that we can get blocked if we scrape too quickly, since websites take measures against people who access them too frequently. 

texts <- map(ArticleLinksFlat, get.text)

dates <- map(ArticleLinksFlat, get.date)

titles <- map(ArticleLinksFlat, get.title)

ScotlandNews<- as.data.frame(cbind(texts,dates,titles))
ScotlandNews$texts <-unlist(ScotlandNews$texts)# transform texts from list to vector
ScotlandNews$dates <-unlist(ScotlandNews$dates)# transform dates from list to vector
ScotlandNews$titles <-unlist(ScotlandNews$titles)# transform titles from list to vector
ScotlandNews$dates<-as.Date(ScotlandNews$dates, format = "%d %B %Y")# make sure the date is encoded as date (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/as.Date)

ScotlandNews

# Subset most recent articles 
# Cost of Living is not a new concept so although is likely that most of the article we scraped are from the last couple of years let's subset our data set to make sure all of them are connected to the ongoing issue
ScotlandNewsLast3Years <- subset(ScotlandNews, dates >= "2020-01-01")  # Subset so the date is greater than Jan 1st 2020
# Now we have only 222 articles let's round to 200
ScotlandNewsLast3Years <- ScotlandNewsLast3Years %>% 
  arrange(desc(dates)) %>% #order by the latest dates
  slice(1:200) # Select the most recent 200 articles (the first 200 in a list)

## Export the file created----------
write.csv(ScotlandNewsLast3Years, "Day1/WebScraping/outputs/ScotlandNews.csv")
