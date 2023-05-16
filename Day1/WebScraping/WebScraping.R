library(tidyverse)
library(rvest)

# The first page
page1 <- 'https://www.gov.uk/search/news-and-communications?keywords=cost%20of%20living&order=relevance'
# The first part of the urls
format <- 'https://www.gov.uk/search/news-and-communications?keywords=cost+of+living&order=relevance&page='
# the numbers for the rest of the pages (here we are using a sample so it's more managable later on)
nums <- 2:10 ##this is just a demo mention timeouts
#nums <- 2:632 #uncomment this later
pages <- paste0(format, nums)
pages <- c(page1, pages)

# Have a look at the beginning of the list
head(pages)

get.article.links <- function(x){
  links <- read_html(x) %>%
    html_nodes('.gem-c-document-list__item-title.govuk-link') %>%
    html_attr('href')
}


ArticleLinks <- map(pages, get.article.links)#Create a list of the first 200 Articles

head(ArticleLinks) #look at the top of it

# The solution is to address the second problem first, and then fix each of the urls

ArticleLinksFlat <- ArticleLinks %>% 
  flatten()

head(ArticleLinksFlat) #look at the top of it


# As we can see, 'https://www.gov.scot' is excluded from our links above, so just need to append each link with it.

base <- 'https://www.gov.uk'

ArticleLinksFlat <- paste0(base, ArticleLinksFlat)#Nb I am overwriting my previous variable

# Inspect this visually
head(ArticleLinksFlat)

length(ArticleLinksFlat)

# See what one entry looks like
ArticleLinksFlat[100]

# Create test set of links that will select only the first 5 links
test <- head(ArticleLinksFlat)
test

# Write a function to get titles. To identify the right html node you need to inspect any of the link pages 
get.title <- function(x){
  title <- read_html(x) %>% 
    html_node('.gem-c-lead-paragraph') %>%
    html_text()
}

# Test the function
ArticlesTest <- map(test, get.title)

# Run the test
ArticlesTest


#Because the date is store together with other info we need to work in steps to get the date 
# First we need to get to
# write a function to get the 2nd element of a list

second_element <- function(x){
  
  out <- x[2]
  
}# Square bracket are used to select by position. Remember that R start counting from 1 not from 0

# Now we can write a function to get the dates

get.date <- function(x){
  
  date <- read_html(x) %>%
    
    # This gets the block in which the author and date are stored together, the output will be a list of 2
    
    html_nodes('.gem-c-metadata__definition') %>%
    
    html_text()%>%
    
    # This gets the 2nd element
    
    second_element()
  
}


DatesTest <-map(test, get.date)


# Write a function to get the text
get.text <- function(x){
  body <- read_html(x) %>%
    html_node('.gem-c-govspeak') %>%
    html_text()
}


# Link them together
map(test, get.date)
map(test, get.text)

# Now that we know they work, time to apply them to the full data set. It is always a good idea to start small identify the right tag and then process the whole dataset 

# Note that a big issue with web scraping is that we can get blocked if we scrape too quickly, since websites take measures against people who access them too frequently. 

texts <- map(ArticleLinksFlat, get.text)#Get all the texts from the list of links

dates <- map(ArticleLinksFlat, get.date)#Get all the dates from the list of links

titles <- map(ArticleLinksFlat, get.title)#Get all the titles from the list of links

#Clean the Text because each Gov.uk Article contains a Further information section that is not present in the Scottish Gov 
# extract text up to the word "Further information"


# Remove text after "Further information"
clean_text <- sub(MyRegex, "", texts)

MyRegex<-"(?i)\n(further|more|additional)\\s+information.*$"


UKNews<- as.data.frame(cbind(dates,titles, clean_text))
UKNews$clean_text <-unlist(UKNews$clean_text)# transform texts from list to vector
UKNews$dates <-unlist(UKNews$dates)# transform dates from list to vector
UKNews$titles <-unlist(UKNews$titles)# transform titles from list to vector
UKNews$dates<-as.Date(UKNews$dates, format = "%d %B %Y")# make sure the date is encoded as date (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/as.Date)


write.csv(UKNews, "UKNews.csv")

# The first page
page1 <- 'https://www.gov.scot/news/'
# The first part of the urls
format <- 'https://www.gov.scot/news/?term=cost%20of%20living&cat=filter&page='
# the numbers for the rest of the pages (here we are using a sample so it's more managable later on)
nums <- 2:10 ##this is just a demo mention timeouts
#nums <- 2:632 #uncomment this later
pages <- paste0(format, nums)
pages <- c(page1, pages)

# Have a look at the beginning of the list
head(pages)

get.article.links <- function(x){
  links <- read_html(x) %>%
    html_nodes('.ds_search-result__link') %>%
    html_attr('href')
}

ArticleLinks <- map(pages, get.article.links)

head(ArticleLinks)

# The solution is to address the second problem first, and then fix each of the urls

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
ScotlandNews$dates<-as.Date(ScotlandNews$dates, format = "%d %B %Y %H:%M")# make sure the date is encoded as date (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/as.Date)

ScotlandNews



