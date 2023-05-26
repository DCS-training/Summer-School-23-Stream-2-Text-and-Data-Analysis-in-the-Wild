library(tidyverse)
library(rvest)
# The first thing we need to do is observe how the website is organised. In order to see easier the HTML tags on the page you can download the selector gadget extension to add to your browser

browseURL("https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb")

# Once we have that installed let's have a look to the news homepage 
browseURL("https://www.gov.uk/search/news-and-communications")

# We see that there is a search bar and we are going to use it to pull only the relevant news items using the keyword "Cost of living". 

# Note how by searching with the keyword the address of the page and that is what we are going to use so we copy and paste that below to create the nesting of the pages we want to scrape

# If we scroll to the bottom of the page we see there are multiple pages results let's see how the address for the second page looks like 

# note that it looks the same with the addition of &page = 2 etc.. so we need to map this in order to be able to scrape all pages results

# The first page
page1 <- 'https://www.gov.uk/search/news-and-communications?keywords=cost%20of%20living%20&order=relevance' # Define a new object with the website link for the first page 

# The first part of the urls for all the other pages results
format <- 'https://www.gov.uk/search/news-and-communications?keywords=cost+of+living&order=relevance&page='

# The numbers for the rest of the pages (here we are using a sample so it's more managable later on)
nums <- 2:15 # This is just a demo to not scrape too many pages 
#nums <- 2:500 # Uncomment this later if you want to scrape more 
pages <- paste0(format, nums)#  We now have a chr vector with the start of each page of the results (well the second to 15 pages)

pages <- c(page1, pages) # We add the first page 

# Have a look at the beginning of the list
head(pages)

# Now we need to start building the function to extract the info from our pages
# We can use selector gadget to help us identify the right html node [do this now]
get.article.links <- function(x){ # Put x, which stands in this case for a single URL, as the only argument for the function
  links <- read_html(x) %>% # Reads the HTML of the page into the environment
    html_nodes('.gem-c-document-list__item-title.govuk-link') %>% # Selects all instances of content matching a CSS selector
    html_attr('href') # Specifies that we want the URL from this content
}

# Now map our function over the list of URLs to create a list of the first 300 Articles
ArticleLinks <- map(pages, get.article.links)

head(ArticleLinks) #look at the top of it

# The solution is to address the second problem first, and then fix each of the urls

# flatten() takes a nested list (a list of lists) and turns it into a single list, following the order of the original list
# So if you have a list of lists of the names of students in classes, where the first entry is all the names of students in the first class, the second is the names of the students in the second class, etc,
# flatten() will turn this into a list of students, starting from the first student in the first class, without the nesting
ArticleLinksFlat <- ArticleLinks %>% 
  flatten()

head(ArticleLinksFlat) #look at the top of it


# As we can see, 'https://www.gov.uk' is excluded from our links above, so just need to append each link with it.
# Create a string with this text
base <- 'https://www.gov.uk'

ArticleLinksFlat <- paste0(base, ArticleLinksFlat) #Nb I am overwriting my previous variable

# Inspect this visually
head(ArticleLinksFlat)

# Get the length
length(ArticleLinksFlat)

# See what one entry looks like
ArticleLinksFlat[100]

# Now that we have the list of link we need to extract titles content and dates

# Again first of all we subset to make sure we are not going to be timed out

# Create test set of links that will select only the first 5 links using the head() function
test <- head(ArticleLinksFlat)
test

# Write a function to get titles. 
# To identify the right html node you need to inspect any of the link pages with the selector gadget
get.title <- function(x){
  title <- read_html(x) %>% # Get the HTML
    html_node('.gem-c-lead-paragraph') %>% # Select the first item with the chosen CSS selector
    html_text() # Specify that we want the text
}

# Test the function
ArticlesTest <- map(test, get.title) # This will actually do the scraping bit

# Run the test
ArticlesTest # The output looks alright, so now let's look at dates


#Because the date is stored together with other info we need to work in steps to get the date 

# First we need to write a function to get the 2nd element of a list
second_element <- function(x){ # Give the function one argument, which is the list in which we have the dates
  
  out <- x[2] # Select the 2nd element of the list
  # Square bracket are used to select by position. Remember that R start counting from 1 not from 0
  
}


# Now we can write a function to get the dates, including this previous function

get.date <- function(x){
  
  date <- read_html(x) %>% # Read the HTML of the page
    
    # This gets the block in which the author and date are stored together, the output will be a list of 2
    
    html_nodes('.gem-c-metadata__definition') %>%
    
    html_text() %>% 
    
    # Include the previous function at the end of the pipe, his gets the 2nd element
    second_element()
  
}


DatesTest <-map(test, get.date) # Again let's scrape the first ones so that we can check 


# Write a function to get the text of the article 
get.text <- function(x){
  body <- read_html(x) %>% # Get the HTML
    html_node('.gem-c-govspeak') %>% # Get the first object matching the CSS selector
    html_text() # Specify that we want the text
}

# Test the function
map(test, get.text)

# Now that we know they work, time to apply them to the full data set. It is always a good idea to start small identify the right tag and then process the whole dataset 

# Note that a big issue with web scraping is that we can get blocked if we scrape too quickly, since websites take measures against people who access them too frequently. 

texts <- map(ArticleLinksFlat, get.text)#Get all the texts from the list of links for the first 300 articles

dates <- map(ArticleLinksFlat, get.date)#Get all the dates from the list of links for the first 300 articles

titles <- map(ArticleLinksFlat, get.title)#Get all the titles from the list of links for the first 300 articles

#Clean the Text because each Gov.uk Articles contains a Further information section that is not present in the Scottish Gov 
# extract text up to the word "Further information"


# Remove text after "Further information"
# Generate the Regex function to remove anything past further info
MyRegex<-"(?i)\n(further|more|additional)\\s+information.*$"

clean_text <- sub(MyRegex, "", texts)

# Now we can finally bring them together in a dataframe 
UKNews<- as.data.frame(cbind(dates,titles, clean_text))
UKNews$clean_text <-unlist(UKNews$clean_text)# transform texts from list to vector
UKNews$dates <-unlist(UKNews$dates)# transform dates from list to vector
UKNews$titles <-unlist(UKNews$titles)# transform titles from list to vector
UKNews$dates<-as.Date(UKNews$dates, format = "%d %B %Y")# make sure the date is encoded as date (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/as.Date)

# Cost of Living is not a new concept so although is likely that most of the article we scraped are from the last couple of years let's subset out data-set to make sure all of them are connected to the ongoing issue

UkNewsLat3Years<- subset(UKNews, dates >= "2020-01-01") 
# now we have only 222 let's round to 200
UkNewsLat3Years<-UkNewsLat3Years %>% 
  arrange(desc(dates)) %>% #order by the latest dates
  slice(1:200)# Select the most recent 200 articles

#Export the file created 
write.csv(UkNewsLat3Years, "Day1/WebScraping/outputs/UKNews.csv")

# Now we do the same for the Scotland News ======================

# Some hint to start 
# The first page
page1 <- 'https://www.gov.scot/news/'
# The first part of the urls
format <- 'https://www.gov.scot/news/?term=cost%20of%20living&cat=filter&page='
# the numbers for the rest of the pages (here we are using a sample so it's more managable later on)
nums <- 2:30 ##this is just a demo mention timeouts. To get the same amount since in the scotland webpage is 10 per page 
#nums <- 2:632 #uncomment this later
pages <- paste0(format, nums)
pages <- c(page1, pages)

# Have a look at the beginning of the list
head(pages)
