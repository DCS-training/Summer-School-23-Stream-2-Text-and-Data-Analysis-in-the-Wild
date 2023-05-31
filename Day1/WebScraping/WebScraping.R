############### WEB SCRAPING ##########################
# RESEARCH QUESTIONS #######
# 1. Can I automatically extract news items from the UK Government website News sections on the cost of living?
# 2. Can I do the same for the Scottish Government website?

# PART1: Scraping the UK Government Website ###########
# 1. Getting Setup ====================
## 1.1. Libraries needed--------------
install.packages("rvest")
library(tidyverse)
library(rvest)
## 1.2. Analyse the structure of the website we want to use -------
# The first thing we need to do is observe how the website is organised. In order to see easier the HTML tags on the page you can download the selector gadget extension to add to your browser
browseURL("https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb")

# Once we have that installed let's have a look to the news homepage 
browseURL("https://www.gov.uk/search/news-and-communications")

# We see that there is a search bar and we are going to use it to pull only the relevant news items using the keyword "Cost of living". 
# Note how by searching with the keyword the address of the page and that is what we are going to use so we copy and paste that below to create the nesting of the pages we want to scrape

# If we scroll to the bottom of the page we see there are multiple pages results let's see how the address for the second page looks like 

# Note that it looks the same with the addition of &page = 2 etc.. so we need to map this in order to be able to scrape all pages results

# 2. Create List of URL ===================
# URL starts 
page1 <- 'https://www.gov.uk/search/news-and-communications?keywords=cost%20of%20living%20&order=relevance' # Define a new object with the website link for the first page 

# The first part of the urls for all the other pages results
format <- 'https://www.gov.uk/search/news-and-communications?keywords=cost+of+living&order=relevance&page='

# The numbers for the rest of the pages (here we are using a sample so it's more manageable later on)
nums <- 2:15 # This is just a demo to not scrape too many pages 

#nums <- 2:500 # Un-comment this later if you want to scrape more 
pages <- paste0(format, nums)#  We now have a chr vector with the start of each page of the results (well the second to 15 pages)

pages <- c(page1, pages) # We add the first page 

# Have a look at the beginning of the list
head(pages)

# 3. Getting the list of Links ===========
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

# 4. Extract information from links ==============
# Now that we have the list of link we need to extract titles content and dates

# Again first of all we subset to make sure we are not going to be timed out

## 4.1. Create a test environment ------------------
# Create test set of links that will select only the first 5 links using the head() function
test <- head(ArticleLinksFlat)
test

### 4.1.1 Function for Titles -------------
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

### 4.1.2 Function for Dates -------------
# Because the date is stored together with other info we need to work in steps to get the date 

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

### 4.1.3 Function for Text-------------
# Write a function to get the text of the article 
get.text <- function(x){
  body <- read_html(x) %>% # Get the HTML
    html_node('.gem-c-govspeak') %>% # Get the first object matching the CSS selector
    html_text() # Specify that we want the text
}

# Test the function
map(test, get.text)

## 4.2 Scrape the whole dataset---------------
# Now that we know they work, time to apply them to the full data set. It is always a good idea to start small identify the right tag and then process the whole dataset 
# Note that a big issue with web scraping is that we can get blocked if we scrape too quickly, since websites take measures against people who access them too frequently. 
texts <- map(ArticleLinksFlat, get.text)#Get all the texts from the list of links for the first 300 articles
dates <- map(ArticleLinksFlat, get.date)#Get all the dates from the list of links for the first 300 articles
titles <- map(ArticleLinksFlat, get.title)#Get all the titles from the list of links for the first 300 articles

# 5. Clean the results & Export =====================
# Clean the Text because each Gov.uk Articles contains a Further information section that is not present in the Scottish Gov 
# Extract text up to the word "Further information"
## 5.1 Remove text after "Further information"---------
# Write a Regex function to remove anything past further info
MyRegex<-"(?i)\n(further|more|additional)\\s+information.*$"
# sub() applies the regex to the texts, replacing it with nothing (ie. "")
clean_text <- sub(MyRegex, "", texts)

## 5.2 Create a new Data Frame---------------
# Now we can finally bring them together in a data frame 
UKNews<- as.data.frame(cbind(dates,titles, clean_text))
UKNews$clean_text <-unlist(UKNews$clean_text) # Transform texts from list to vector
UKNews$dates <-unlist(UKNews$dates) #Transform dates from list to vector
UKNews$titles <-unlist(UKNews$titles) #Transform titles from list to vector
UKNews$dates<-as.Date(UKNews$dates, format = "%d %B %Y") #Make sure the date is encoded as date (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/as.Date)

## 5.3 Subset most recent articles ----------
# Cost of Living is not a new concept so although is likely that most of the article we scraped are from the last couple of years let's subset our data set to make sure all of them are connected to the ongoing issue
UkNewsLast3Years <- subset(UKNews, dates >= "2020-01-01")  # Subset so the date is greater than Jan 1st 2020
# Now we have only 222 articles let's round to 200
UkNewsLast3Years <- UkNewsLast3Years %>% 
  arrange(desc(dates)) %>% #order by the latest dates
  slice(1:200) # Select the most recent 200 articles (the first 200 in a list)

## 5.4 Export the file created----------
write.csv(UkNewsLast3Years, "Day1/WebScraping/outputs/UKNews.csv")


# PART2: Exercise ############
# Now we do the same for the Scotland News 
# As a group, scrape the Scottish government news website and create a DF comparable to the UK one
# Some hint to start 
# The first page
page1 <- 'https://www.gov.scot/news/'
# The first part of the urls
format <- 'https://www.gov.scot/news/?term=cost%20of%20living&cat=filter&page='

# Write your code below here
