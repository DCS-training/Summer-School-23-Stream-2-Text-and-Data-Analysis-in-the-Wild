############### DATA VISUALISATION #######################
###### PART 1: Plots in R ######

# 1. Getting Set ###########
## 1.1 Packages we need ==========
library(tidyverse)
## 1.2 Import the data sets ===========
Scot_data <- read_csv("Day5/data/Full_Scottish_Data.csv")
Scot_data$Deprivation <-ifelse(Scot_data$SIMD>=median(Scot_data$SIMD),"least","most")# We are going to need one more binary variable that will divide our dataset in most deprived/less deprived based on the median
authority_data<-read_csv("Day5/data/authority_data_cleaned.csv")

# 2. Basic Visualisation ################
# Base R has a series of ways to visualise data.

## 2.1 Structured Visualisation ====================
# We can use functions to more easily pull information from our data at a glance.
str(authority_data) # Basic structural information of the data set
head(authority_data) # View the first 10 observations
tail(authority_data) # View the last 15 observations
summary(authority_data) # More detailed summary of the data. This saves using individual functions for each column eg.-mean(airquality$Temp)

View(authority_data) # A more user friendly view of the data frame (you can also click on the variable in the environment)

## 2.2 Basic Plots ================
# A more effective way to visualise all of this data can be through graphs, or plots

# The base graph functions in R come from the default 'graphics' package
help(graphics)
library(help = "graphics")

plot(authority_data$WelfareApp) # Basic scatter plot of a specific variable
plot(Scot_data$life_expectancy_2022, Scot_data$SIMD) # Basic scatter plot showing the relationship between two variables
# With this, we can see that there is a positive correlation between life expectancy and SIMD ranking, eg., with lower levels of deprivation, life expectancy is higher.

plot(authority_data) # We can also see a matrix of plots across the entire data frame (this looks silly with so much data)

# Base Plot has deeper customization available, see-
browseURL("https://towardsdatascience.com/a-guide-to-data-visualisation-in-r-for-beginners-ef6d41a34174")

# However, the level of detail and customization afforded by ggplot2 is far greater

# 3.  GGPlot and the Grammar of Graphics ################
# The main package we will be using for data visualisation is ggplot2, included in the tidyverse package
help(ggplot)

## 3.1 Basic Syntax ======================
# Lets visualise the same scatter plot above using ggplot/
ggplot(Scot_data, aes(x= SIMD, y= life_expectancy_2022)) + geom_point() # While this may seem slightly more complicated than basic plot functions for a very similar result, it allows far more customization and the syntax is actually fairly straightforward

# ggplot syntax follows the 'grammar of graphics'. See-
browseURL("https://r-unimelb.gitbook.io/rbook/putting-the-r-in-art/ggplot2-and-the-grammar-of-graphics")
browseURL("https://www.springer.com/in/book/9780387245447")

# ggplot is called, as with any function, using 'ggplot()'. The first argument is the data itself 'Scot_data'. The second is aesthetics 'aes()'. Aesthetics takes additional arguments, in this case, the data to plot on the x 'SIMD' and y 'life_expectancy_2022' axes. Finally, we include a separate function for building the geometry of the plot, in this case 'geom_point()', to create a scatter/dot plot.

Basic_plot <- ggplot(Scot_data, aes(SIMD, life_expectancy_2022, shape = location)) # Additional elements can be added to aesthetics, eg. shape, in this case based on whether the authority is Urban or Rural.

Basic_plot + geom_point()

Basic_plot + geom_point(aes(colour = region), size = Scot_data$Population/100000) # Aesthetic values and size can also be added to the geometry (note, size takes raw values, so we divided it by 100,000 here so each point doesn't take up the entire plot!)

Basic_plot + geom_point(aes(colour = region), size = 4) + geom_point(colour  =  "grey90", size  =  1.5) # Additional plots can overlay those that come earlier in the statement, using the same data and aesthetic

# The basic syntax is the same for any plot in ggplot
Basic_plot <- ggplot(Scot_data, aes(food_insecurity2018_2022)) 
Basic_plot + geom_bar(fill = 'indianred3', colour="black") # A barplot. Note that only one axis is plotted, as the y (by default) is a count

Basic_plot + geom_histogram(colour = 'black', fill = 'indianred3', binwidth = sd(Scot_data$food_insecurity2018_2022)/3) # Note that colour dictates the outline of the histogram bins, while fill changes the colour. bandwidth and bins lets you set the width of each bin, or the number of bins. (Bandwidth overrides bins)
Basic_plot + geom_histogram(colour = 'black', fill = 'indianred3', bins = 10)


## 3.2. Greater Customization with ggplot2 ==================
# These are fairly basic plots, but a great level of customization is possible with ggplot2

### 3.2.1 Working with the Theme ---------------
# The theme() argument allows for deep customization of non-data components of plots 
HousePrices <- ggplot(authority_data, aes(HousePrices)) # We can also check the general trend of how house prices have icnreased from 2021-2022.

HousePrices  + geom_histogram(colour = 'gray2', fill = 'indianred3', bins = 20) + theme_bw() 

HousePrices  + geom_histogram(colour = 'gray2', fill = 'indianred3', bins = 20) + theme_dark()

HousePrices  + geom_histogram(colour = 'gray2', fill = 'indianred3', bins = 20) +  theme_void()

# The increases have a roughly normal distribution, with an average increase of around 12%, but with some being considerably higher. 

# This site contains a list of possible ready made themes, but standard themes can be much more minutely adjusted
browseURL("https://ggplot2.tidyverse.org/reference/ggtheme.html")

### 3.2.2 Multidimensional Data ---------
ggplot(authority_data, aes(life_expectancy_2022, Alcohol, shape = as.factor(SIMDQuint))) + # we need to use factor because when we re-import the data R encoded SIMD Quint as numeric again
  geom_point(aes(colour = location), size = 4) # In this scatter plot, we used the SIMD Quantile variable to change the shape and 'location' to change the colour of the data, adding more dimensions to the plot.

# This appears to show a negative relationship between alcohol abuse/deprivation and life expectancy. 

HousePrices  + geom_histogram(aes(fill = as.factor(SIMDQuint)), colour = 'gray2', bins = 20) # Note aesthetics using variable rather than preset colours require to be within the aesthetic 'aes()' argument.

# This shows a fairly even spread across deprivation levels for the increase in house prices.

### 3.2.3. Legends ---------------
# Legends can be customized using theme() in multiple ways, including setting position or removing it altogether
HousePrices <- ggplot(authority_data, aes(HousePrices)) # Let's create a plot showing the change in house prices across the different areas of Scotland, as a percentage. We did it already for the authority-data but this is to show that you can do it on the fly too

HousePrice_SIMD <- HousePrices +
  geom_histogram(aes(fill = as.factor(SIMDDecil)), colour = 'gray2', bins = 40)

HousePrice_SIMD

HousePrice_SIMD +
  theme(legend.position = "none") # Remove legend

HousePrice_SIMD +
  theme(legend.position = "top") # Move legend above plot

HousePrice_SIMD +
  theme(legend.position = "top", legend.title=element_blank()) # Remove legend title

# The formatting of the legend can also be adjusted in more detail
HousePrice_SIMD  +
  theme_bw() +
  theme(legend.position = "top", legend.title=element_text(colour = "white", size = 10, face="bold"), legend.text=element_text(colour = "white", size = 8), legend.background = element_rect(fill = "black"), legend.key = element_rect(fill = "black")) + labs(fill = "SIMD Deciles", x = "Avereage House Costs as a Percentage")
# element_ allows specific non-data components of plots to be adjusted. eg. element_text for text and elecment_rect for borders and backgrounds

# These plots seem to show that houses cost increase across all SIMD deciles and the highest increases are mostly on higher deciles areas 

### 3.2.4. Labels --------------
# Labels can also be easily customized in ggplot2
Histogram <- HousePrices +
  geom_histogram(aes(fill = as.factor(SIMDDecil)), colour = 'gray2', bins = 40) +
  theme_bw() +
  theme(legend.position = "top", legend.title=element_text(colour = "white", size = 10, face="bold"), legend.text=element_text(colour = "white", size = 8), legend.background = element_rect(fill = "black"), legend.key = element_rect(fill = "black"))

Histogram + labs(title="House Costs Change and SIMD", x="Average House Cost Change", y="Number of Authorities", fill="SIMD Decile")

# labs() allows labels to be customised. For more detail see:
browseURL("https://ggplot2.tidyverse.org/reference/labs.html")

# Labels can also be added onto the plot itself
Histogram + labs(title="House Costs Change and SIMD Deciles", x="Average House Cost Change (%)", y="Number of Authorities", fill="SIMD Deciles") +
  ggplot2::annotate(geom="label",x=c(14,20), y=c(4.2,1.5), label= c("Average Increase", "Higher Increases"), fill="black", colour=c("steelblue1", "tomato3"))
 
# annotate() allows labels to be added with the x and y position on the plot and the label itself required
# rect can also be used to highlight a specific area
ggplot(Scot_data, aes(SIMD, life_expectancy_2022, shape = location)) +
  geom_point(aes(colour = region), size = 4) +
  ggplot2::annotate("rect", xmin=3500, xmax=3675, ymin=77.3, ymax=78, alpha=0.5)  # note that alpha allows the transparency of an object to be set

# segment can add a custom line
ggplot(Scot_data, aes(SIMD, life_expectancy_2022, shape = location)) + geom_point(aes(colour = region), size = 4) +
  ggplot2::annotate(geom="segment", x=2900, xend=3800, y=74, yend=80, colour="plum4", lwd=1.5, alpha=0.5) # lwd sets the line width

library(ggrepel) # Lets add more useful labels.

Scatter_plot <- ggplot(Scot_data, aes(SIMD, life_expectancy_2022, shape = location)) + geom_point(aes(colour = region), size = 4) + 
  geom_label_repel(aes(label = authority),
                   size = 2.5,
                   box.padding   = 0.2, 
                   point.padding = 0.4,
                   segment.color = 'grey50') # ggrepel allows labels to be added that won't clash with one another.

Scatter_plot # Now we can see which specific authorities are the most deprived, with Glasgow City having the highest levels of deprivation and the lowest life expectancy. (It might be easier to see if you click 'zoom').

### 3.2.5. Colours ----------------------
# The default colours in R aren't necessarily always the best options, but this too can be customized
browseURL("https://r-charts.com/colors/") # Default colours in R

custom_colours <- c('olivedrab3', '#EEC900', 'firebrick3', '#2FA3C8')

Scatter_plot +
  scale_colour_manual(values = custom_colours)

# Pre-set colour palettes can also be used
library(viridis)
Scatter_plot +
  scale_colour_viridis(discrete = TRUE)

# For information on available colour palettes see
browseURL("https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/")

### 3.2.6. Facet Wrapping --------------
# Facet wrapping can allow for more obvious comparison between specified attributes
Scatter_plot +
  facet_wrap(~region)
Scatter_plot +
  facet_wrap(~region, ncol = 4, dir = 'v')


## 3.3. Different Types of Plots =======
# So far we have looked mainly at scatter plots and histograms. There are many more than this, we can look at some of the main other options now, but for greater detail for many available plots see:
browseURL("http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html")

### 3.3.1. Scatter plot --------------
Scatter_plot # This plot is very commonly used and particularly useful for identifying correlations. In this case,a positive correlation between deprivation and life expectancy.

### 3.3.2. Histogram---------------
ggplot(authority_data, aes(Alcohol)) +
  geom_histogram(aes(fill = as.factor(SIMDDecil)), colour = 'gray2', bins = 20) +
  theme_bw() # Histograms are particularly useful for plotting distributions and detecting whether or not the plot is normally distributed. With sensible use of fill, different attributes can also be easily compared. This plot seems to suggest the more deprived authorities generally see greater levels of alcohol abuse. 


### 3.3.3. Density Plot -------------------
ggplot(Scot_data, aes(Alcohol)) +
  geom_density() +
  facet_wrap(~Deprivation) # This is similar to a histogram, in that it can show peaks and troughs in the distribution, but with a density plot, the precise shape of each factors distribution can be more easily visualised. This shows a large peak among the least deprived authorities at a low level of alcohol abuse, the peak is less pronounced but at higher levels of alcohol abuse for the more deprived authorities.

### 3.3.4 Boxplots -----------------
ggplot(Scot_data, aes(x= Deprivation, y= life_expectancy_2022, fill= location)) +
  geom_boxplot() # This plot allows for distributions, means, range and outliers to be very easily identified. The mean is the horizontal line across each box and the top section of each box the 75%ile, with the bottom section 25%ile. The colour or shape of outliers can be altered with outlier. Or even removed eg.
ggplot(Scot_data, aes(x= Deprivation, y= life_expectancy_2022, fill= location)) +
  geom_boxplot(outlier.shape = NA)

boxplot.stats(Scot_data$life_expectancy_2022)

###### PART 2: Practicals 1 ######
# ==== Practical 1 ====
# Try to visualise any correlation between average gas consumption and energy bills. Compare the most and least deprived areas as well as the urban/rural divide.

# ==== Practical 2 ====
# Visualise the difference in journey times using public transport to a GP. Look at regional differences.

# ==== Practical 3 ====
# Expand on the above, but divide by rural and urban.

###### PART 3: Advanced Data Visualisation ######
# 1. Spatial Data ######
# Spatial data is requires more complex to work with in R. This will just provide a brief example of how spatial data can be visualise din R, but it is important to understand the complexities of spatial data if you want to take this further.

## 1.1. Getting setup ===========
### 1.1.1 Additional Libraries needed --------------
library(rgdal)
library(sp)
library(sf)
library(gridExtra)
library(latticeExtra)

### 1.1.2 Import Simple Shapefiles ------------------
Local_auth <- st_read("Day5/data/Spatial/Local_auth.shp") # st_read() is part of the sf (Simple Features) package, which allows us to import shapefiles and their attributes. Look at the Local_auth object. It contains much of the same data as we have in our csv file, but also contains spatial data.

Local_auth$geometry # This provides some of the geographic data.

### 1.1.3 basic visualisation ----------------
plot(st_geometry(Local_auth)) # st_geometry() allows us to plot only the spatial data. But we can visualise different, non-spatial data.

plot(Local_auth[1]) # Plot the first column, just the authorities.
plot(Local_auth[4]) # We can also visualise the rural urban divide.

plot(st_geometry(Local_auth), axes = T) # We can also included axes.


## 1.2. spplot ======
# spplot is a more advanced package for analysing larger more complex spatial data. We need to re-import the local authority shape file using readOGR (part of the rgdal package) in order for the object to be compatible with spplot.
Scot <- readOGR(dsn="Day5/data/Spatial", layer = "Local_auth") # note that when using readOGR, we do not include the file extension, as multiple different types of file are being used

spplot(Scot, zcol = 'SIMD') # Using sspolt, we can define a z column, which can help us to visualise additional data. In this case the deprivation rank of each authority.
Scot$region <- as.factor(Scot$region) # Make sure region is a factor.
spplot(Scot, zcol = 'region')

# We can also again calculate increase/decrease and look at % of increase/decrease of a factor
Scot$homeless_1 <- as.integer(Scot$homeless_1)# make sure is encoded as integer number
Scot$homelessne <- as.integer(Scot$homelessne) # make sure is encoded as integer number
Scot$HomelessIncrease <- ((Scot$homeless_1-Scot$homelessne)/Scot$homelessne)*100 # calculate %increase/decrease
spplot(Scot, zcol = 'HomelessIncrease')

# ==== Practical 4 ====
# Play around and visualise some of the factors we have looked at earlier in the day, but in a spatial format.



# 2. Visualise Textual Data with Animated Graphs ################
## 2.2 Word Frequency======================
### 2.2.1 Setting up ---------------
# Additional Libraries needed
library(gganimate)
library(gifski)
library(av)
# Re-import file 
uk_data_clean <- read_csv("Day5/data/TextDataVis.csv")

### 2.2.2 Data Wrangling ------------
# Most Recurrent words each month 
# Create a new column that will have Month and year 
uk_data_clean$MonthYear <- format(as.Date(uk_data_clean$dates, format="%Y-%m-%d"),"%Y-%m")
# Group articles by month 
uk_data_clean_G<-uk_data_clean%>%
  group_by(MonthYear)%>%
  summarize(clean_text=paste0(clean_text, collapse="\r\n"), titles=paste0(titles,collapse="\r\n"))

### 2.2.3 Preprocess the text data (We are using tidytext)----------
library(tidytext)
library(textstem)
processed_df <- uk_data_clean_G %>% # what I am using
  mutate(clean_text = tolower(clean_text)) %>% # lower all words
  unnest_tokens(word, clean_text) %>% # tokenise
  mutate(word = lemmatize_words(word))%>% # lemmatise
  filter(!word %in% stop_words$word) # remove stop words 

# Filter for the top recurring keywords overall 
top_keywords <- processed_df %>%
  count(word) %>%
  top_n(15, n) %>% # Filter for the top 15
  arrange(desc(n))

# Count the keywords by date 
keyword_by_date <- processed_df %>%
  count(MonthYear, word) %>%
  inner_join(top_keywords, by = "word")%>%
  mutate(Ntotal=n.y, Nmonth=n.x)%>%
  arrange(desc(Ntotal))

### 2.2.3  Create an animated stacked bar chart  -------------------
animation<-ggplot(keyword_by_date, aes(x = word, y = Nmonth, fill = word)) +
  geom_col(colour="black") + # border of the bars
  theme_minimal() + # remove almost all graph annotation
  coord_flip()+ # rotate of 90 degree
  theme(legend.position = "none",axis.title=element_text(size=16,face="bold"), axis.text = element_text(size=12),plot.title = element_text(size=22, face="bold")) + # Adjust the legend 
  labs(x = "Keyword", y = "Count", fill = "Keyword") + # Rename the labels
  scale_fill_viridis_d(option = "plasma", direction = -1)+ # Use the Viridis Palette
  transition_states(
    states = MonthYear,
    transition_length = 2,
    state_length = 1) + # Define the parameters of the animation
  enter_fade() + # Define type of animation entrance
  exit_fade()+ # Define type of animation exit
  labs(title = 'Month-Year : {closest_state}')# generate an animate legend

# Animate and Save 
animate(animation, height = 1000, width =800,fps = 1.5)# Specify details animation
anim_save("Day5/outputs/keyword_animation.gif")# Export the graph as a gif



## 2.3 Time Series of the Sentiment Analysis=======
### 2.3.1 Data Wrangling ------------
# Re-import datasets 
sentiment_scores<-read_csv("Day5/data/Sentiment_scores.csv")
SentimentScotland<-read_csv("day3/SentimentAnalysis/data/scotland_data.csv")

# Merge back with the original data set
WithSentiment<- cbind(sentiment_scores,SentimentScotland)
str(WithSentiment)
# Transform POSIX CT in Date
WithSentiment$Date<-as.Date(as.POSIXct(WithSentiment$created_utc, 'GMT'))
WithSentiment$MonthYear <- format(as.Date(WithSentiment$Date, format="%Y-%m-%d"),"%Y-%m")
# Rework the data set to be able to plot bar plot
ToPlot <-WithSentiment[,c(1:8,16)]#select the first 8 sentiments+MonthYear
LongPlot<-ToPlot %>% #re-shape the dataset to follow the tidy data rules
  pivot_longer(
    cols = !MonthYear,
    names_to = "Sentiment",
    values_to = "count"
  )
# Group results by month-year
ByMonthYear<-LongPlot%>%
  group_by(MonthYear,Sentiment)%>%
  summarise(count=round(mean(count)))

### 2.3.2 Create an animated visualisation -----------------
# Make a ggplot, but add frame=month: one image per month-year
animationSent<-ggplot(ByMonthYear, aes(x=Sentiment, y=count, fill=Sentiment)) +
  geom_col(colour="black") +# border of the bars
  theme_minimal()+ # minimise theme
  theme_bw() + #B/w background
  coord_flip()+ # Flip the coordinates
  theme(legend.position = "none",axis.title=element_text(size=16,face="bold"), axis.text = element_text(size=12),plot.title = element_text(size=22, face="bold")) + # Adjust the legend 
  geom_text(aes(label = count, hjust=0))+
  labs(x = "Sentiment", y = "Count", fill = "Sentiment") + # Rename the labels
  scale_fill_viridis_d(option = "plasma", direction = -1)+ # Use the Viridis Palette
  # gganimate specific bits:
  transition_states(
    states = MonthYear,
    transition_length = 2,
    state_length = 1) + # Define the parameters of the animation
  enter_fade() + # Define type of animation entrance
  exit_fade()+ # Define type of animation exit
  labs(title = 'Month-Year : {closest_state}')# generate an animate legend

# Animate and Save 
animate(animationSent, height = 1000, width =800,fps = 1.5)# Specify details animation
anim_save("Day5/outputs/Sentiment_animation.gif")# Export


# Data visualisation is a huge part of R, this has just scratched the surface, but hopefully gives the basics so you can look at more specific and advanced visualisations for your own data in the future.


##### END ######
