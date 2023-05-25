# #### Data Visualisation ####
# #### PART 1: Plots in R ####

library(tidyverse)

Scot_data <- read_csv("Data/Full_Scottish_Data.csv")

# ==== Basic Visualisation ====
# Base R has a series of ways to visualise data.

# ---- Structured Visualisation ----

# We can use functions to more easily pull information from our data at a glance.

str(Scot_data) # Basic structural information of the dataset
head(Scot_data) # View the first 10 observations
tail(Scot_data) # View the last 15 observations
summary(Scot_data) # More detailed summary of the data. This saves using individual functions for each column eg.-mean(airquality$Temp)

View(Scot_data) # A more user friendly view of the dataframe (you can also click on the variable in the environment)

# ---- Basic Plots ----
# A more effective way to visualise all of this data can be through graphs, or plots

# The base graph functions in R come from the default 'graphics' package
help(graphics)
library(help = "graphics")

plot(Scot_data$Alcohol) # Basic scatter plot of a specific variable
plot(Scot_data$life_expectancy_2022, Scot_data$SIMD) # Basic scatter plot showing the relationship between two variables
# With this, we can see that there is a positive correlation between life expectancy and SIMD ranking, eg., with lower levels of deprivation, life expectancy is higher.

plot(Scot_data) # We can also see a matrix of plots across the entire dataframe (this looks silly with so much data)

# Base Plot has deeper customisation available, see-
browseURL("https://towardsdatascience.com/a-guide-to-data-visualisation-in-r-for-beginners-ef6d41a34174")

# However, the level of detail and customisation afforded by ggplot2 is far greater

# ---- Basic ggplot2 Syntax ----

# The main package we will be using for data visualisation is ggplot2, included in the tidyverse package
help(ggplot)

# Lets visualise the same scatter plot above using ggplot/
ggplot(Scot_data, aes(x= SIMD, y= life_expectancy_2022)) + geom_point() # While this may seem slightly more complicated than basic plot functions for a very similar result, it allows far more customisation and the syntax is actually fairly straightforward

# ggplot syntax follows the 'grammar of graphics'. See-
browseURL("https://r-unimelb.gitbook.io/rbook/putting-the-r-in-art/ggplot2-and-the-grammar-of-graphics")
browseURL("https://www.springer.com/in/book/9780387245447")

# ggplot is called, as with any function, using 'ggplot()'. The first argument is the data itself 'Scot_data'. The second is aesthetics 'aes()'. Aesthetics takes additional arguments, in this case, the data to plot on the x 'SIMD' and y 'life_expectancy_2022' axes. Finally, we include a separate function for building the geometry of the plot, in this case 'geom_point()', to create a scatter/dot plot.

Basic_plot <- ggplot(Scot_data, aes(SIMD, life_expectancy_2022, shape = location)) + geom_point() # Additional elements can be added to aesthetics, eg. shape, in this case basedon whether the authority is Urban or Rural.

Basic_plot

Basic_plot + geom_point(aes(colour = region), size = Scot_data$Population/100000) # Aesthetic values and size can also be added to the geometry (note, size takes raw values, so we dividde it by 100,000 here so each point dosen't take up the entire plot!)

Basic_plot + geom_point(aes(colour = region), size = 4) + geom_point(colour  =  "grey90", size  =  1.5) # Additional plots can overlay those that come earlier in the statement, using the same data and aesthetic

# The basic syntax is the same for any plot in ggplot
Basic_plot <- ggplot(Scot_data, aes(`food_insecurity2018-2022`)) 
Basic_plot + geom_bar(fill = 'indianred3') # A barplot. Note that only one axis is plotted, as the y (by default) is a count

Basic_plot + geom_histogram(colour = 'black', fill = 'indianred3', binwidth = sd(Scot_data$`food_insecurity2018-2022`)/4) # Note that colour dictates the outline of the historgram bins, while fill changes the colour. binwidth and bins lets you set the width of each bin, or the number of bins. (Binwidth overrides bins)
Basic_plot + geom_histogram(colour = 'black', fill = 'indianred3', bins = 10)


# ==== Greater Customisation with ggplot2 ====
# These are fairly basic plots, but a great level of customisation is possible with ggplot2

# The theme() argument allows for deep customisation of non-data components of plots 
Alcohol_plot <- ggplot(Scot_data, aes(Alcohol))

Alcohol_plot + geom_histogram(colour = 'gray2', fill = 'indianred3', bins = 20) + theme_bw() 

Alcohol_plot + geom_histogram(colour = 'gray2', fill = 'indianred3', bins = 20) + theme_dark()

Alcohol_plot + geom_histogram(colour = 'gray2', fill = 'indianred3', bins = 20) +  theme_void()

# This site contains a list of possible ready made themes, but standard themes can be much more minutely adjusted
browseURL("https://ggplot2.tidyverse.org/reference/ggtheme.html")

# ---- Multidimensional Data ----
ggplot(Scot_data, aes(life_expectancy_2022, Alcohol, shape = Deprivation)) + geom_point(aes(colour = location), size = 4) # In this scatter plot, we used the 'Deprivation' variable to change the shape and and 'location' to change the colour of the data, adding more dimensions to the plot.

Alcohol_plot + geom_histogram(aes(fill = Deprivation), colour = 'gray2', bins = 20) # Note aesthetics using variable rather than preset colours require to be within the aesthetic 'aes()' argument.

# ---- Legends ----
# Legends can be customised using theme() in multiple ways, including setting position or removing it altogether
Rent_plot <- ggplot(Scot_data, aes((average_rent_2022-average_rent_2021)/average_rent_2021)) # Let's create a plot showing the change in rent between the most and least deprived authorities, as a percentage.

Rent_plot + geom_histogram(aes(fill = Deprivation), colour = 'gray2', bins = 40)

Rent_plot + geom_histogram(aes(fill = Deprivation), colour = 'gray2', bins = 40) + theme(legend.position = "none") # Remove legend
Rent_plot + geom_histogram(aes(fill = Deprivation), colour = 'gray2', bins = 40) + theme(legend.position = "top") # Move legend above plot
Rent_plot + geom_histogram(aes(fill = Deprivation), colour = 'gray2', bins = 40) + theme(legend.position = "top", legend.title=element_blank()) # Remove legend title

# The formatting of the legend can also be adjusted in more detail
Rent_plot + geom_histogram(aes(fill = Deprivation), colour = 'gray2', bins = 40) +  theme_bw() + theme(legend.position = "top", legend.title=element_text(colour = "white", size = 10, face="bold"), legend.text=element_text(colour = "white", size = 8), legend.background = element_rect(fill = "black"), legend.key = element_rect(fill = "black")) + labs(fill = "Deprivation", x = "Avereage Rent Change as a Percentage")
# element_ allows specific non-data components of plots to be adjusted. eg. element_text for text and elecment_rect for borders and backgrounds

# These plots seem to show that the least deprived authorities generally saw modest increases in rent, where the most deprived authorities saw more significant but varied change, some with a decrease. This may show the temporary rent controls imposed as working to some extent.

# ---- Labels ----
# Labels can also be easily customised in ggplot2
Histogram <- Rent_plot + geom_histogram(aes(fill = Deprivation), colour = 'gray2', bins = 40) +  theme_bw() + theme(legend.position = "top", legend.title=element_text(colour = "white", size = 10, face="bold"), legend.text=element_text(colour = "white", size = 8), legend.background = element_rect(fill = "black"), legend.key = element_rect(fill = "black"))

Histogram + labs(title="Rent Change and Deprivation", x="Average Rent Change", y="Number of Authorities", fill="Deprivation")

# labs() allows labels to be customised. For more detail see:
browseURL("https://ggplot2.tidyverse.org/reference/labs.html")

# Labels can also be added onto the plot itself
Histogram + labs(title="Rent Change and Deprivation", x="Average Rent Change", y="Number of Authorities", fill="Deprivation") + annotate(geom="label", x = c(0.01, 0.075), y = c(5,5), label = c("Low End","High End"), fill="black", colour=c("steelblue1", "tomato3"))

# annotate() allows labels to be added with the x and y position on the plot and the label itself required
# rect can also be used to highlight a specific area
ggplot(Scot_data, aes(SIMD, life_expectancy_2022, shape = location)) + geom_point(aes(colour = region), size = 4) + annotate("rect", xmin=3500, xmax=3675, ymin=77.3, ymax=78, alpha=0.5)  # note that alpha allows the transparency of an object to be set

# segment can add a custom line
ggplot(Scot_data, aes(SIMD, life_expectancy_2022, shape = location)) + geom_point(aes(colour = region), size = 4) + annotate(geom="segment", x=2900, xend=3800, y=74, yend=80, colour="plum4", lwd=1.5, alpha=0.5) # lwd sets the line width


library(ggrepel) # Lets add more useful labels.

Scatter_plot <- ggplot(Scot_data, aes(SIMD, life_expectancy_2022, shape = location)) + geom_point(aes(colour = region), size = 4) + 
  geom_label_repel(aes(label = authority),
                   size = 2.5,
                   box.padding   = 0.2, 
                   point.padding = 0.4,
                   segment.color = 'grey50') # ggrepel allows labels to be added that won't clash with one another.

Scatter_plot # Now we can see which specific authorities are the most deprived, with Glasgow City having the highest levels of deprivation and the lowest life exectancy.

# ---- Colours ----
# The default colours in R aren't necessarily always the best options, but this too can be customised
browseURL("https://r-charts.com/colors/") # Default colours in R

custom_colours <- c('olivedrab3', '#EEC900', 'firebrick3', '#CD0000')

Scatter_plot + scale_colour_manual(values = custom_colours)

# Pre-set colour palettes can also be used
library(viridis)
Scatter_plot + scale_colour_viridis(discrete = TRUE)

# For information on available colour palettes see
browseURL("https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/")

# ==== Facet Wrapping ====
# Facet wrapping can allow for more obvious comparison between specified attributes
Scatter_plot + facet_wrap(~Deprivation)
Scatter_plot + facet_wrap(~region, ncol = 4, dir = 'v')


# ==== Different Types of Plots ====

# So far we have looked mainly at scatter plots and histograms. There are many more than this, we can look at some of the main other options now, but for greater detail for many available plots see:
browseURL("http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html")

# Scatter plot
Scatter_plot # This plot is very commonly used and particularly useful for identifying correlations. In this case,a positive correlation between deprivation and life expectancy.

# Histogram
ggplot(Scot_data, aes(Alcohol)) + geom_histogram(aes(fill = Deprivation), colour = 'gray2', bins = 20) +  theme_bw() # Histograms are particularly useful for plotting distributions and detecting whether or not the plot is normally distributed. With sensible use of fill, different attributes can also be easily compared. This plot seems to suggest the more deprived authorities generally see greater levels of alcohol abuse. 

# Density Plot
ggplot(Scot_data, aes(Alcohol)) + geom_density() + facet_wrap(~ Deprivation) # This is similar to a histogram, in that it can show peaks and troughs in the distribution, but with a density plot, the precise shape of each factors distribution can be more easily visualised. This shows a large peak amongst the least deprived authorites at a low level of alcohol abuse, the peak is less rponounced but at higher levels of alcohol abuse for the more deprived authorities.

# Boxplots

ggplot(Scot_data, aes(x= Deprivation, y= life_expectancy_2022, fill= location)) + geom_boxplot() # This plot allows for distributions, means, range and outliers to be very easily identified. The mean is the horizontal line across each box and the top section of each box the 75%ile, with the bottom section 25%ile. The colour or shape of outliers can be altered with outlier. Or even removed eg.
ggplot(Scot_data, aes(x= Deprivation, y= life_expectancy_2022, fill= location)) + geom_boxplot(outlier.shape = NA)

boxplot.stats(Scot_data$life_expectancy_2022)

# #### PART 2: Practicals ####
# ==== Practical 1 ====
# Try to visualise any correlation between average gas consumption and energy bills. Compare the most and least deprived areas as well as the urban/rural divide.

# ==== Practical 2 ====
# Visualise the difference in journey times using public transport to a GP. Look at regional differences.

# ==== Practical 3 ====
# Expand on the above, but divide by rural and urban.

# #### PART 3: Advanced Visualisations ####
# ==== Spatial Data ====
# Spatial data is requires more complex to work with in R. This will just provide a brief example of how spatial data can be visualise din R, but it is important to udenrstand the complexities of spatial data if you want to take this further.

# Mount packages
library(rgdal)
library(sp)
library(sf)
library(gridExtra)
library(latticeExtra)

# Import Simple Shapefiles
Local_auth <- st_read("Data/Spatial/Local_auth.shp") # st_read() is part of the sf (Simple Features) package, which allows us to import shapefiles and their attributes. Look at the Local_auth object. It contains much of the same data as we have in our csv file, but also contains spatial data.

Local_auth$geometry # This provides some of the geographic data.

plot(st_geometry(Local_auth)) # st_geometry() allows us to plot only the spatial data. But we can visualise different, non-spatial data.

plot(Local_auth[1]) # Plot the first column, just the authorities.
plot(Local_auth[4]) # We can also visualise the rural urban divide.

plot(st_geometry(Local_auth), axes = T) # We can also included axes.


# ==== spplot ====
# spplot is a more advanced package for analysing larger more complex spatial data. We need to reimport the local authority shape file using readOGR (part of the rgdal package) in order for the object to be compatible with spplot.
Scot <- readOGR(dsn="Data/Spatial", layer = "Local_auth") # note that when using readOGR, we do not include the file extension, as multiple different types of file are being used

Scot$Cont_First <- as.factor(Scot$Cont_First) # Make sure First Party is a factor.

spplot(Scot, zcol = 'SIMD') # Using sspolt, we can define a z column, which can help us to visualise additional data. In this case the deprivation rank of each authority.

Scot$region <- as.factor(Scot$region) # Make sure region is a factor.
spplot(Scot, zcol = 'region')

# ==== Practical 4 ====
# Try and visualise some of the factors we have looked at earlier in the day, but in a spatial format.

# Data visualisation is a huge part of R, this has just scratched the surface, but hoepfully gives the basics so you can look at more specific and advanced visualisations for your own data in the future.


# #### END ####
