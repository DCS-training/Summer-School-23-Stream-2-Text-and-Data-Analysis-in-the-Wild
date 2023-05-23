
#### Block 8 Code ####

#### Packages ####

library(ggdendro)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(MASS)

#### Import Dataset ####
AuthorityData <- read_excel("AuthorityData.csv")

#### Principle Component Analysis ####

#### Selecting columns with numerical components (those 4-7) to make a new data frame ####
measures <- AuthorityData[,c(4,5,6,7)]#so I am selecting all rows and the columns from the second to the fifth
View(measures)# let see the new data set 

#### Computing the PCA of the numerical measures ####
pcs <- prcomp(measures) #defining a new object called pcs that will run a principal components analysis of our numeric variables
pcs
plot(pcs)

#### Create a data frame copied from sites and including the PC values ####
pcdata <- AuthorityData #creating a new dataframe named pcSites containing all the variables that were on the original sites dataframe 
pcdata$pc1 <- pcs$x[,1] #creating a new variable named pc1 that will contain all the pc1 values (all rows and the first column of the pcs list)
pcdata$pc2 <- pcs$x[,2]#creating a new variable named pc2 that will contain all the pc1 values (all rows and the second column of the pcs list)
View(pcdata)

#### Plotting 
#plot with ggplot
ggplot(pcdata, aes(x=pc1, y=pc2, color=location)) + #use pc1, pc2 and colour code by period
  geom_point(size=6, alpha=0.5)+# use scatterplot and set size to 6 and 50% transparency
  theme_bw()+ # white background
  labs(title = "PCA")+ #add a title
  geom_text(aes(label = authority),hjust=0, vjust=0) # adds authority labels

autoplot(pcs, data=AuthorityData, colour='location', size=4, alpha=0.5, loadings=TRUE,loadings.label = TRUE )+
  theme_bw()# this time we are not using ggplot but we are using autoplot within ggfortify and we are leaving the system to decide which type of graph to plot 
#The loadings arrows will identify which are the variables that more impact the differences across the groups

#### K-Means Clustering ####

numGroups = 2 #how many groups I expect in my data set
myKMeans <- kmeans(measures,numGroups)#define a new variable named myKMeans that will apply the kmeans function on our measures using the number of groups we decided at the start
myKMeans

#### Adding cluster number to each site in a new data frame and plotting ####
kmeandata <- pcdata #again we start by copying our old data frame 
kmeandata$cluster <- myKMeans$cluster # we are now adding a new variable named cluster that will contain the info about in which cluster the system put that specific site 
# and now we are going to plot the result
ggplot(kmeandata, aes(x=pc1, y=pc2, col=location)) + #use pc1 as x pc2 as y and colour code by period
  geom_point() +# use a scatter plot 
  facet_wrap(~cluster)+ #subplot by the cluster value
  theme_bw()# white background

ggplot(kmeandata, aes(x=pc1, y=pc2, col=location)) +#use pc1 as x pc2 as y and colour code by period
  geom_point() +# use a scatter plot 
  facet_grid(location~cluster)+ #subplot by the cluster value and period
  theme_bw()# white background

#### Hierarchical Clustering ####

#### Measuring pairwise distance between sites using the UPGMA ####
distMeasures <- dist(measures)# calculate the distance matrix computed by using the specified distance measure and save this info in a new variable called distMeasures

authorityHClust <- hclust(distMeasures, method="average")# Hierarchical cluster analysis usining average and save the result as a new object named sitesHclust

#### Plotting results as a dendrogram ####
authorityHClust$labels <- AuthorityData$location #create a new variable named labels in our sitesHClust that will contain the info about the period form the sites dataframe 

ggdendrogram(authorityHClust, rotate=T)#Plot the results using ggdendrogram 


#### Machine Learning using Linear Discriminant Analysis ####

daModel <- lda(location~FoodInsecurity+WelfareApp+Rent+Homeless, AuthorityData)#using linear discriminant analysis that will group together the period and elevation with all the other info we have available and save it in a new object named daModel
daModel #look what is inside the model we created

prediction <- predict(daModel, AuthorityData)#use the predict function to analyse the sites file. In the real word the file sites of course will not be the same that we used to create the model but it will be a new dataset that we want to analyse against the model


daAuthority <- pcdata #again we copy the pcSites and name it daSites
daAuthority$predicted <- prediction$class #we create a new variables named predicted that will take the analysis done by the predict function

# We plot the results
ggplot(daAuthority, aes(x=pc1, y=pc2, col=predicted)) + #using pc1 in x pc2 in y and colour coded by predicted 
  geom_point() + # plot it as scatter plot
  facet_wrap(~location)+ #subplot by period
  theme_bw()# use a white background
