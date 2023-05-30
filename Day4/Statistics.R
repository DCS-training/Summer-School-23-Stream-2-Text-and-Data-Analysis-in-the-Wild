############### STATISTICAL ANALYSIS #######################
# RESEARCH QUESTIONS
# 1. Which factor is associated with the increase of house price?
# 1.1. does the average energy bill impact it?
# 1.2. Is alcohol consumption associated with the increase of house price?
# 1.3.Is the increase of house price dependent of location (Rural vs Urban)?

# 1. Getting Set ###########
## 1.1. Packages we need ==========
library("tidyverse")
library("lme4")
library("effects")
library("sjPlot")
library("ggfortify")

## 1.2. Import the data sets ===========
Scot_data <- read_csv("Day3/DataWrangling/outputs/Full_Scottish_Data.csv")
authority_data<-read_csv("Day3/DataWrangling/outputs/authority_data_cleaned.csv")

## 1.3. Get to know our data ===========
head(Scot_data)
str(Scot_data)

head(authority_data)
str(authority_data)

# 2. Descriptive Stats ######
## 2.1. Data cleaning and data wrangling ===========

# Set categorical variables as factors
Scot_data$location <-as.factor(Scot_data$location)
levels(Scot_data$location)

# Create new columns for housing increase. We just need to subtract "house_price_jul_21" from "house_price_jul_22".
Scot_data$Housing_Increase <- Scot_data$house_price_jul_22 -  Scot_data$house_price_jul_21
summary(Scot_data$Housing_Increase)

## 2.2. Exercise 1 =====================
# Summarising statistics of our data:
# Create a new column showing rent increase


## 2.3. Mean Median Sum and SD ==================
### 2.3.1. Mean ------------
# Mean calculates the mean, and so we can compare mean rent across Scotland between 2021 an 2022.
mean(Scot_data$average_rent_2021)
mean(Scot_data$average_rent_2022)
# This suggests rent is increasing.

### 2.3.2. Median ------------
# median can be used in a similar way.
median(Scot_data$welfare_applications2020_2021)
median(Scot_data$welfare_applications2021_2022)

### 2.3.3. Sum ------------
# We can also just sum data if we want totals.
sum(Scot_data$homelessness_applications2020_21)
sum(Scot_data$homelessness_applications2021_2022)
# This shows an increase in welfare and homelessness applications.

### 2.3.4. SD ------------
# sd can be used to calculate the standard deviation of data set. This can be helpful to understand how different each observation is across the data set.
sd(Scot_data$average_energy_bill_2021) 
mean(Scot_data$average_energy_bill_2021) # This is quite a sma#ll value compared to the mean, and so energy bills seem quite consistent across authorities.

## 2.4 Summarise our observations =================
# Summarise the factors of our interest into a table
# Ssummary statistics for housing_increase,  rent_increase, average_energy_bill_2021 into a table
housing_summary <- Scot_data %>% 
  summarise(Average_Housing_increase = mean(Housing_Increase), 
    Average_Rent_increase = mean(Rent_Increase), 
    Average_Energy_bill_in_2021 = mean(average_energy_bill_2021), 
  )
# we can create different slices of our data this way
summary_stats <- Scot_data %>%
  summarize(
    Mean_population = mean(Population),
    Median_SIMD = median(SIMD),
    SD_Food_insc = sd(food_insecurity2018_2022)
  )

# 3. Regression #######
# Which factor is associated with the increase of house price? 

## 3.1. Check against a specific factor ============
# Is energy bill associated with the increase of house price? 

# We first try to visualise the data. We can plot the relation using geom_smooth() function and specify method as "lm". e.g., geom_smooth(method = "lm", se=TRUE)

### 3.1.1 Plot the relation -------------
# Consider using geom_smooth() to superimpose the best-fitting line describing the association of interest
ggplot(data = Scot_data,
         aes(x=average_energy_bill_2021, 
             y=Housing_Increase))+
  geom_point() + #scatter plot
  geom_smooth(method = "lm", se=TRUE)+ #best fitting line
  labs(x= "average_energy_bill_2021",
       y = "House Price Increase from 2021-2022",
  )

# It looks like energy bill is not associated with housing price change. This make sense cause we do not have the data of 2022 yet
### 3.1.2. Regression model for Energy --------------
# Let's test this in a regression model. To fit a regression model with a continuous outcome variable, we can use the lm() function from the "lme4" package. If the outcome variable is not continuous, you use different formulas. Due to limited time, we will not cover those in this workshop.

# Fit a regression model including energy bill as a predictor, using lm() function.
model1 <- lm (Housing_Increase ~ average_energy_bill_2021, data = Scot_data)
summary(model1)

# Intercept is the expected value of Y (the outcome variable) when X (the predictor) is 0.

# The intercept of the model tells us that the expected house price increase in Scotland between 2021 and 2022 is £31466.38, when energy bill in 2021 was 0. It is significant (P <.05), suggesting that price increase differs among different areas. 

# But is it meaningful to consider price increase when energy bill is 0?

# Top tip: Scale a continuous predictor to make the intercept more meaningful

# A more meaningful intercept would represent the expected value of y when x is at the average value i.e., when x is the mean (rather than 0). We can do so by scaling the predictor using scale() function. This can be done within the model (rather than change the data).

### 3.1.3. Refit Regression Model -----------
# Refit the regression model, now use scaled energy bill as a predictor.
model1_scale <- lm (Housing_Increase ~ scale(average_energy_bill_2021), data = Scot_data)

summary(model1_scale)
# Now the intercept is more meaningful. It tells us the expected increase of house price with an average energy bill.  In other words, when an area's energy bill is at the average value across Scotland, the expected increase of house price in that area would be 20,974.

# Slope is the number of units by which Y (the outcome variable) increases on average, as X (the predictor variable) increases by a unit. In this case, the slope shows that energy bill increases by 1 unit, the expected increase of house price increased by -£1,436, or decreases by £1,436. HOWEVER, the results is insignificant (p = .39), suggesting that house price increase is not associated with energy bill.

# Note that the overall model quality is low. The R-squared and adjusted R-squared shows how much variance the predictor can explain - in this case, nearly zero! (Note the negative number for the adjusted R-squared. The formula for adjusted R square allows it to be negative. It is intended to approximate the actual percentage variance explained. So if the actual R square is close to zero the adjusted R square can be slightly negative. You can just think of it as an estimate of zero). Finally, the last line of the model output tells us this model is not better than a null model which does not include any predictor : F(1, 30) = 0.75, P < .39.
# We can test this manually by building a null model which includes no predictor but only the intercept.

### 3.1.4. Compare to the Null Model ------------
# Fit a regression model including no predictor, using lm() function.
model_null <- lm (Housing_Increase ~ 1, data = Scot_data)

# We then compare it with our model1 which had "energy bill" as a predictor.

anova(model_null, model1_scale)
# Indeed model comparison suggests that our model1 (the model that included "energy_bill" as a predictor) is not better a null model without a predictor. Note that the model comparison gave us same results as the model output of model1. 

# We can conclude that house price increase is not associated with energy bill.

## 3.2. Check against Alcohol Consumption =======
# Is alcohol consumption associated with the increase of house price? 

### 3.2.1. Plot the relation -----------
ggplot(data = Scot_data,
         aes(x=Alcohol, 
             y=Housing_Increase))+
  geom_point() +
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "Alcohol Consumption",
       y = "House Price Increase from 2021-2022",
  )


# The plot seems to suggest that there is a trend - as the consumption of alcohol increases, the housing increase becomes smaller. In other words, for the areas that consumed more alcohol, their house price increased less than other areas. 

### 3.2.2. Regression Model for alcohol consumption vs house prices ------------------
# Is this difference significant statistically? 

# Let's test this in a regression model. Fit a regression model including scaled alcohol consumption as a predictor, using lm() function.
model2 <- lm (Housing_Increase ~ scale(Alcohol), data = Scot_data)
summary(model2)

### 3.2.3. Discussion Point ---------------------
# Can you explain the model results? 
# Discuss this with your neighbours and try to interpret the results.

# An example answer: 

# The model intercept is significant (P < .001) and it tells us that the expected increase of house price in Scotland is £20,974 on average, for an area that consumed average amount of alcohol.  

# The slope (Alcohol) tells us that, as Alcohol consumption increases by 1 unit, the increase of house price increase is increased by -£3,812, i.e., reduced by £3,812. Note that this predictor is significant, p < .05.

# This model can explain 14.43% of the variance. It is significantly better than the null model, F(1, 30) = 6.23, p < .05.

# We can conclude that the increase of house price is associated with alcohol consumption.

## 3.3. Is the increase of house price dependent of location (Rural vs Urban)? =============

### 3.3.1. Plot the relation -------------------

# when dealing with a categorical predictor, it would make sense to use box plot instead of scatter plot.
ggplot(data = Scot_data,
         aes(x=location, 
             y=Housing_Increase, 
             fill=location))+
  geom_boxplot() + # boxplot
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "Location (Rural vs Urban)",
       y = "House Price Increase from 2021-2022"
  )

# How to read a box plot? https://www.simplypsychology.org/boxplots.html

# We can compare the medians of box plots. If the median line of a box plot lies outside of the box of a comparison box plot, then there is likely to be a difference between the two groups.

# It looks like location is not associated with the increase of housing price.

### 3.3.2. Regression Model for Rural vs Urban -------------------
# Let's test this in a regression model. Fit a regression model including the 2-level factor "location" as a predictor, using lm() function.
model3 <- lm (Housing_Increase ~ location, data = Scot_data)
summary(model3)

### How do we interpret the results when the predictor is a categorical variable?

# Hint: The intercept now is the expected y when x is at the reference level.

# R automatically uses dummy coding and choose a reference level for us based on the alphabetic order of the level name. For example here "Rural" is chosen as the reference level as letter "R" comes before letter "U" for "Urban". You can manually change the reference level using the relevel() function.

# Intercept: The expected increase of house price is £20945.58 in Rural areas.

# Slope: The expected increase of house price for the Urban areas is £69.42 more than Rural areas. However, this difference is NOT significant.

# We can conclude that there is no difference between the two categories,in other words, housing increase is a general trend that occurs in both Urban and Rural areas.

## 3.4. Is Deprivation associated with increase of house price? ========
# First let's recall what SIMD is. 
# SIMD ranks data zones from most deprived (ranked 1) to least deprived (ranked 6,976). More deprived areas have smaller SIMD values. 

# https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/#supportingdocuments

### 3.4.1 Exercise 2 --------------
# Can you try to plot the relation between Housing_Increase and SIMD ?
# Can you Interpret the results?

## 3.5. Multiple regression (i.e., with multiple predictors) ======= 
# We can have more than one predictors in a single regression.  

# From the above we know that the increase of house price is significantly associated with alcohol consumption, and also with SIMD but only marginally significantly. Let's see what happens if we include both alcohol and SIMD in the model.
model5 <- lm(Housing_Increase ~ scale(SIMD) + scale(Alcohol)  , data = Scot_data)
summary(model5)

# Now we see that if we include them both, neither factor remains significant.

# Is this model better than the model only including Alcohol as a predictor (model2) or the one only including SIMD as a predictor (model4)?

# We can compare model fit using the anova() function again. Remember we used it when compared our very first model with a null model? We do exactly the same here.

# Use anova() to compare the two-predictor model with the one-predictor model that including only Alcohol as a predictor.
anova (model2, model5)

# The results show that including both Alcohol and SIMD as predictors did not improve model fit compare to either one-predictor model (F(1) =.01, p=.91). 



# 4. Null Hypothesis Testing ############################

# We can use Null Hypothesis Testing (NHT) to try and reject the Null Hypothesis, this allows us to better understand whether or not the results we are seeing are statistically significant, or might just be explained by random noise etc.

## 4.1 Two Sample t.tests ===================================
### 4.1.1. Compare rents 2021-2022--------------
# Let's test whether or not the increases we observed between 2021 and 2022.
two_sample_ttest <- t.test(Scot_data$average_rent_2022, Scot_data$average_rent_2021)

print(two_sample_ttest) # Our p value is significantly larger than 0.05, and so the Null Hypothesis cannot be rejected.

# We can also use this to see if specific factors make a difference. For example, is the increase in food insecurity significantly higher between urban and rural authorities.

### 4.1.2. Compare Urban vs Rural--------------
# In this case we want to use the data set that contains the % of increase/decrease

authority_data_cleaned<-read_csv("Day3/DataWrangling/outputs/authority_data_cleaned.csv")

Rural <- subset(authority_data_cleaned, location =='Rural')
Urban <- subset(authority_data_cleaned, location =='Urban')

mean(Rural$FoodInsecurity)
mean(Urban$FoodInsecurity)

# Rural Food insecurity decreased while the urban food insecurity increase. The Null Hypothesis is that the Urban and Rural distinction is not a significant factor in this pattern. We can use a t.test to try and reject this Null Hypothesis.
two_sample_ttest <- t.test(Rural$FoodInsecurity, Urban$FoodInsecurity)

print(two_sample_ttest) # So even if the means are very different Again, we cannot reject the Null Hypothesis.

### 4.1.3. Exercise 3------------------

# What about homeless application? Check the difference in increase/decrease of homeless applications between urban and rural authorities. Use a two sample t.test and determine whether or not the Null Hypothesis, that the Urban and Rural distinction is not a major factor, can be rejected.

## 4.2 One Sample t.tests ===================================
# For one sample t.tests, we can test the mean of a population against a hypothetical mean.

# In this case, we can test the raise of average rent costs in Scotland between 2021 and 2022 and the UK mean. The percentage of increase in rent across Uk between June 2021 and June 2022 has been of 9.5% (browseURL(https://www.haart.co.uk/landlords/landlord-advice/rent-increase-notices-what-is-a-fair-rent-increase/)) 

# We can use this figure as a 'mu' value to run a one sample t.test.
mean(authority_data_cleaned$Rent) # Rent raise seems lower, but is this statistically significant?

t.test(authority_data_cleaned$Rent, mu = 9.5) # With a p value below 0.05, the Null Hypothesis can be rejected, and it seems there is a significant difference between Scotland raise in rent and general UK one (note these are imperfect data, with the Scottish sample include the whole 2022).


## 4.3 ANOVA Tests ===================================
# ANOVA tests can determine the impact of an independent variable on a quantitative dependent variable of a data set. 
### 4.3.1. Homeless application in different deprivation areas ---------
# For example, how the rent increase impact the different deprivation areas.
summary(aov(Homeless ~ SIMDQuint, data = authority_data)) # ANOVA tests can use the summary() function and the aov() function to give helpful outputs. The quantitative variable being tested is on the left separated from the independent variables by a ~. You can add multiple independent variables separated by +. Assign the data source with data = .

# The p.value is above 0.05, so it seems that deprivation areas does not play a significant role in the increase of the Homeless applications overall but lets see in more detail 

# Save the result of the anova
AnovaResult<-aov(Homeless ~ as.factor(SIMDQuint), data = authority_data_cleaned)

# Now use Tukey multiple pairwise-comparisons
TukeyTest <- TukeyHSD(AnovaResult)
print(TukeyTest)
#Check across all possible comparison. So the are statistically significant variations across the different comparisons but they null themselves when we look at the whole sample

### 4.3.2. Exercise 4 ----
# Using an ANOVA test, determine the extent to which deprivation, the Urban-Rural divide and the areas of Scotland are each significant factors in determining increase/decrease in Homeless Applications.

# This is just an introduction to hypothesis testing, but we will continue over the rest of the day looking at different statistical analyses.



# 5. Principle Component Analysis ###############
## 5.1. Getting the Dataset ready =============
# We do not need all the variable let's create a new data frame with only the one we want
PCADataset<-authority_data[,c(1:3,17,18,9:13,15)]# subset the columns we want by position
PCADataset$SIMDQuint<-as.character(PCADataset$SIMDQuint)# make sure that the SIMD quintile is correctly encoded 
PCADataset$SIMDDecil<-as.character(PCADataset$SIMDDecil)# make sure that the SIMD decile is correctly encoded 

### 5.1.1. Subset the numerical columns -----------
# Selecting columns with numerical components (those 6-11) to make a new data frame 
measures <- PCADataset[,c(6:11)]#so I am selecting all rows and the columns from the fifth to the tenth
View(measures)# let see the new data set 

## 5.2. Computing the PCA of the numerical measures ==============
pcs <- prcomp(measures) #defining a new object called pcs that will run a principal components analysis of our numeric variables
pcs
plot(pcs)


## 5.3. Add PC1 and PC2 to the original dataset==========
pcdata <- PCADataset #creating a new data frame named pcdata containing all the variables that were on the original authority data 
pcdata$pc1 <- pcs$x[,1] #creating a new variable named pc1 that will contain all the pc1 values (all rows and the first column of the pcs list)
pcdata$pc2 <- pcs$x[,2]#creating a new variable named pc2 that will contain all the pc1 values (all rows and the second column of the pcs list)
View(pcdata)

## 5.4. Plotting ====================
### 5.4.1. Plot with ggplot -----------
ggplot(pcdata, aes(x=pc1, y=pc2, color=location)) + #use pc1, pc2 and colour code by period
  geom_point(size=6, alpha=0.5)+# use scatter plot and set size to 6 and 50% transparency
  theme_bw()+ # white background
  labs(title = "PCA")+ #add a title
  geom_text(aes(label = authority),hjust=0.5, vjust=-0.5) # adds authority labels

### 5.4.2. Plot with ggfortify -----------
autoplot(pcs, data=PCADataset, colour='location', alpha=0.5, loadings=TRUE,loadings.label = TRUE, loadings.colour = 'black',loadings.label.colour= 'black')+
  theme_bw()


# Adding Labels
autoplot(pcs, data=PCADataset, colour='location', alpha=0.5, loadings=TRUE,loadings.label = TRUE, shape=FALSE, loadings.colour = 'black',loadings.label.colour= 'black')+
  theme_bw()+
  geom_label(aes(label = authority, hjust=0.5,vjust=0.5, colour=location))# this time we are not using ggplot but we are using autoplot within ggfortify and we are leaving the system to decide which type of graph to plot 
#The loadings arrows will identify which are the variables that more impact the differences across the groups


## 5.5. Exercise 5
# Look at Impact on different deprivation areas

# 6. K-Means Clustering #####
## 6.1. Intialise Kmeans ===========
numGroups = 2 #how many groups I expect in my data set
myKMeans <- kmeans(measures,numGroups)#define a new variable named myKMeans that will apply the kmeans function on our measures using the number of groups we decided at the start
myKMeans

## 6.2. Adding cluster numbers to original dataset ================
#each site in a new data frame and plotting ####
kmeandata <- pcdata #again we start by copying our old data frame 
kmeandata$cluster <- myKMeans$cluster # we are now adding a new variable named cluster that will contain the info about in which cluster the system put that specific site 

## 6.3. Plot the results=============
# and now we are going to plot the result
ggplot(kmeandata, aes(x=pc1, y=pc2, col=location)) + #use pc1 as x pc2 as y and colour code by period
  geom_point() +# use a scatter plot 
  facet_wrap(~cluster)+ #subplot by the cluster value
  theme_bw()+# white background
  geom_label(aes(label = authority, hjust=0.5,vjust=0.5))

# check for errors
ggplot(kmeandata, aes(x=pc1, y=pc2, col=location)) +#use pc1 as x pc2 as y and colour code by period
  geom_point() +# use a scatter plot 
  facet_grid(location~cluster)+ #subplot by the cluster value and period
  theme_bw()+# white background
  geom_label(aes(label = authority, hjust=0.5,vjust=0.5))

#### THE END ############