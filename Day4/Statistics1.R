############### STATISTICAL ANALYSIS 1 #######################
###### PART 1: Descriptive Stats ######

# 1. Getting Set ###########
# 1.1 Packages we need ==========
library("tidyverse")
library("lme4")
library("effects")
library("sjPlot")

# 1.2 Import the data sets ===========
Scot_data <- read_csv("Day3/DataWrangling/outputs/Full_Scottish_Data.csv")

# 1.3 Get to know our data ===========
head(Scot_data)
str(Scot_data)


# 1.4. Data cleaning and data wrangling ===========

# 1.4.1 Set categorical variables as factors ------
Scot_data$location <-as.factor(Scot_data$location)
levels(Scot_data$location)

## 1.4.2 Create new columns to show housing/rent increase from 2021 to 2022 -----

# Create new columns for housing increase. We just need to subtract "house_price_jul_21" from "house_price_jul_22".
Scot_data$Housing_Increase <- Scot_data$house_price_jul_22 -  Scot_data$house_price_jul_21
summary(Scot_data$Housing_Increase)

# 2. Exercise 1 ###########
# 2.1 Summarising statistics of our data ========
# Create a new column showing rent increase.

# Look at mean median sum and Sd
# mean calculates the mean, and so we can compare mean rent across Scotland between 2021 an 2022.
mean(Scot_data$average_rent_2021)
mean(Scot_data$average_rent_2022)
# This suggests rent is increasing.

# median can be used in a similar way.
median(Scot_data$welfare_applications2020_2021)
median(Scot_data$welfare_applications2021_2022)

# We can also just sum data if we want totals.
sum(Scot_data$homelessness_applications2020_21)
sum(Scot_data$homelessness_applications2021_2022)
# This shows an increase in welfare and homelessness applications.

# sd can be used to calculate the standard deviation of data set. This can be helpful to understand how different each observation is across the data set.
sd(Scot_data$average_energy_bill_2021) 
mean(Scot_data$average_energy_bill_2021) # This is quite a small value compared to the mean, and so energy bills seem quite consistent across authorities.

# Summarise the factors of our interest into a table

# summary statistics for housing_increase,  rent_increase, average_energy_bill_2021 into a table

housing_summary <- Scot_data %>%
  summarize(Average_Housing_increase = mean(Housing_Increase), 
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

# 2.2 Which factor is associated with the increase of house price? ======== 

## 2.2.1 Is energy bill associated with the increase of house price? ------

# We first try to visualise the data. We can plot the relation using geom_smooth() function and specify method as "lm". e.g., geom_smooth(method = "lm", se=TRUE)

# Plot the relation
# consider using geom_smooth() to superimpose the best-fitting line describing the association of interest
Plot_housing <- 
  ggplot(data = Scot_data,
         aes(x=average_energy_bill_2021, 
             y=Housing_Increase))+
  geom_point() +
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "average_energy_bill_2021",
       y = "House Price Increase from 2021-2022",
  )
Plot_housing
# It looks like energy bill is not associated with housing price change. This make sense cause we do not have the data of 2022 yet

# Let's test this in a regression model. To fit a regression model with a continuous outcome variable, we can use the lm() function from the "lme4" package. If the outcome variable is not continuous, you use different formulas. Due to limited time, we will not cover those in this workshop.

# Fit a regression model including energy bill as a predictor, using lm() function.
model1 <- lm (Housing_Increase ~ average_energy_bill_2021, data = Scot_data)
summary(model1)

# Intercept is the expected value of Y (the outcome variable) when X (the predictor) is 0.

# The intercept of the model tells us that the expected house price increase in Scotland between 2021 and 2022 is £31466.38, when energy bill in 2021 was 0. It is significant (P <.05), suggesting that price increase differs among different areas. 

# But is it meaningful to consider price increase when energy bill is 0?

# Top tip: Scale a continuous predictor to make the intercept more meaningful

# A more meaningful intercept would represent the expected value of y when x is at the average value i.e., when x is the mean (rather than 0). We can do so by scaling the predictor using scale() function. This can be done within the model (rather than change the data).

# Refit the regression model, now use scaled energy bill as a predictor.
model1_scale <- lm (Housing_Increase ~ scale(average_energy_bill_2021), data = Scot_data)

summary(model1_scale)
# Now the intercept is more meaningful. It tells us the expected increase of house price with an average energy bill.  In other words, when an area's energy bill is at the average value across Scotland, the expected increase of house price in that area would be 20.974.

# Slope is the number of units by which Y (the outcome variable) increases on average, as X (the predictor variable) increases by a unit. In this case, the slope shows that energy bill increases by 1 unit, the expected increase of house price increased by -£1,436, or decreases by £1,436. HOWEVER, the results is insignificant (p = .39), suggesting that house price increase is not associated with energy bill.

# Note that the overall model quality is low. The R-squared and adjusted R-squared shows how much variance the predictor can explain - in this case, nearly zero! (Note the negative number for the adjusted R-squared. The formula for adjusted R square allows it to be negative. It is intended to approximate the actual percentage variance explained. So if the actual R square is close to zero the adjusted R square can be slightly negative. You can just think of it as an estimate of zero). Finally, the last line of the model output tells us this model is not better than a null model which does not include any predictor : F(1, 30) = 0.75, P < .39.

# We can test this manually by building a null model which includes no predictor but only the intercept.

# Fit a regression model including no predictor, using lm() function.
model_null <- lm (Housing_Increase ~ 1, data = Scot_data)

# We then compare it with our model1 which had "energy bill" as a predictor.

anova(model_null, model1_scale)
# Indeed model comparison suggests that our model1 (the model that included "energy_bill" as a predictor) is not better a null model without a predictor. Note that the model comparison gave us same results as the model output of model1. 

# We can conclude that house price increase is not associated with energy bill.

## 2.2.2 Is alcohol consumption associated with the increase of house price? ------

# Plot the relation
Plot_housing2  <- 
  ggplot(data = Scot_data,
         aes(x=Alcohol, 
             y=Housing_Increase))+
  geom_point() +
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "Alcohol Consumption",
       y = "House Price Increase from 2021-2022",
  )

Plot_housing2

# The plot seems to suggest that there is a trend - as the consumption of alcohol increases, the housing increase becomes smaller. In other words, for the areas that consumed more alcohol, their house price increased less than other areas. 

# Is this difference significant statistically? 

# Let's test this in a regression model. Fit a regression model including scaled alcohol consumption as a predictor, using lm() function.
model2 <- lm (Housing_Increase ~ scale(Alcohol), data = Scot_data)
summary(model2)

### Discussion Point
#### Can you explain the model results? 
#### Discuss this with your neighbours and try to interpret the results.

# An example answer: 

# The model intercept is significant (P < .001) and it tells us that the expected increase of house price in Scotland is £20,974 on average, for an area that consumed average amount of alcohol.  

# The slope (Alcohol) tells us that, as Alcohol consumption increases by 1 unit, the increase of house price increase is increased by -£3,812, i.e., reduced by £3,812. Note that this predictor is significant, p < .05.

# This model can explain 14.43% of the variance. It is significantly better than the null model, F(1, 30) = 6.23, p < .05.

# We can conclude that the increase of house price is associated with alcohol consumption.

# 2.2.3 Is the increase of house price dependent of location (Rural vs Urban)? ------

# Plot the relation

# when dealing with a categorical predictor, it would make sense to use box plot instead of scatter plot.

Plot_housing3 <- 
  ggplot(data = Scot_data,
         aes(x=location, 
             y=Housing_Increase, 
             fill=location))+
  geom_boxplot() +
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "Location (Rural vs Urban)",
       y = "House Price Increase from 2021-2022"
  )

Plot_housing3

# How to read a box plot? https://www.simplypsychology.org/boxplots.html

# We can compare the medians of box plots. If the median line of a box plot lies outside of the box of a comparison box plot, then there is likely to be a difference between the two groups.

# It looks like location is not associated with the increase of housing price.

# Let's test this in a regression model. Fit a regression model including the 2-level factor "location" as a predictor, using lm() function.
model3 <- lm (Housing_Increase ~ location, data = Scot_data)
summary(model3)

### How do we interpret the results when the predictor is a categorical vairable?

# Hint: The intercept now is the expected y when x is at the reference level.

# R automatically uses dummy coding and choose a reference level for us based on the alphabetic order of the level name. For example here "Rural" is chosen as the reference level as letter "R" comes before letter "U" for "Urban". You can mannually change the reference level using the relevel() function.

# Intercept: The expected increase of house price is £20945.58 in Rural areas.

# Slope: The expected increase of house price for the Urban areas is £69.42 more than Rural areas. However, this difference is NOT significant.

# We can conclude that there is no difference between the two categories,in other words, housing increase is a general trend that occurs in both Urban and Rural areas.

# 2.2.4 Is Deprivation associated with increase of house price? ------ 

# First let's recall what SIMD is. 
# SIMD ranks data zones from most deprived (ranked 1) to least deprived (ranked 6,976). More deprived areas have smaller SIMD values. 

# https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/#supportingdocuments

# 3. Exercise 2 ##############

# Can you try to plot the relation between Housing_Increase and SIMD ?
# Can you Interpret the results?

#  3.1 Multiple regression (i.e., with multiple predictors) ======= 

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

# 4: Null Hypothesis Testing ############################

# We can use Null Hypothesis Testing (NHT) to try and reject the Null Hypothesis, this allows us to better understand whether or not the results we are seeing are statistically significant, or might just be explained by random noise etc.

# 4.1 Two Sample t.tests ===================================

# Let's test whether or not the increases we observed between 2021 and 2022.
two_sample_ttest <- t.test(Scot_data$average_rent_2022, Scot_data$average_rent_2021)

print(two_sample_ttest) # Our p value is significantly larger than 0.05, and so the Null Hypothesis cannot be rejected.

# We can also use this to see if specific factors make a difference. For example, is the increase in food insecurity significantly higher between urban and rural authorities.

# In this case we want to use the data set that contains the % of increase/decrease

authority_data_cleaned<-read_csv("Day3/DataWrangling/outputs/authority_data_cleaned.csv")

Rural <- subset(authority_data_cleaned, location =='Rural')
Urban <- subset(authority_data_cleaned, location =='Urban')

mean(Rural$FoodInsecurity)
mean(Urban$FoodInsecurity)

# Rural Food insecurity decreased while the urban food insecurity increase. The Null Hypothesis is that the Urban and Rural distinction is not a significant factor in this pattern. We can use a t.test to try and reject this Null Hypothesis.
two_sample_ttest <- t.test(Rural$FoodInsecurity, Urban$FoodInsecurity)

print(two_sample_ttest) # So even if the means are very different Again, we cannot reject the Null Hypothesis.

# ---- Practical 1 ---------------------

# What about homeless application? Check the difference in increase/decrease of homeless applications between urban and rural authorities. Use a two sample t.test and determine whether or not the Null Hypothesis, that the Urban and Rural distinction is not a major factor, can be rejected.

# 4.2 One Sample t.tests ===================================
# For one sample t.tests, we can test the mean of a population against a hypothetical mean.

# In this case, we can test the raise of average rent costs in Scotland between 2021 and 2022 and the UK mean. The percentage of increase in rent across Uk between June 2021 and June 2022 has been of 9.5% (browseURL(https://www.haart.co.uk/landlords/landlord-advice/rent-increase-notices-what-is-a-fair-rent-increase/)) 

# We can use this figure as a 'mu' value to run a one sample t.test.
mean(authority_data_cleaned$Rent) # Rent raise seems lower, but is this statistically significant?

t.test(authority_data_cleaned$Rent, mu = 9.5) # With a p value below 0.05, the Null Hypothesis can be rejected, and it seems there is a significant difference between Scotland raise in rent and general UK one (note these are imperfect data, with the Scottish sample include the whole 2022).


# 4.3 ANOVA Tests Explained ===================================
# ANOVA tests can determine the impact of an independent variable on a quantitative dependent variable of a data set. 

# For example, how the rent increase impact the different deprivation areas.

summary(aov(Homeless ~ SIMDQuint, data = authority_data_cleaned)) # ANOVA tests can use the summary() function and the aov() function to give helpful outputs. The quantitative variable being tested is on the left separated from the independent variables by a ~. You can add multiple independent variables separated by +. Assign the data source with data = .


# The p.value is below 0.05, so it seems that deprivation does play a significant role in the increase of the Homeless applications overall but lets see in more detail 

# Save the result of the anova
AnovaResult<-aov(Homeless ~ as.factor(SIMDQuint), data = authority_data_cleaned)

# Now use Tukey multiple pairwise-comparisons
TukeyTest <- TukeyHSD(AnovaResult)
print(TukeyTest)
#Check across all possible comparison

# ---- Practical 2 ----
# Using an ANOVA test, determine the extent to which deprivation, the Urban-Rural divide and the areas of Scotland are each significant factors in determining increase/decrease in Food Insecurity.

# This is just an introduction to hypothesis testing, but we will continue over the rest of the day looking at different statistical analyses.


# #### END ####
