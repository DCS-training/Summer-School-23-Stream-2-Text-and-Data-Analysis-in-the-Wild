title: "Block 6 Regression - with Solution"
# Packages needed ===================
library("tidyverse")
library("lme4")
library("effects")
library("sjPlot")

Scot_data <- read_csv("Day3/DataWrangling/outputs/Full_Scottish_Data.csv")

# 1. Get to know our data
head(Scot_data)
str(Scot_data)

## 1.1 Data cleaning

### 1.1.1 Set categorical variables as factors
Scot_data$location <-as.factor(Scot_data$location)
levels(Scot_data$location)


## 1.1.2 Create new columns to show housing/rent increase from 2021 to 2022
#Create new columns for housing increase. We just need to subtract "house_price_jul_21" from "house_price_jul_22".

Scot_data$Housing_Increase <- Scot_data$house_price_jul_22 -  Scot_data$house_price_jul_21
summary(Scot_data$Housing_Increase)

### Exercise 1(A)

Create a new column showing rent increase.
Scot_data$Rent_Increase <- Scot_data$average_rent_2022 -  Scot_data$average_rent_2021

summary(Scot_data$Rent_Increase)


## 1.3 Summarise the factors of our interest into a table

# summary statistics for housing_increase,  rent_increase, average_energy_bill_2021 into a table

housing_summary <- Scot_data %>%
  summarize(
    Average_Housing_increase = mean(Housing_Increase), 
    Average_Rent_increase = mean(Rent_Increase), 
    Average_Energy_bill_in_2021 = mean(average_energy_bill_2021), 
  )%>%
  
  print(housing_summary) 

### Exercise 1(B)

#Summary statistics for alcohol assumption, life expectancy in 2022, population into a table
#Remember that you can use mean() to get the average, sd() to get the standard deviation, and median() to get the median.

life_summary <- Scot_data %>%
  summarize(
    Alcohol_Consumption = mean(Alcohol), 
    Alcohol_SD = sd(Alcohol), 
    Life_Expectancy_2022 = mean(life_expectancy_2022),
    Life_Expectancy_2022_SD = sd(life_expectancy_2022),
  )

life_summary 

# 2 Which factor is associated with the increase of house price?

## 2.1 Is energy bill associated with the increase of house price?

#We first try to visualise the data. We can plot the relation using geom_smooth() function and specify method as "lm". e.g., geom_smooth(method = "lm", se=TRUE)


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

#Plot_housing
#It looks like energy bill is not associated with housing price change.

#Let's test this in a regression model. To fit a regression model with a continuous outcome variable, we can use the lm() function from the "lme4" package. If the outcome variable is not continuous, you use different formulas. Due to limited time, we will not cover those in this workshop.

#Fit a regression model including energy bill as a predictor, using lm() function.

model1 <- lm (Housing_Increase ~ average_energy_bill_2021, data = Scot_data)

summary(model1)

#Intercept is the expected value of Y (the outcome variable) when X (the predictor) is 0.
#The intercept of the model tells us that the expected house price increase in Scotland between 2021 and 2022 is £31466.38, when energy bill in 2021 was 0. It is significant (P <.05), suggesting that price increase differs among different areas. 
#But is it meaningful to consider price increase when energy bill is 0?
## Top tip: Scale a continuous predictor to make the intercept more meaningful
#A more meaningful intercept would represent the expected value of y when x is at the average value i.e., when x is the mean (rather than 0). We can do so by scaling the predictor using scale() function. This can be done within the model (rather than change the data).

#Refit the regression model, now use scaled energy bill as a predictor.
model1_scale <- lm (Housing_Increase ~ scale(average_energy_bill_2021), data = Scot_data)

summary(model1_scale)


#Now the intercept is more meaningful. It tells us the expected increase of house price with an average energy bill.  In other words, when an area's energy bill is at the average value across Scotland, the expected increase of house price in that area would be 20.974.

#Slope is the number of units by which Y (the outcome variable) increases on average, as X (the predictor variable) increases by a unit. In this case, the slope shows that energy bill increases by 1 unit, the expected increase of house price increased by -£1,436, or decreases by £1,436. HOWEVER, the results is insignificant (p = .39), suggesting that house price increase is not associated with energy bill.

#Note that the overall model quality is low. The R-squared and adjusted R-squared shows how much variance the predictor can explain - in this case, nearly zero! (Note the negative number for the adjusted R-squared. The formula for adjusted R square allows it to be negative. It is intended to approximate the actual percentage variance explained. So if the actual R square is close to zero the adjusted R square can be slightly negative. You can just think of it as an estimate of zero). Finally, the last line of the model output tells us this model is not better than a null model which does not include any predictor : F(1, 30) = 0.75, P < .39.

#We can test this manually by building a null model which includes no predictor but only the intercept.

#Fit a regression model including no predictor, using lm() function.

model_null <- lm (Housing_Increase ~ 1, data = Scot_data)


#We then compare it with our model1 which had "energy bill" as a predictor.

anova(model_null, model1_scale)

#Indeed model comparison suggests that our model1 (the model that included "energy_bill" as a predictor) is not better a null model without a predictor. Note that the model comparison gave us same results as the model output of model1. 

#We can conclude that house price increase is not associated with energy bill.

## 2.2 Is alcohol consumption associated with the increase of house price?

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


#The plot seems to suggest that there is a trend - as the consumption of alcohol increases, the housing increase becomes smaller. In other words, for the areas that consumed more alcohol, their house price increased less than other areas. 

#Is this difference significant statistically? 
  
#Let's test this in a regression model. Fit a regression model including scaled alcohol consumption as a predictor, using lm() function.

#model2 <- lm (Housing_Increase ~ scale(Alcohol), data = Scot_data)

# summary(model2)


### Discussion Point
#### Can you explain the model results? 
#### Discuss this with your neighbours and try to interpret the results.

#An example answer: 

#The model intercept is significant (P < .001) and it tells us that the expected increase of house price in Scotland is £20,974 on average, for an area that consummed average amout of alcohol.  

#The slope (Alcohol) tells us that, as Alcohol consumption increases by 1 unit, the increase of house price increase is increased by -£3,812, i.e., reduced by £3,812. Note that this predictor is significant, p < .05.

#This model can explain 14.43% of the variance. It is significantly better than the null model, F(1, 30) = 6.23, p < .05.

#We can conclude that the increase of house price is associated with alcohol consumption.

## 2.3 Is the increase of house price dependant of location (Rural vs Urban)?

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


#How to read a box plot? https://www.simplypsychology.org/boxplots.html

#We can compare the medians of box plots. If the median line of a box plot lies outside of the box of a comparison box plot, then there is likely to be a difference between the two groups.

#It looks like location is not associated with the increase of housing price.

#Let's test this in a regression model. Fit a regression model including the 2-level factor "location" as a predictor, using lm() function.


#model3 <- lm (Housing_Increase ~ location, data = Scot_data)

#summary(model3)


### How do we interpret the results when the predictor is a categorical vairable?

#Hint: The intercept now is the expected y when x is at the reference level.

#R automatically uses dummy coding and choose a reference level for us based on the alphabetic order of the level name. For example here "Rural" is chosen as the reference level as letter "R" comes before letter "U" for "Urban". You can mannually change the reference level using the relevel() function.

#Intercept: The expected increase of house price is £20945.58 in Rural areas.

#Slope: The expected increase of house price for the Urban areas is £69.42 more than Rural areas. However, this difference is NOT significant.

#We can conclude that there is no difference between the two categories,in other words, housing increase is a general trend that occurs in both Urban and Rural areas.

## 2.4 Is Deprivation associated with increase of house price?

#Note that there are two measures of Deprivation in this dataset. One is "ISMD" which is a continuous vairable. Another is named "Deprivation", which is a 2-level factor.


### 2.4.1 Using "SIMD" as predictor

#First let's recall what SIMD is. 

#SIMD ranks data zones from most deprived (ranked 1) to least deprived (ranked 6,976). More deprived areas have smaller SIMD values. 

#https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/#supportingdocuments

### Exercise 2(A)

#Can you try to plot the relation between Housing_Increase and SIMD ?

# Plot the relation
# consider using geom_smooth() to superimpose the best-fitting line describing the association of interest

Plot_housing4a <- 
    ggplot(data = Scot_data,
           aes(x=SIMD, 
               y=Housing_Increase))+
    geom_point() +
    geom_smooth(method = "lm", se=TRUE)+
    labs(x= "SIMD",
         y = "House Price Increase from 2021-2022",
         )

Plot_housing4a


#It looks like on average the increase of housing price becomes larger from more deprived areas to less deprived areas.

#Is this difference significant statistically? 

### Exercise 2(B)

#Can you try to fit a regression model using SIMD as a predictor and Housing_Increase as the outcome variable? 
#Let's test this in a regression model. Fit a regression model including the continuous variable "SIMD" as a predictor, using lm() function.

model4a <- lm (Housing_Increase ~ scale(SIMD), data = Scot_data)

summary(model4a)


#Can you Interpret the results?
  
  #Model results suggests that SIMD is MARGINALLY significant. As SIMD increase by 1 unit, the increase of house price seems to increase by £2,951. However, this association is only marginally significant.


### 2.4.2 Using "Deprivation" as predictor 

### Exercise 2(C)
#Can you try to plot the relation between Housing_Increase and Deprivation ? Hint, for categorical predictor, it is better to use box plot rather than scatter plot.


# Remember when dealing with a categorical predictor, it would make sense to use box plot instead of scatter plot

Plot_housing4b <- 
  ggplot(data = Scot_data,
         aes(x=Deprivation, 
             y=Housing_Increase, 
             fill=Deprivation))+
  geom_boxplot() +
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "Deprivation Level",
       y = "House Price Increase from 2021-2022",
  )

Plot_housing4b


#It looks like on average housing price increase is smaller for more deprived area (but be careful with the variances in the less deprived areas). 

#Is this difference significant statistically? 
  
  ### Exercise 2(D)
  
 # Can you try to fit a regression model using Deprivatoin as a predictor and Housing_Increase as the outcome variable?
 # Let's test this in a regression model, including the 2-level factor "Deprivation" as a predictor, using lm().

model4b <- lm (Housing_Increase ~ Deprivation, data = Scot_data)

summary(model4b)

#Can you interpret the results?
#Hint: Remember the intercept now is the expected y when x is at the reference level.

#Intercept: The expected increase of house price is £22,814 for those areas labelled as "Least" deprived.

#Slope: The expected increase of house price for the "Most" deprived areas is £3,680 less than the "Least" deprived areas. HOWEVER, this difference is NOT significant.

#We can conclude that there is no difference between the two categories,in other words, housing increase is a general trend that occurs in both deprived and and less deprived areas.


## 2.5 Multiple regression (i.e., with multiple predictors)

#We can have more than one predictors in a single regression.  

#From the above we know that the increase of house price is significantly associated with alcohol consumption, and also with SIMD but only marginally significantly. Let's see what happens if we include both alcohol and SIMD in the model.

model5 <- lm(Housing_Increase ~ scale(SIMD) + scale(Alcohol)  , data = Scot_data)

summary(model5)


#Now we see that if we include them both, neither factor remains significant.

#Is this model better than the model only including Alcohol as a predictor (model2) or the one only including SIMD as a predictor (model4a)?
  
#We can compare model fit using the anova() function again. Remember we used it when compared our very first model with a null model? We do exactly the same here.

#Use anova() to compare the two-predictor model with the one-predictor model that including only Alcohol as a predictor.

anova (model2, model5)

#The results show that including both Alcohol and SIMD as predictors did not improve model fit compare to either one-predictor model (F(1) =.01, p=.91). 

### Exercise 2(E)
#Use anova() to compare the two-predictor model with the one-predictor model that including only SIMD as a predictor.

anova (model4a, model5)

#Did the two-predictor model improve model fit?
  
#Answer
#No, it did not.The results show that including both Alcohol and SIMD as predictors did not improve model fit compare to a one-predictor model that only include SIMD as a predictor (F(1) =.2.43, p=.13). 

### Discussion Point
#Why do you think this is the case? Can you plot the data including both predictors and see if it gives us any hint?
  
 # To plot data using two continuous predictors, we first need to split one numeric predictor into groups. We can do this by using cut() function to add a new variable called “SIMD_group” and create a new data frame called "Scot".

Scot <- Scot_data %>%
  mutate(
    SIMD_group = cut(SIMD, 5)
  )


# now plot the data with two predictors using the new data
Plot_housing5 <- 
  ggplot(data =Scot,
         aes(x=Alcohol, 
             y=Housing_Increase, 
             colour = SIMD_group))+
  geom_point() +
  geom_smooth(method = "lm", se=FALSE)+
  labs(x= "Alcohol Consumption",
       y = "House Price Increase from 2021-2022",
  ) 

Plot_housing5


#Look at the plot, does it give you any idea why our significant or marginally significant predictor became insignificant in the multiple regression model?
  
 # Answer
#Alcohol consumption is associated with the increase of house price in very different ways for different SIMD_group and the effects cancelled each other out.   

# 3 Extra Exercise 

#Now we have learnt something about the increase of house price. What about rent? Can you explore what factors influence rent increase?
  
#Feel free to work as a team or individually

#Before you check the suggestions below, give it a try and see what you yourselves can find out. 

## 3.1 Is energy bill associated with rent increase?


# Plot the relation
Plot_rent <- 
  ggplot(data = Scot_data,
         aes(x=average_energy_bill_2021, 
             y=Rent_Increase))+
  geom_point() +
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "average_energy_bill_2021",
       y = "Rent Increase from 2021-2022",
  )

Plot_rent


#Fit a regression model including energy bill as a predictor, using lm().

rent_model1 <- lm (Rent_Increase ~ scale(average_energy_bill_2021), data = Scot_data)

summary(rent_model1)


### How do you interpret the results? What can you conclude?

### Answer
#Energy bill is not associated with rent increase ($\beta B$ = .50, SE = 3.88, p = .90).

## 3.2 Is alcohol consumption associated with rent increase?


# Plot the relation
Plot_rent2  <- 
  ggplot(data = Scot_data,
         aes(x=Alcohol, 
             y=Rent_Increase))+
  geom_point() +
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "Alcohol Consumption",
       y = "Rent Increase from 2021-2022",
  )

Plot_rent2


#Fit a regression model including Alcohol as a predictor, using lm().
#Remember you can scale the predictor to make the intercept more meaningful.


rent_model2 <- lm (Rent_Increase ~ scale(Alcohol), data = Scot_data)

summary(rent_model2)



### How do you interpret the results? What can you conclude?

### Answer
#Alcohol consumption is not associated with rent increase ($\beta B$ = -3.48, SE = 3.83, p = .37).

## 3.3 Is location accociated with rent increase?

# Plot the relation
# consider using geom_smooth() to superimpose the best-fitting line describing the association of interest

Plot_rent3 <- 
  ggplot(data = Scot_data,
         aes(x=location, 
             y=Rent_Increase, 
             fill=location))+
  geom_boxplot() +
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "Location (Rural vs Urban)",
       y = "Rent Increase from 2021-2022"
  )

Plot_rent3


#Let's test this in a regression model. Fit a regression model including location as a predictor.

rent_model3 <- lm (Rent_Increase ~ location, data = Scot_data)

summary(rent_model3)


### How do you interpret the results? What can you conclude?

### Answer
Location is not associated with rent increase ($\beta B$ = -9.81, SE = 7.57, p = .21).


## 3.4 Is Deprivation associated with rent increase?

### 3.4.1 Using 2-level factor "Deprivation" as a predictor

# Plot the relation
Plot_rent4 <- 
    ggplot(data = Scot_data,
           aes(x=Deprivation, 
               y=Rent_Increase, 
               fill=Deprivation))+
    geom_boxplot() +
    geom_smooth(method = "lm", se=TRUE)+
    labs(x= "Deprivation Level",
         y = "Rent Increase from 2021-2022",
         )

Plot_rent4


#Let's test this in a regression model. Fit a regression model including the 2-level factor "Deprivation" as a predictor.

rent_model4a <- lm (Rent_Increase ~ Deprivation, data = Scot_data)

summary(rent_model4a)


### How do you interpret the results? What can you conclude?

### Answer
#Deprivation level is not associated with rent increase ($\beta B$ = -3.00, SE = 7.62, p = .70).

### 3.4.2 Using continuous variable SIMD as a predictor

#Plot the relation using geom_smotth (method = "lm").

Plot_rent4b <- 
  ggplot(data = Scot_data,
         aes(x=SIMD, 
             y=Rent_Increase))+
  geom_point() +
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "SIMD",
       y = "Rent Increase from 2021-2022",
  )

Plot_rent4b


#Let's test this in a regression model. Fit a regression model including the continuous variable SIMD as a predictor.

rent_model4b <- lm (Rent_Increase ~ SIMD, data = Scot_data)

summary(rent_model4b)


### How do you interpret the results? What can you conclude?

### Answer
#SIMD is not associated with rent increase ($\beta B$ = -3.00, SE = 7.62, p = .70).
