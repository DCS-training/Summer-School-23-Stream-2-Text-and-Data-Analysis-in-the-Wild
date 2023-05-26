# #### Statistical Analysis Practical Solutions ####

### Exercise 1(A)
# Create a new column showing rent increase.
Scot_data$Rent_Increase <- Scot_data$average_rent_2022 -  Scot_data$average_rent_2021

summary(Scot_data$Rent_Increase)


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








# ==== Practical 1 ====
# What about homless application? Check the difference in increase/decrease of homeless application between urban and rural authorities. Use a two sample t.test and determine whether or not the Null Hypothesis, that the Urban and Rural distinction is not a major factor, can be rejected.

mean(Rural$Homeless)
mean(Urban$Homeless) # Urban journey times are shorter on average. Let's test the Null Hypothesis 
one_sample_ttest <- t.test(Rural$Homeless, Urban$Homeless)

print(one_sample_ttest) # In this case, we can see that the p value is = below 0.05, and so, the Null Hypothesis can be rejected, and we can conclude that there is a statistically significant impact on the raise of rents between urban and rural areas. 


# ==== Practical 2 ====
# Using an ANOVA test, determine the extent to which deprivation, the Urban-Rural divide and the areas of Scotland are each significant factors in determining increase/decrease in House Prices.

summary(aov(Homeless ~ SIMDQuint + location + region, data = authority_data_cleaned))


# Looking at this, the Null Hypothesis that the Urban-Rural divide and the region have no impact on homeless applications. 
