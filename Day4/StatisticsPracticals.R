# #### Statistical Analysis Practical Solutions ####

## 2.2 Exercise 1 =====================
# Summarising statistics of our data:
# Create a new column showing rent increase
Scot_data$Rent_Increase <- Scot_data$average_rent_2022 -  Scot_data$average_rent_2021
summary(Scot_data$Rent_Increase)


## 2.5 Exercise 2 =====================
# Can you provide summary statistics to show the mean of Population, the median of SIMD, and the sd of food_insecurity2018_2022?

summary_stats <- Scot_data %>%
  summarize(
    Mean_population = mean(Population),
    Median_SIMD = median(SIMD),
    SD_Food_insc = sd(food_insecurity2018_2022)
  )


### 3.4 Exercise 3 --------------
# (A) Can you try to plot the relation between Housing_Increase and SIMD ?
# Plot the relation
# consider using geom_smooth() to superimpose the best-fitting line describing the association of interest
ggplot(data = Scot_data,
         aes(x=SIMD, 
             y=Housing_Increase))+
  geom_point() +
  geom_smooth(method = "lm", se=TRUE)+
  labs(x= "SIMD",
       y = "House Price Increase from 2021-2022",
  )
# It looks like on average the increase of housing price becomes larger from more deprived areas to less deprived areas.
# Is this difference significant statistically? 

# (B) Can you try to fit a regression model using SIMD as a predictor and Housing_Increase as the outcome variable? 
#Let's test this in a regression model. Fit a regression model including the continuous variable "SIMD" as a predictor, using lm() function.
model4a <- lm (Housing_Increase ~ scale(SIMD), data = Scot_data)
summary(model4a)

# (C) Can you Interpret the results?
# Model results suggest that SIMD is MARGINALLY significant. As SIMD increase by 1 unit, the increase of house price seems to increase by £2,951. However, this association is only marginally significant.

### 3.5.1. Exercise 4 --------------
# Is the two-predictor model (SIMD + Alcohol) better than the one-predictor(SIMD only) model? 
#Use anova() to compare the two-predictor model with the one-predictor model that including only SIMD as a predictor.
anova (model4a, model5)

#Did the two-predictor model improve model fit?
#Answer
#No, it did not.The results show that including both Alcohol and SIMD as predictors did not improve model fit compare to a one-predictor model that only include SIMD as a predictor (F(1) =.2.43, p=.13). 

### 4.1.3 Exercise 5------------------

# What about homeless application? Check the difference in increase/decrease of homeless applications between urban and rural authorities. Use a two sample t.test and determine whether or not the Null Hypothesis, that the Urban and Rural distinction is not a major factor, can be rejected.
mean(Rural$Homeless)
mean(Urban$Homeless) # Urban journey times are shorter on average. Let's test the Null Hypothesis 
one_sample_ttest <- t.test(Rural$Homeless, Urban$Homeless)

print(one_sample_ttest) # In this case, we can see that the p value is = below 0.05, and so, the Null Hypothesis can be rejected, and we can conclude that there is a statistically significant impact on the raise of rents between urban and rural homeless applications. 


### 4.3.2 Exercise 6 ----
# Using an ANOVA test, determine the extent to which deprivation, the Urban-Rural divide and the areas of Scotland are each significant factors in determining increase/decrease in Homeless Applications.

summary(aov(Homeless ~ SIMDQuint + location + region, data = authority_data_cleaned))

# Looking at this, the Null Hypothesis that the Urban-Rural divide have an impact on homeless applications. 
