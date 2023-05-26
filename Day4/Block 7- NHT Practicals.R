# #### Statistical Analysis Practical Solutions ####

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
