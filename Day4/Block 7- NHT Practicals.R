# #### Statistical Analysis Practical Solutions ####

# ==== Practical 1 ====
# What about public transport journey times to a GP? Check the difference between transport times to a GP (PT_GP) between urban and rural authorities. Use a two sample t.test and determine whether or not the Null Hypothesis, that the Urban and Rural distinction is not a major factor, can be rejected.

mean(Rural$PT_GP)
mean(Urban$PT_GP) # Urban journey times are shorter on average. Let's test the Null Hypothesis 
one_sample_ttest <- t.test(Rural$PT_GP, Urban$PT_GP)

print(one_sample_ttest) # In this case, we can see that the p value is well below 0.05, and so, the Null Hypothesis can be rejected, and we can conclude that there is a statistically significant impact on journey times to a GP's office between rural and urban authorities in Scotland. 

# ==== Practical 2 ====
# Does deprivation have an impact on alcohol consumption? Check the most deprived 16 authorities alcohol consumption and see if it is significantly higher than the theoretical mean/mu value of 0.2 (rough rate of alcohol abuse across Scotland). Note, you will have to subset values below the median SIMD to get the 16 most deprived authorities.

Deprived <- subset(Scot_data, Scot_data$Deprivation == 'Most')

mean(Deprived$Alcohol) # There is a higher rate of alcohol abuse in the more deprived areas.

t.test(Deprived$Alcohol, mu = 0.2) # The p.value is below 0.05, and so the Null Hypothesis can be rejected. Deprivation seems to influence alcohol abuse in Scotland.


# ==== Practical 3 ====
# Using an ANOVA test, determine the extent to which deprivation, alcohol abuse and the Urban-Rural divide are each significant factors in determining life expectancy.

summary(aov(life_expectancy_2022 ~ Deprivation + location + Alcohol, data = Scot_data))

# Looking at this, the Null Hypothesis that the Urban-Rural divide has no impact on life-expectancy cannot be rejected, as the p.value is above 0.05. The Null Hypothesis for alcohol abuse can be rejected, just, but with more a limite dimpact on residuals, it seems it's role in affecting life expectancy is far less than deprivation.
