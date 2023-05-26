#############################################################
################# Statistical Analysis ######################
#############################################################

##### PART 1: Descriptive Statistics ####

library(tidyverse)

Scot_data <- read_csv("Day3/DataWrangling/outputs/Full_Scottish_Data.csv")

# We can use descriptive statistics to get some basic information from our data.

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

# summary_stats can be used to summarise much of this information

# Calculate basic summary statistics
summary_stats <- Scot_data %>%
  summarize(
    Mean_population = mean(Population),
    Median_SIMD = median(SIMD),
    SD_Food_insc = sd(food_insecurity2018_2022)
  )

# Print the summary statistics
print(summary_stats)


# #### PART 2: Null Hypothesis Testing ############################
# We can use Null Hypothesis Testing (NHT) to try and reject the Null Hypothesis, this allows us to better understand whether or not the results we are seeing are statistically significant, or might just be explained by random noise etc.

# ==== Two Sample t.tests ===================================
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

# ==== One Sample t.tests ====
# For one sample t.tests, we can test the mean of a population against a hypothetical mean.

# In this case, we can test the raise of average rent costs in Scotland between 2021 and 2022 and the UK mean. The percentage of increase in rent across Uk between June 2021 and June 2022 has been of 9.5% (browseURL(https://www.haart.co.uk/landlords/landlord-advice/rent-increase-notices-what-is-a-fair-rent-increase/)) 

# We can use this figure as a 'mu' value to run a one sample t.test.
mean(authority_data_cleaned$Rent) # Rent raise seems lower, but is this statistically significant?

t.test(authority_data_cleaned$Rent, mu = 9.5) # With a p value below 0.05, the Null Hypothesis can be rejected, and it seems there is a significant difference between Scotland raise in rent and general UK one (note these are imperfect data, with the Scottish sample include the whole 2022).


# ==== ANOVA Tests ====
# ANOVA tests can determine the impact of an independent variable on a quantitative dependent variable of a data set. 

# For example, how the rent increase impact the different deprivation areas.
summary(aov(Homeless ~ SIMDQuint, data = authority_data_cleaned)) # ANOVA tests can use the summary() function and the aov() function to give helpful outputs. The quantitative variable being tested is on the left separated from the independent variables by a ~. You can add multiple independent variables separated by +. Assign the data source with data = .


# The p.value is below 0.05, so it seems that deprivation does play a significant role in the increase of the Homeless applications overall but lets see in more detail 
# Save the result of the anova
AnovaResult<-aov(Homeless ~ SIMDQuint, data = authority_data_cleaned)

# Now use Tukey multiple pairwise-comparisons
TukeyTest <- TukeyHSD(AnovaResult)
print(TukeyTest)
#Check across all possible comparison

# ---- Practical 2 ----
# Using an ANOVA test, determine the extent to which deprivation, the Urban-Rural divide and the areas of Scotland are each significant factors in determining increase/decrease in Food Insecurity.

# This is juts an introduction to hypothesis testing, but we will continue over the rest of the day looking at different statistical analyses.


# #### END ####
