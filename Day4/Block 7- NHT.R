# #### Statistical Analysis ####
# #### PART 1: Descriptive Statistics ####

library(tidyverse)

Scot_data <- read_csv("Data/Full_Scottish_Data.csv")

# We can use descriptive statistics to get some basic information from our data.

# mean calculates the mean, and so we can compare mean rent across Scotland between 2021 an 2022.

mean(Scot_data$average_rent_2021)
mean(Scot_data$average_rent_2022)
# This suggests rent is increasing.

# median can be used in a similar way.
median(Scot_data$`welfare_applications2020-2021`)
median(Scot_data$`welfare_applications2021-2022`)

# We can also just sum data if we want totals.
sum(Scot_data$`homelessness_applications2021-21`)
sum(Scot_data$`homelessness_applications 2021-2022`)
# This shows an increase in welfare and homelessness applications.

# sd can be used to calulate the standard deviation ofa dataset. This can be helpful to udnerstand how different each observation is across the dataset.
sd(Scot_data$average_energy_bill_2021) 
mean(Scot_data$average_energy_bill_2021) # This is quite a small value compared to the mean, and so energy bills seem quite consistent across authorities.

# summary_stats can be used to summarise much of this information

# Calculate basic summary statistics
summary_stats <- Scot_data %>%
  summarize(
    Mean_population = mean(Population),
    Median_SIMD = median(SIMD),
    SD_Food_insc = sd(`food_insecurity2018-2022`)
  )

# Print the summary statistics
print(summary_stats)


# #### PART 2: Null Hypothesis Testing ####
# We can use Null Hypothesis Testing (NHT) to try and reject the Null Hypothesis, this allows us to better udnerstand whether or not the results we are seeing are statistically significant, or might just be explained by random noise etc.

# ==== Two Sample t.tests ====
# Let's test whether or not the increases we observed between 2021 and 2022.

two_sample_ttest <- t.test(Scot_data$average_rent_2022, Scot_data$average_rent_2021)

print(two_sample_ttest) # Our p value is significantly larger than 0.05, and so the Null Hypothesis cannot be rejected.

# We can also use this to see if specific factors make a difference. For example, are energy bills significantly higher between urban and rural authorities.

Rural <- subset(Scot_data, Scot_data$location =='Rural')
Urban <- subset(Scot_data, Scot_data$location =='Urban')

mean(Rural$average_energy_bill_2021)
mean(Urban$average_energy_bill_2021)

# Rural energy bills are higher on average. The Null Hypothesis is that the Urban and Rural distinction is not a significant factor in this increase. We can use a t.test to try and reject this Null Hypothesis.
two_sample_ttest <- t.test(Rural$average_energy_bill_2021, Urban$average_energy_bill_2021)

print(two_sample_ttest) # Again, we cannot reject the Null Hypothesis.

# ---- Practical 1 ----

# What about public transport journey times to a GP? Check the difference between transport times to a GP (PT_GP) between urban and rural authorities. Use a two sample t.test and determine whether or not the Null Hypothesis, that the Urban and Rural distinction is not a major factor, can be rejected.

# ==== One Sample t.tests ====
# For one sample t.tests, we can test the mean of a population against a hypothetical mean.

# In this case, we can test alcohol consumption between England and Scotland. The percentage of adults drinking more than 14 units a week in England is reported as 22.8% between 2015 and 2018 (browseURL("https://www.gov.uk/government/statistics/local-alcohol-profiles-for-england-lape-july-2021-update/local-alcohol-profiles-for-england-short-statistical-commentary-july-2021"))

# We can use this figure as a 'mu' value to run a one sample t.test.
mean(Scot_data$Alcohol) # Alcohol consumption seems lower, but is this statistically significant?

t.test(Scot_data$Alcohol, mu = 0.228) # With a p value below 0.05, the Null Hypothesis can be rejected, and it seems there is a significant difference between alcohol consumption in England and Scotland (note these are imperfect data, with the Scottish sample being more recent).

# ---- Practical 2 ----
# Does deprivation have an impact on alcohol consumption? Check the most deprived 16 authorities alcohol consumption and see if it is significantly higher than the theoretical mean/mu value of 0.2 (rough rate of alcohol abuse across Scotland).

# ==== ANOVA Tests ====
# ANOVA tests can determine the impact of an independent variable on a quantitative dependent variable of a dataset. 

# For example, lets see how deprivation impacts life expectancy.
Deprived <- subset(Scot_data, Scot_data$Deprivation == 'Most')
Less_deprived <- subset(Scot_data, Scot_data$Deprivation == 'Least')

mean(Deprived$life_expectancy_2022)
mean(Less_deprived$life_expectancy_2022) # We can see the mean life expectancy is definitely higher in the less deprived areas.

summary(aov(life_expectancy_2022 ~ Deprivation, data = Scot_data)) # ANOVA tests can use the summary() function and the aov() function to give helpful outputs. The quantitative variable being tested is on the left separated from the independent variables by a ~. You can add multiple independent variables separated by +. Assign the data source with data = .

# The p.value is well below 0.05, so it seems that deprivation plays a significant role negatively affecting life expectancy.

# ---- Practical 3 ----
# Using an ANOVA test, determine the extent to which deprivation, alcohol abuse and the Urban-Rural divide are each significant factors in determining life expectancy.

# This is juts an introduction to hypothesis testing, but we will continue over the rest of the day looking at different statistical analyses.


# #### END ####
