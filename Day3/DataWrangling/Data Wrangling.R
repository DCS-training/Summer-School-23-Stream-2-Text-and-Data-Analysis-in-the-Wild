##########  DATA WRANGLING ################

# RESEARCH QUESTIONS #######
# 1.not a question per se but how can we make sure to get the data to a stage that can make them usable for the analysis 
# 2.a corollary is that we want our data to satisfy the FAIR principles so you should never edit your raw data and all the pre- processing that can happen via R is better. This will make sure that if someone else wants to reproduce your steps they will be able to do so

# 1. Getting Set up Importing Data ==============
## 1.1. Packages needed--------------
library(tidyverse)

## 1.2. Import the data------------
# In order to work with your own data, you will need to import it into R
read_csv("Day3/DataWrangling/Data/AuthorityData.csv") # The read_csv() function in 'tidyverse' is the easiest way to do this

authority_data <- read_csv("Day3/DataWrangling/Data/AuthorityData.csv",locale = locale(encoding = "WINDOWS-1252")) # Rather than just seeing a single output, you can assign the data to a variable. The second part is to make sure £ symbol is encoded correctly

View(authority_data) # To view the data frame, or click on the object in the global environment

SIMD <- read_csv("Day3/DataWrangling/Data/simd2020_withinds.csv",locale = locale(encoding = "WINDOWS-1252"))

# 2. Sub setting Data =========================

## 2.1 Data Slicing ------
authority_data[,3] # Select a specific column
authority_data[5,] # Select specific row
authority_data[7,1] # Select a specific cell
authority_data[,'life_expectancy_2022'] 
authority_data$region # The name of columns can also be used

# NB if you select by position and you save the selection you are going to have a data frame with one variable if you select a variable with the df$variable you are going to have a vector/list 

# If you want to subset by df$variable and still see it as a df you need to add the as_tibble function
as_tibble(authority_data$life_expectancy_2022) # Viewing as a table makes it easier to visualise, though removing as_tibble() will still show the data

## 2.2. Extracting Data ----------------
Clackmannanshire <- authority_data[7,]# Select the row corresponding to Clackmannanshire by its position

Clackmannanshire

Clacks_SIMD <- subset(SIMD, SIMD$Council_area =='Clackmannanshire')# Select the 72 rows corresponding to areas in the Clackmannanshire from the SIMD dataset 

Clacks_SIMD

High_dep <- subset(SIMD, SIMD$SIMD2020v2_Rank <= 50)# Subset by values e.g. only those that have a rank < or equal to 50

High_dep

## 2.3. Change data class ---------------
class(High_dep$Council_area)# Check the class of the variable

High_dep$Council_area <- as_factor(High_dep$Council_area) # Change to factor

is.factor(High_dep$Council_area)# Check that the variable is now a factor 

#NB in general as.something will transform to that is.something will check if it is true (it is the same difference between = and ==)
levels(High_dep$Council_area)# Show the possible values within council area 

# 3. Tidying Data ====================
# We have some NULL and N/A values in our data frames. This is a common occurrence and it is important to have all of these be consistent for tidy data. Generally 'NA' is the best option, as these values can be filtered out where needed.

# For example if we want to sum all of the food bank parcels across all councils in 2022.
sum(authority_data$foodbank_parcels_2022)

# We get an error as the data type is 'character', which can't be summed, only numeric.

# Trying to set this to numeric, we get another error, because some of the rows in the column contain the characters 'NULL' or a comma etc..

## 3.1. Clean with Regex -------
# We can remove specific characters, such as commas, using gsub.
authority_data$foodbank_parcels_2022 <- gsub(",","",authority_data$foodbank_parcels_2022)

help(gsub) # gsub usage

## 3.2. Re-encode NULL Values ------

# We can then subset the 'NULL's and change them to a more appropriate value.
authority_data$foodbank_parcels_2022[authority_data$foodbank_parcels_2022 == "NULL"] # This subsets the data to only include cells where foodbank_parcels_2022 == 'NULL'. We can then change this subseted data to 'NA'.

authority_data$foodbank_parcels_2022[authority_data$foodbank_parcels_2022 == "NULL"] <- NA

authority_data[authority_data == "NULL"] <- NA # We can easily do this for the whole dataset.

authority_data[authority_data == "n/a"] <- NA # And for any other variations we want to change to 'NA'.

## 3.3. Re-encode as a different data type --------
# We should now have tidier null data values in our data.
authority_data$foodbank_parcels_2022 <- as.numeric(authority_data$foodbank_parcels_2022) # We can now change the data to be numeric.

sum(authority_data$foodbank_parcels_2022, na.rm = T) # And use the na.rm argument to remove NA values from our sum calculation, giving us the total known food bank parcels in 2022. It can also be appropriate in some cases to change null values to 0 etc., in which case na.rm would not be necessary. 

## 3.4. Additional Cleaning -------------
# What other cleaning do we need? 
# First check if everything is encoded correctly
summary(authority_data)
glimpse(authority_data) # glimpse can also be used, or we can click the dropdown arrow in the environment panel.

# house_price_jul_21
# house_price_jul_22
# average_household_gas_consumption_kwh_2021   
# total_consumption_kwh
# all should be numeric but they are not why? 
# can we just try to transform them all together?

authority_data_cleaned <- authority_data %>% 
  mutate_at(c(4:21), as.double)

# we get an error. This means that is not just an encoding issue there is something in it that is not a number. 
# some data are weird e.g. , in the thousand pound and there are £ symbols in there 
# Remove % and £ and re-encode as Continuous values
authority_data_cleaned <- mutate_if(authority_data, 
                                    is.character, 
                                    str_replace_all, 
                                    pattern = "[,£]", 
                                    replacement = "")

authority_data<-authority_data_cleaned %>% 
  mutate_at(c(4:21), as.double) #NB we create a new df authority data cleaned in the first step to make sure to not overwrite things now that we are happy everything is as it should we are overwriting the initial authority_data to the cleaned version of it 
summary(authority_data)# We are good to go

# One last thing the first row of our data set is actually the sum of Scotland so we do not want it there for our analyses.
authority_data <- authority_data[2:33,] # All columns but only rows 2:33

## 3.5. Check for Null values ------
# This will become more important tomorrow but let's check how many columns have null values
colSums(is.na(authority_data))

## 3.6. Cleaning a new data set -------
# Now let's look at the SIMD and check the problems there
summary(SIMD)
# broadband # nocentralheating_rate # overcrowded_rate # overcrowded_rate 
# crime_rate #crime_count # University # not_participating # Attainment
# Attendance # LBWT # DEPRESS # employment_rate # income_rate 
# These should really be numbers but they are encoded as characters. This is because % are present in the % variables and some null value is encoded as *
#Step 1 Remove % and * and re-encode as Continuous values
SIMD <- mutate_if(SIMD, #change if is character
                  is.character, 
                  str_replace_all, 
                  pattern = "[%*]", #What I am searching
                  replacement = "")#What I am replacing with

# Now that we have removed all the weird symbols we can transform the columns containing numeric values into proper numeric ones 
SIMD <- SIMD %>% 
  mutate_at(c(4:49), as.double)

# Check now 
summary(SIMD)# Good to go


# 4. Practical 1 ==========================
# Create a new column in authority_data that shows the difference between foodbank_parcels in 2021 and 2022, and check whether there was an overall increase or decrease. Use authority_data$Foodbank_difference <- to assign a new column. Note how the presence of null values will impact this (you can create basic equations and use variables as values).


# 5. Combining Data sets ==========================
# We can combine datasets that have shared data using the merge() function. Looking at the authority_data and SIMD data frames, the authority and Council_area columns have the same data. 

## 5.1 Group by SIMD Data set ----------
# First we will need to combine the multiple rows of each council area in SIMD into single occurrences. For this, we can use pipes and the group_by() function.

SIMD$Council_area <- as.factor(SIMD$Council_area) # Changing the data type to a factor is best practice here, although it should still work as a character type.

Councils <- SIMD %>% 
  group_by(Council_area) %>% 
  summarize(Population=sum(Total_population),
            SIMDQuint=as.factor(floor(mean(SIMD2020v2_Quintile))),
            SIMDDecil=as.factor(floor(mean(SIMD2020v2_Decile))),
            Alcohol= round(mean(ALCOHOL, na.rm = T)), # One ward in Glasgow has NA value, so na.rm = T will discount this from the mean calulation.
            PT_GP=round(mean(PT_GP)))


## 5.2 Practical 2 -------------
# Try to add the mean SIMD2020v2_Rank as a new column in the new Councils data frame. You will need to use group_by again.

## 5.3. Combining the Tidied Data --------
authority_data$authority <- as.factor(authority_data$authority) # Make sure both shared columns are the same data type.

Merged_data <- merge(authority_data, Councils, by.x = 'authority', by.y = 'Council_area') # The merge(function) can take several arguments, the first two being the two data frames to merge. Using by we can let the function know which column has the shared data, in this case, since they have different names, we have to us by.x and by.y (NB, as always, R is case sensitive, so if there are any slight differences in naming, the merge won't work properly, so keep an eye on that. These data SHOULD be fromatted the same)


# 6. Final Practicals ========

## 6.1. Practical 3 --------------
# Does the council area with the highest deprivation receive the most food bank parcels?

## 6.2. Practical 4 ---------------
# Create a new column for food bank parcels (in 2021 and 2022) per 1,000 population. Now check whether the council of highest deprivation has the most food bank parcels per capita.

## 6.3. Practical 5 -------------
# Create a new column with the average time traveled to a GP by Public Transport (SIMD$PT_GP) for each council. Add this to the merged column and check how the councils of lower and higher deprivation compare in this category.


# END-ish ####
# There is a lot more that is possible with R in terms of data wrangling. But now that we have an understanding of this, and some tidier, computer friendly data, we can start to check some of the observations we have made in these practicals through statistical analyses. 

# 7. Bonus: setting for tomorrow =============
#This is in preparation for tomorrow 
# On our data set for each variable there are two value normally 2020 and 2022 so we want to analyse the increase and decrease. For some of the techniques we are going to look tomorrow it is better if they are all in the same scale so we will transform them all in % increase/ decrease 

## 7.1. Create new variables % increases -----------
# Food insecurity
authority_data$FoodInsecurity <- round(((authority_data$food_insecurity2018_2022-authority_data$food_insecurity2017_2021)/authority_data$food_insecurity2017_2021)*100,2) 
# House prices
authority_data$HousePrices<- round(((authority_data$house_price_jul_22-authority_data$house_price_jul_21)/authority_data$house_price_jul_21)*100,2) 
# Welfare applications
authority_data$WelfareApp<-round(((authority_data$welfare_applications2021_2022-authority_data$welfare_applications2020_2021)/authority_data$welfare_applications2020_2021)*100,2) 
# Homeless applications
authority_data$Homeless<- round(((authority_data$homelessness_applications2021_2022-authority_data$homelessness_applications2020_21)/authority_data$homelessness_applications2020_21)*100,2)
# N of business
authority_data$Business<- round(((authority_data$businesses_2022-authority_data$businesses_2021)/authority_data$businesses_2021)*100,2)
# Foodbank parcels
authority_data$Foodbank<-round(((authority_data$foodbank_parcels_2022-authority_data$foodbank_parcels_2021)/authority_data$foodbank_parcels_2021)*100,2)
# Average rent
authority_data$Rent<-round(((authority_data$average_rent_2022-authority_data$average_rent_2021)/authority_data$average_rent_2021)*100,2)
## 7.2. Subset only the column I want ------------:
authority_data_cleaned<-authority_data[,c(1:3,6,13,15,21:29)]# Nice cleaned data set for tomorrow
## 7.3. Add variables to SIMD ---------- 
# Add some variables from the SIMD Data set 
FromSIMD <- SIMD %>% 
  group_by(Council_area) %>% 
  summarize(Population=sum(Total_population),
            SIMDQuint=as.factor(floor(mean(SIMD2020v2_Quintile))),
            SIMDDecil=as.factor(floor(mean(SIMD2020v2_Decile))),
            Alcohol= round(mean(ALCOHOL)),
            PT_GP=round(mean(PT_GP)))

Merge <- merge(authority_data_cleaned, FromSIMD, by.x = 'authority', by.y = 'Council_area')
# Rename Merge as authority_data_cleaned to export 
authority_data_cleaned<-Merge


## 7.4. Save them ---------------
# Just to make sure we have it ready tomorrow let's save it as an output
write_csv(authority_data_cleaned, "Day3/DataWrangling/outputs/authority_data_cleaned.csv")

write_csv(Merged_data, "Day3/DataWrangling/outputs/Full_Scottish_Data.csv")
