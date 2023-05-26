###########################################################################
#  PART 1: IMPORTING AND MANIPULATING DATA 
###########################################################################

# Research Questions:
# 1.not a question per se but how can we make sure to get the data on a stage that can make them usable for the analysis 
# 2.a corollary is that we want our data to satisfy the FAIR principles so you should never edit your raw data and all the pre- processing that can happen via R is better. This will make sure that if someone else wants to reproduce your steps they will be able to do so

# ==== Importing Data =======================
# Packages needed
library(tidyverse)

# In order to work with your own data, you will need to import it into R
read_csv("Day3/DataWrangling/data/AuthorityData.csv") # The read_csv() function in 'tidyverse' is the easiest way to do this

authority_data <- read_csv("Day3/DataWrangling/data/AuthorityData.csv",locale = locale(encoding = "WINDOWS-1252")) # Rather than just seeing a single output, you can assign the data to a variable. The second part is to make sure £ symbol is encoded correctly

View(authority_data) # To view the data frame, or click on the object in the global environment

SIMD <- read_csv("Day3/DataWrangling/data/simd2020_withinds.csv",locale = locale(encoding = "WINDOWS-1252"))

# ==== Sub setting Data =========================

# ----  Data ---------------------------------
authority_data[,3] # Select a specific column
authority_data[5,] # Select specific row
authority_data[7,1] # Select a specific cell
authority_data[,'life_expectancy_2022'] 
authority_data$region # The name of columns can also be used

#NB if you select by position and you save the selection you are going to have a dataframe with one variable if you select a variable with the df$variable you are going to have a vector/list 

#If you want to subset by df$variable and still see it as a df you need to add the as_tibble function in front
as_tibble(authority_data$life_expectancy_2022) # Viewing as a table makes it easier to visualise, though removing as_tibble() will still show the data

# ---- Extracting Data ----------------------------
Clackmannanshire <- authority_data[7,]# Select the row corresponding to Clackmannanshire by its position

Clacks_SIMD <- subset(SIMD, SIMD$Council_area =='Clackmannanshire')# Select the 72 rows corresponding to areas in the Clackmannanshire from the SIMD dataset 

High_dep <- subset(SIMD, SIMD$SIMD2020v2_Rank <= 50)# Subset by values e.g. only those that have a rank < or equal to 50

# Change data class -----------------------------
class(High_dep$Council_area)# check what class my variable is
High_dep$Council_area <- as_factor(High_dep$Council_area)
is.factor(High_dep$Council_area)# Check if they are factor now 
#NB in general as.something will transform to that is.something will check if it is true (it is the same difference between = and ==)
levels(High_dep$Council_area)# Show the possible values within council area 

# ==== Tidying Data ====================

# We have some NULL and N/A values in our data frames. This is a common occurrence and it is important to have all of these be consistent for tidy data. Generally 'NA' is the best option, as these values can be filtered out where needed.

# For example if we want to sum all of the food bank parcels across all councils in 2022.
sum(authority_data$foodbank_parcels_2022)

# We get an error as the data type is 'character', which can't be summed, only numeric.

# Trying to set this to numeric, we get another error, because some of the rows in the column contain the characters 'NULL' or a comma.

# We can remove specific characters, such as commas, using gsub.
authority_data$foodbank_parcels_2022 <- gsub(",","",authority_data$foodbank_parcels_2022)

# We can then subset the 'NULL's and change them to a more appropriate value.
authority_data$foodbank_parcels_2022[authority_data$foodbank_parcels_2022 == "NULL"] # This subsets the data to only include cells where foodbank_parcels_2022 == 'NULL'. We can then change this subseted data to 'NA'.
authority_data$foodbank_parcels_2022[authority_data$foodbank_parcels_2022 == "NULL"] <- NA

authority_data[authority_data == "NULL"] <- NA # We can easily do this for the whole dataset.

authority_data[authority_data == "n/a"] <- NA # And for any other variations we want to change to 'NA'.

# We should now have tidier null data values in our data.
authority_data$foodbank_parcels_2022 <- as.numeric(authority_data$foodbank_parcels_2022) # We can now change the data to be numeric.

sum(authority_data$foodbank_parcels_2022, na.rm = T) # And use the na.rm argument to remove NA values from our sum calculation, giving us the total known foodbank parcels in 2022. It can also be appropriate in some cases to change null values to 0 etc., in which case na.rm would not be necessary. 

# What other cleaning we need? 
# First check if everything is encoded correctly
summary(authority_data)
# house_price_jul_21
# house_price_jul_22
# average_household_gas_consumption_kwh_2021   
# total_consumption_kwh
# all should be numerics but they are not why? 
# can we just try to transform them all together?

authority_data_cleaned<-authority_data %>% 
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

# One last thing the first row of our dataset is actually the sum of scotland so we do not want it there 
authority_data<- authority_data[2:33,]#All column but only row 2:33

# This will become more important tomorrow but let's check how many columns have null values
colSums(is.na(authority_data))

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


# ==== Practical 1 ==========================
# Create a new column in authority_data that shows the difference between foodbank_parcels in 2021 and 2022, and check whether there was an overall increase or decrease. Use SSch$Foodbank_difference <- to assign a new column. Note how the presence of null values impact this 


# ==== Combining Datasets ==========================
# We can combine datsets that have shared data using the merge() function. Looking at the authority_data and SIMD dataframes, the authority and Council_area columns have the same data. 

# First we will need to combine the multiple rows of each council area in SIMD into single occurrences. For this, we can use pipes and the group_by() function.

SIMD$Council_area <- as.factor(SIMD$Council_area) # Changing the data type to a factor is best practice here, although it should still work as a character type.

Councils <- SIMD %>% 
                group_by(Council_area) %>% 
                summarize(Population=sum(Total_population))

# ========== Practical 2 =====================
# Try to add the mean SIMD2020v2_Rank as a new column in the new Councils dataframe. You will need to use group_by again.

# ========== Combining the Tidied Data ===================
authority_data$authority <- as.factor(authority_data$authority) # Make sure both shared columns are the same data type.

Merged_data <- merge(authority_data, Councils, by.x = 'authority', by.y = 'Council_area') # The merge(function) can take several arguments, the first two being the two dataframes to merge. Using by we can let the function know which column has the shared data, in this case, since they have different names, we have to us by.x and by.y.


# #### PART 4- PRACTICALS ####

# ==== Practical 3 ====
# Does the council area with the highest deprivation receive the most foodbank parcels?

# ==== Practical 4 ====
# Create a new column for foodbank parcels (in 2021 and 2022) per 1,000 population. Now check whether the council of highest deprivation has the most foodbank parcels per capita.

# ==== Practical 5 ====
# Create a new column with the average time traveled to a GP by Public Transport (SIMD$PT_GP) for each council. Add this to the merged column and check how the councils of lower and higher deprivation compare in this category.


# #### END-ish ####
# There is a lot more that is possible with R in terms of data wrangling. But now that we have an understanding of this, and some tidier, computer friendly data, we can start to check some of the observations we have made in these practicals through statistical analyses. 

# ==== This is in preparation for tomorrow ====================
# On our dataset for each variable there are two value normally 2020 and 2022 so we want to analyse the increase and decrease. For some of the techniques we are going to look tomorrow it is better if they are all in the same scale so we will transform them all in % increase/ decrease 
authority_data$FoodInsecurity <- round(((authority_data$food_insecurity2018_2022-authority_data$food_insecurity2017_2021)/authority_data$food_insecurity2017_2021)*100,2) 

authority_data$HousePrices<- round(((authority_data$house_price_jul_22-authority_data$house_price_jul_21)/authority_data$house_price_jul_21)*100,2) 
authority_data$WelfareApp<-round(((authority_data$welfare_applications2021_2022-authority_data$welfare_applications2020_2021)/authority_data$welfare_applications2020_2021)*100,2) 

authority_data$Homeless<- round(((authority_data$homelessness_applications2021_2022-authority_data$homelessness_applications2020_21)/authority_data$homelessness_applications2020_21)*100,2)

authority_data$Business<- round(((authority_data$businesses_2022-authority_data$businesses_2021)/authority_data$businesses_2021)*100,2)

authority_data$Foodbank<-round(((authority_data$foodbank_parcels_2022-authority_data$foodbank_parcels_2021)/authority_data$foodbank_parcels_2021)*100,2)

authority_data$Rent<-round(((authority_data$average_rent_2022-authority_data$average_rent_2021)/authority_data$average_rent_2021)*100,2)

authority_data_cleaned<-authority_data[,c(1:3,6,13,14,22:28)]# Nice cleaned dataset for tomorrow
# Add some variables from the SIMD Dataset 
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

# Just to make sure we have it ready tomorrow let's save it as an output
write_csv(authority_data_cleaned, "Day3/DataWrangling/outputs/authority_data_cleaned.csv")

write_csv(Merged_data, "Day3/DataWrangling/outputs/Full_Scottish_Data.csv")
