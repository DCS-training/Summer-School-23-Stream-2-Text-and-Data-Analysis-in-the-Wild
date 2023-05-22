
# #### PART 1: IMPORTING AND MANIPULATING DATA ####

# ==== Importing Data ====
# Packages needed
library(tidyverse)
# In order to work with your own data, you will need to import it into R

read_csv("Day3/DataWrangling/data/AuthorityData.csv") # The read_csv() function in 'tidyverse' is the easiest way to do this

SSch_data <- read_csv("Day3/DataWrangling/data/AuthorityData.csv") # Rather than just seeing a single output, you can assign the data to a variable

View(SSch_data) # To view the dataframe, or click on the object in the global environment

SIMD <- read_csv("Day3/DataWrangling/data/simd2020_withinds.csv")

# ==== Subsetting Data ====

# ---- Selecting Data ----

SSch_data[,3] # Select column
SSch_data[5,] # Select row
SSch_data[7,1] # Select cell

SSch_data[,'life_expectancy_2022'] 
SSch_data$region # The name of columns can also be used

as_tibble(SSch_data$life_expectancy_2022) # Viewing as a table makes it easier to visualise, though removing as_tibble() will still show the data

# ---- Extracting Data ----

Row_7 <- SSch_data[7,]

Clacks_SIMD <- subset(SIMD, SIMD$Council_area =='Clackmannanshire')

High_dep <- subset(SIMD, SIMD$SIMD2020v2_Rank <= 50)

# Change data class

class(High_dep$Council_area)
High_dep$Council_area <- as_factor(High_dep$Council_area)
is.factor(High_dep$Council_area)

# ==== Tidying Data ====

# We have some NULL and N/A values in our data frames. This is a common occurrence and it is important to have all of these be consitent for tidy data. Generally 'NA' is the best option, as these values can be filtered out where needed.

# For example if we want to sum all of the foodbank parcels across all councils in 2022.
sum(SSch_data$foodbank_parcels_2022)

# We get an error as the data type is 'character', which can't be summed, only numeric.

# Trying to set this to numeric, we get another error, because some of the rows in the column contain the characters 'NULL' or a comma.

# We can remove specific characters, such as commas, using gsub.
SSch_data$foodbank_parcels_2022 <- gsub(",","",SSch_data$foodbank_parcels_2022)

# We can then subset the 'NULL's and change them to a more appropriate value.
SSch_data$foodbank_parcels_2022[SSch_data$foodbank_parcels_2022 == "NULL"] # This subsets the data to only include cells where foodbank_parcels_2022 == 'NULL'. We can then change this subseted data to 'NA'.
SSch_data$foodbank_parcels_2022[SSch_data$foodbank_parcels_2022 == "NULL"] <- NA

SSch_data[SSch_data == "NULL"] <- NA # We can easily do this for the whole dataset.

SSch_data[SSch_data == "n/a"] <- NA # And for any other variations we want to change to 'NA'.

# We should now have tidier null data values in our data.

SSch_data$foodbank_parcels_2022 <- as.numeric(SSch_data$foodbank_parcels_2022) # We can now change the data to be numeric.

sum(SSch_data$foodbank_parcels_2022, na.rm = T) # And use the na.rm argument to remove NA values from our sum calculation, giving us the total known foodbank parcels in 2022. It can also be appropriate in some cases to change null values to 0 etc., in which case na.rm would not be necessary. 

# ==== Practical 1 ====
# Create a new column in SSch_data that shows the difference between foodbank_parcels in 2021 and 2022, and check whether there was an overall increase or decrease. Use SSch$Foodbank_difference <- to assign a new column. You will need to tidy the data in the 2021 column


# ==== Combining Datasets ====
# We can combine datsets that have shared data using the merge() function. Looking at the SSch_data and SIMD dataframes, the authority and Council_area columns have the same data. 

# First we will need to combine the multiple rows of each council area in SIMD into single occurrences. For this, we can use pipes and the group_by() function.

SIMD$Council_area <- as.factor(SIMD$Council_area) # Changing the data type to a factor is best practice here, although it should still work as a character type.

Councils <- SIMD %>% 
                group_by(Council_area) %>% 
                summarize(Population=sum(Total_population))

# ---- Practical 2 ----
# Try to add the mean SIMD2020v2_Rank as a new column in the new Councils dataframe. You will need to use group_by again.

# ---- Combining the Tidied Data ----
SSch_data$authority <- as.factor(SSch_data$authority) # Make sure both shared columns are the same data type.

Merged_data <- merge(SSch_data, Councils, by.x = 'authority', by.y = 'Council_area') # The merge(function) can take several arguments, the first two being the two dataframes to merge. Using by we can let the function know which column has the shared data, in this case, since they have different names, we have to us by.x and by.y.

# #### PART 4- PRACTICALS ####

# ==== Practical 3 ====
# Does the council area with the highest deprivation receive the most foodbank parcels?

# ==== Practical 4 ====
# Create a new column for foodbank parcels (in 2021 and 2022) per 1,000 population. Now check whether the council of highest deprivation has the most foodbank parcels per capita.

# ==== Practical 5 ====
# Create a new column with the average time traveled to a GP by Public Transport (SIMD$PT_GP) for each council. Add this to the merged column and check how the councils of lower and higher deprivation compare in this category.

# #### END ####
# There is a lot more that is possible with R in terms of data wrangling. But now that we have an understanding of this, and some tidier, computer friendly data, we can start to check some of the observations we have made in these practicals through statistical analyses. 
