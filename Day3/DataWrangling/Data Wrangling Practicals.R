# #### Practical Solutions ####

# ==== 1 ====
# Create a new column in authority_data that shows the difference between foodbank_parcels in 2021 and 2022, and check whether there was an overall increase or decrease. Use authority_data$Foodbank_difference <- to assign a new column. You will need to tidy the data in the 2021 column
authority_data$Foodbank_difference <- authority_data$foodbank_parcels_2022-authority_data$foodbank_parcels_2021 # Calculate the difference between the two years.

(sum(authority_data$foodbank_parcels_2022, na.rm = T)-sum(authority_data$foodbank_parcels_2021, na.rm = T)) # Calulate overall difference between the two years. There were 27582 less foodbank parcels in 2022 compared to 2021.

# ==== 2 ====
# Try to add the mean SIMD2020v2_Rank as a new column in the new Councils dataframe. You will need to use group_by again.

Council_SIMD <-   SIMD %>% 
  group_by(Council_area) %>% 
  summarize(SIMD=mean(SIMD2020v2_Rank)) # Create a new object with mean value for each councils SIMD rank.

Councils$SIMD <- Council_SIMD$SIMD # Add the SIMD rank to the Councils object as a new column.

# ==== 3 ====
# Does the council area with the highest deprivation receive the most foodbank parcels?
# Sort the SIMD column to see the lowest value (a low rank indicates high deprivation), and check this against the highest foodbank parcels row. Glasgow City has the highest levels of deprivation, and the most foodbank parcels in both 2021 and 2022.

# ==== 4 ====
# Create a new column for foodbank parcels (in 2021 and 2022) per 1,000 population. Now check whether the council of highest deprivation has the most foodbank parcels per capita.

Merged_data$Foobank_pc_2021 <- (Merged_data$foodbank_parcels_2021/(Merged_data$Population/1000))

Merged_data$Foodbank_pc_2022 <- (Merged_data$foodbank_parcels_2022/(Merged_data$Population/1000))

# Comparing these, we can see that Dundee City, not Glasgow city, actually has the most foodbank parcels per capita.

# ==== 5 ====

# Create a new column with the average time traveled to a GP by Public Transport (SIMD$PT_GP) for each council. Add this to the merged column and check how the councils of lower and higher deprivation compare in this category.

Council_PT_GP <-   SIMD %>% 
  group_by(Council_area) %>% 
  summarize(PT_GP=mean(PT_GP))

Merged_data$PT_GP <- Council_PT_GP$PT_GP

# The more deprived councils don't necessarily seem to have longer journey time by public transport to a GP.


