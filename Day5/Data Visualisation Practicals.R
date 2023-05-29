# #### Visualisation Practicals ####
# ==== Practical 1 ====
# Try to visualise any correlation between average gas consumption and energy bills. Compare the most and least deprived areas as well as the urban/rural divide.

ggplot(Scot_data, aes(x = average_household_gas_consumption_kwh_2021, y = average_energy_bill_2021)) + geom_point(aes(colour = Deprivation, shape = location), size = 3) # Interestingly, there does not seem to be a strong correlation. The more deprived areas generally have lower consumption, but not necessarily lower bills.

# ==== Practical 2 ====
# Visualise the difference in journey times using public transport to a GP. Look at regional differences.

ggplot(Scot_data, aes(x = PT_GP)) + geom_histogram(aes(fill = region), colour = "black", bins = 35) # Journey times are relatively normally distributed, but the highlands and islands clearly have much longer journey times.


# ==== Practical 3 ====
# Expand on the above, but divide by rural and urban.
ggplot(Scot_data, aes(x = PT_GP)) + geom_histogram(aes(fill = region), colour = "black", bins = 15) + facet_wrap(~ location) # Urban authorities generally have lower journey times, but the higher journey times for rural is definitely mainly influenced by long journey times in the Highlands and Islands region.


# ==== Practical 4 ====
# Try and visualise some of the factors we have looked at earlier in the day, but in a spatial format.
spplot(Scot, zcol = 'Alcohol')

Scot$Deprivatio <- as.factor(Scot$Deprivatio)
spplot(Scot, zcol = 'Deprivatio')

spplot(Scot, zcol = 'life_expec')