#Create a new column that will have Month and year 
uk_data_clean$MonthYear <- format(as.Date(uk_data_clean$dates, format="%Y-%m-%d"),"%Y-%m")

# Split the original data frame by unique values of "Category"
split_dfs <- split(uk_data_clean, uk_data_clean$MonthYear)

# Create separate objects for each data frame using unique values as object names
names(split_dfs) <- paste0("Split_", names(split_dfs))
list2env(split_dfs, envir = .GlobalEnv)


install.packages("tidytext")
library(tidytext)
library(dplyr)

articles_df <- uk_data_clean %>%
  unnest_tokens(word, clean_text) %>%
  anti_join(stop_words) %>%
  count(MonthYear, word, sort = TRUE) %>%
  as.data.frame()  # Convert to a data frame

top_keywords_by_month <- articles_df %>%
  group_by(MonthYear) %>%
  top_n(10) 
library(gganimate)
library(gifski)
library(av)
# Make a ggplot, but add frame=year: one image per year
ggplot(top_keywords_by_month, aes(x=word, y=n, fill=word)) +
  geom_col() +
  theme_minimal() +
  #geom_text(aes(label = word, hjust=0))+
  labs(x = "Month", y = "Count", fill = "Keyword") +
  scale_fill_viridis_d(option = "plasma", direction = -1)+ 
  theme(legend.position = "none")  +
  # gganimate specific bits:
  transition_states(
    MonthYear,
    transition_length = 30,
    state_length = 30
  ) +
  ease_aes('linear')+
  labs(title = 'Month-Year : {closest_state}')
