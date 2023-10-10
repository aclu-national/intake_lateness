library(tidyverse)
library(janitor)
library(ggplot2)

# Creating dataframe 
df <- read_csv("intake_data.csv") %>%
  clean_names() %>%
  mutate(
    date_entered = mdy(date_intake_entered),
    date_incident = mdy(date_of_incident),
    date_entered_one = date_entered %m+% months(1),
    date_entered_two = date_entered %m+% months(2),
    date_entered_three = date_entered %m+% months(3),
    date_entered_four = date_entered %m+% months(4),
    date_entered_five = date_entered %m+% months(5),
    date_entered_six = date_entered %m+% months(6),
    difference = date_entered - date_incident,
    difference_one = date_entered_one - date_incident,
    difference_two = date_entered_two - date_incident,
    difference_three = date_entered_three - date_incident,
    difference_four = date_entered_four - date_incident,
    difference_five = date_entered_five - date_incident,
    difference_six = date_entered_six - date_incident
  ) %>%
  filter(difference >= 0)

# Number and percent of people in each lateness
number_people_late <- df %>%
  summarize(
    past_sol = sum(difference > 365),
    past_sol_one = sum(difference_one > 365),
    past_sol_two = sum(difference_two > 365),
    past_sol_three = sum(difference_three > 365),
    past_sol_four = sum(difference_four > 365),
    past_sol_five = sum(difference_five > 365),
    past_sol_six = sum(difference_six > 365),
    total_rows = n()
  ) %>%
  pivot_longer(cols = -total_rows, names_to = "category", values_to = "value") %>%
  mutate(percent = value/total_rows) %>%
  select(-total_rows)

# Key statistics
stats_late <- df %>%
  summarize(
    mean = mean(difference),
    median = median(difference),
    min = min(difference),
    max = max(difference),
    percentile_20 = quantile(difference, probs = 0.20),
    percentile_40 = quantile(difference, probs = 0.40),
    percentile_60 = quantile(difference, probs = 0.60),
    percentile_80 = quantile(difference, probs = 0.80)
  )

# Difference histogram
ggplot(df, aes(x = difference)) +
  geom_histogram(binwidth = 365, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Difference",
    x = "Difference",
    y = "Frequency"
  )

# Difference histogram without 90% percentile
ggplot(df %>% filter(difference <= quantile(df$difference, probs = 0.90)), aes(x = difference)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Difference",
    x = "Difference",
    y = "Frequency"
  )
