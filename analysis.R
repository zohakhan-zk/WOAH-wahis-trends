library(tidyverse)

# Load data
wahis <- read_csv("data/wahis_raw.csv")
glimpse(wahis)

# Add present/absent flag
wahis <- wahis %>%
  mutate(is_present = `Disease status` == "Present")

# Overall summary by country and disease
summary_table <- wahis %>%
  group_by(Country, Disease) %>%
  summarise(
    total_reports = n(),
    present_count = sum(is_present),
    presence_rate = round(present_count / total_reports, 2)
  )

summary_table

# Plot: overall presence rate by country
ggplot(summary_table, aes(x = Country, y = presence_rate, fill = Disease)) +
  geom_col(position = "dodge") +
  labs(
    title = "Disease Presence Rate by Country (2005-2025)",
    y = "Proportion of semesters with disease present",
    x = "Country"
  ) +
  theme_minimal()

# Yearly summary by country, disease, and year
yearly_summary <- wahis %>%
  group_by(Country, Disease, Year) %>%
  summarise(
    total_reports = n(),
    present_count = sum(is_present),
    presence_rate = round(present_count / total_reports, 2)
  )

yearly_summary

# Plot: presence rate over time, faceted by disease
ggplot(yearly_summary, aes(x = Year, y = presence_rate, color = Country)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ Disease, ncol = 1) +
  labs(
    title = "Disease Presence Rate Over Time by Country",
    y = "Proportion of semesters with disease present",
    x = "Year"
  ) +
  theme_minimal()
