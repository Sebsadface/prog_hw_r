library("plotly")
library("leaflet")
library("ggplot2")
library("dplyr")
library("pscl")
rm(list = ls())


shooting_info <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

number_of_shootings <- nrow(shooting_info)
lives_lost <- sum(shooting_info$num_killed, na.rm = TRUE)
ppl_injured <- sum(shooting_info$num_injured, na.rm = TRUE)

most_impacted_city <- shooting_info %>%
  mutate(impact = num_killed + num_injured) %>%
  filter(impact == max(impact, na.rm = FALSE)) %>%
  pull(city)

worst_incident <- shooting_info %>%
  mutate(total = num_killed + num_injured) %>%
  filter(total == max(total, na.rm = FALSE))

most_impacted_state <- shooting_info %>%
  mutate(impact = num_killed + num_injured) %>%
  group_by(state) %>%
  summarise(total = sum(impact)) %>%
  filter(total == max(total, na.rm = TRUE)) %>%
  pull(state)

most_impacted_date <- shooting_info %>%
  mutate(impact = num_killed + num_injured) %>%
  group_by(date) %>%
  summarise(total = sum(impact)) %>%
  filter(total == max(total, na.rm = TRUE)) %>%
  pull(date)

state_info <- shooting_info %>%
  mutate(impact = num_killed + num_injured) %>%
  group_by(state) %>%
  summarise(
    deaths = sum(num_killed, na.rm = TRUE),
    injured = sum(num_injured, na.rm = TRUE)
  ) %>%
  mutate(total = deaths + injured) %>%
  filter(total >= 20) %>%
  arrange(-total)

info_by_states <- shooting_info %>%
  mutate(impact = num_killed + num_injured) %>%
  group_by(state) %>%
  summarise(
    deaths = sum(num_killed, na.rm = TRUE),
    injured = sum(num_injured, na.rm = TRUE)
  ) %>%
  mutate(casualties = deaths + injured) %>%
  filter(casualties > 60) %>%
  arrange(-casualties)

state_plot <- ggplot(data = info_by_states) +
  geom_point(mapping = aes(x = state, y = casualties))
