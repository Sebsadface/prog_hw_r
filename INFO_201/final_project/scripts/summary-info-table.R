library("dplyr")
library("stringr")

# This function creates a summary information table of the Monthly Climate
# Conditions data to show the changes in temperature for each state over a
# course of 50 years, from 1970 to 2020.
# From this summary table, temperatures for every single state have risen by
# about 4-7 degrees within that time frame, so we can conclude that global
# warming has already started to impact the United States.
summary_table <- function(data) {
  # Loading the data in
  monthly_climate_conditions <- read.csv("data/Monthly Climate Conditions.csv",
    stringsAsFactors = FALSE
  )

  # Grouping the data into a summary table with date, state, and temperature
  # columns
  mcc_summary <- monthly_climate_conditions %>%
    group_by("date" = substr(Date, 1, 4), state = word(Location, 1,
      sep = "CD"
    )) %>%
    summarize(temperature = mean(Value)) %>%
    filter(date == 1970 || date == 2020) %>%
    distinct()

  # Rearranging the data by state
  mcc_summary <- mcc_summary %>%
    group_by(date) %>%
    arrange(state)

  # Creating a new data frame with the states and change in temperature
  mcc_new <- mcc_summary %>%
    group_by(state) %>%
    summarize(change = temperature - lag(temperature)) %>%
    na.omit()

  return(mcc_new)
}
