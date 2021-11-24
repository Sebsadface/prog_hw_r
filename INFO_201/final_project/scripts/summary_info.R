library("dplyr")
library("lintr")

# reading in the data set
cei_df <- read.csv("data/U.S. Climate Extremes Index (CEI).csv",
  stringsAsFactors = FALSE
)

# summary function for cei
get_summary_info_cei <- function(cei_df) {
  summary <- list()

  # Number of years the data accounts for
  summary$years <- nrow(cei_df)

  # The year with the highest percentage of days above normal
  summary$max_high_year <- cei_df %>%
    filter(above_normal == max(above_normal)) %>%
    pull(year)

  # The percentage of the year above
  summary$max_above_norm <- cei_df %>%
    filter(above_normal == max(above_normal)) %>%
    pull(above_normal)

  # The year with the highest percentage of days below normal
  summary$max_low_year <- cei_df %>%
    filter(below_normal == max(below_normal)) %>%
    pull(year)

  # The average actual percentage of extreme weather days
  summary$avg_actual_percent <- cei_df %>%
    summarize(average = mean(actual_percent, na.rm = TRUE)) %>%
    pull(average)
  return(summary)
}

cei_summary <- get_summary_info_cei(cei_df)
