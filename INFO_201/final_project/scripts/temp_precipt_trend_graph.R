library(dplyr)
library(ggplot2)

# This function takes the Temperature and Precipitation Trends.csv data to make
# a plot of average temps since 1970

# reads data in
temp_precip_df <- read.csv("data/Temperature and Precipitation Trends.csv",
  stringsAsFactors = FALSE
)

temp_precipt_trend_graph <- function(data_frame) {
  # clean data a little
  clean_df <- data_frame %>%
    filter(Date > 197001) %>%
    mutate(Year = trunc(Date / 100)) %>%
    group_by(Year) %>%
    summarize(
      Value = mean(Value, na.rm = TRUE),
      Anomaly = mean(Anomaly, na.rm = TRUE)
    )

  # make graph
  plot <- ggplot(data = clean_df) +
    geom_point(mapping = aes(x = Year, y = Value, color = Anomaly)) +
    scale_colour_gradient2(low = "blue", mid = "black", high = "red") +
    geom_smooth(mapping = aes(x = Year, y = Value)) +
    ggtitle("Temperature and Precipitation Trends (in October)") +
    ylab("Average Temperature (Degrees Farenheit)")

  return(plot)
}

temp_precipt_graph <- temp_precipt_trend_graph(temp_precip_df)
