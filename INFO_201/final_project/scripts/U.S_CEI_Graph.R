library("ggplot2")
library("plotly")
library("dplyr")

rm(list = ls())
# This file creates a interactive graph of the percentage of extreme whether
# days using line encoding to illustrate the over all change in the number of
# cextreme weather days in the U.S. from 1910 to 2019.
# Reads a csv file and save it to a variable called `cei_info`
cei_info <- read.csv("data/U.S. Climate Extremes Index (CEI).csv",
  stringsAsFactors = FALSE
)
cei_graph <- function(cei_info) {
  cei_info <- cei_info %>%
    mutate(mean = mean(cei_info$actual_percent, na.rm = TRUE))
  # Makes a graph using ggplot
  cei_plot <- ggplot(data = cei_info) +
    geom_smooth(mapping = aes(
      x = year,
      y = actual_percent
    )) +
    labs(
      title = "U.S. Climate Extreme Index (1910 - 2019)",
      x = "Year",
      y = "Percentage of Extreme Weather Days"
    )
  # Makes the plot interactive
  graph <- ggplotly(cei_plot)

  return(graph)
}
