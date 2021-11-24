library("shiny")
library("dplyr")
library("ggplot2")

# This file creates the Temperature and Precipitation tab, and allows users to
# use a slider and observe data on average temperature every year.

# load in the data
temp_precipt_df <- read.csv("data/Temperature and Precipitation Trends.csv",
  stringsAsFactors = FALSE
)

# Creates the plot and shows a point that the user has chosen
temp_precipt_trend_graph <- function(temp_precipt_df, year) {
  # clean data a little
  clean_df <- temp_precipt_df %>%
    mutate(Year = trunc(Date / 100)) %>%
    group_by(Year) %>%
    summarize(
      Value = mean(Value, na.rm = TRUE),
      Anomaly = mean(Anomaly, na.rm = TRUE)
    )

  highlight_df <- clean_df %>%
    filter(Year == year)

  # make graph
  plot <- ggplot(data = clean_df) +
    geom_point(mapping = aes(x = Year, y = Value, color = Anomaly)) +
    geom_text(
      data = highlight_df,
      aes(
        x = Year, y = Value,
        label = paste0(year, Value)
      ),
      color = "darkblue",
      nudge_y = -1
    ) +
    geom_point(
      data = highlight_df,
      aes(x = Year, y = Value),
      color = "green",
      size = 3
    ) +
    scale_colour_gradient2(low = "blue", mid = "black", high = "red") +
    geom_smooth(mapping = aes(x = Year, y = Value)) +
    ggtitle("Temperature and Precipitation Trends (in October)") +
    xlab("Year") +
    ylab("Average Temperature (Degrees Farenheit)")

  return(plot)
}

temp_main <- mainPanel(
  plotlyOutput("temp_precipt_trend_graph")
)

# Creates a slider so the user can choose the year they want to observe.
temp_sidebar <- sidebarPanel(
  sliderInput(
    "year",
    "Year to observe",
    min = 1895,
    max = 2020,
    value = 2020,
    step = 1
  )
)

temp_precipt_trend_html <- "<div>
  <h2>Temperature and Precipitation Trends</h2>
  <p>On this tab, we want to answer the question: What is the trend of average
  precipitation and snowfall of each state in the United States over the last 50
  years or more? To answer this question, we have a graph of the average amount
  of precipitation for the entire nation from 1895 to 2020, and an interactive
  panel that allows the user to choose which years to focus on.
  This visualization is based on <a href=
  \"https://www.ncdc.noaa.gov/cag/national/time-series\">
  this</a> NOAA data.</p>
</div>"

# Creates the tabPanel and everything in it
temp_panel <- tabPanel(
  "Temperature and Precipitation",
  fluidPage(HTML(temp_precipt_trend_html)),
  temp_sidebar,
  temp_main
)
