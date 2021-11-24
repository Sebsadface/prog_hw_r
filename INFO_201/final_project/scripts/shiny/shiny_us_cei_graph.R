library("ggplot2")
library("plotly")
library("dplyr")
library("stringr")

# reads in the data
cei_info <- read.csv(
  "data/U.S. Climate Extremes Index (CEI).csv",
  stringsAsFactors = FALSE
)

# this function is used in the server to build the graph based on selected data
cei_graph <- function(cei_info, input) {
  # Makes a graph using ggplot
  if (input == "Actual Percentage") {
    cei_plot <- ggplot(data = cei_info) +
      geom_smooth(
        mapping = aes(
          x = year,
          y = actual_percent
        ),
        method = "loess",
        formula = "y ~ x",
        color = "blue"
      ) +
      labs(
        title = "U.S. Climate Extreme Index (1910 - 2019)",
        x = "Year",
        y = "Percentage of Extreme Weather Days"
      )
    # Makes the plot interactive
    graph <- ggplotly(cei_plot)

    return(graph)
  } else if (input == "Below Normal Percentage") {
    cei_plot <- ggplot(data = cei_info) +
      geom_smooth(
        mapping = aes(
          x = year,
          y = below_normal
        ),
        method = "loess",
        formula = "y ~ x",
        color = "green"
      ) +
      labs(
        title = "U.S. Climate Extreme Index (1910 - 2019)",
        x = "Year",
        y = "Percentage of Extreme Weather Days Below Normal"
      )
    # Makes the plot interactive
    graph <- ggplotly(cei_plot)

    return(graph)
  } else {
    cei_plot <- ggplot(data = cei_info) +
      geom_smooth(
        mapping = aes(
          x = year,
          y = above_normal
        ),
        method = "loess",
        formula = "y ~ x",
        color = "red"
      ) +
      labs(
        title = "U.S. Climate Extreme Index (1910 - 2019)",
        x = "Year",
        y = "Percentage of Extreme Weather Days Above Normal"
      )
    # Makes the plot interactive
    graph <- ggplotly(cei_plot)

    return(graph)
  }
}


# sidebar to select index category
cei_sidebar <- sidebarPanel(
  radioButtons(
    "index_category",
    "Index Category",
    choices = (c(
      "Above Normal Percentage",
      "Below Normal Percentage",
      "Actual Percentage"
    )),
    selected = "Above Normal Percentage"
  )
)

us_cei_graph_html <- "<div>
  <h2>U.S. Climate Extremes Index (CEI)</h2>
  <p>This page is dedicated on answering the question:
  how have the number of days of extreme weather changed over the past 50 or
  more years, particularly in the United States? We've chosen to answer this
  question by providing an interactive line graph that allows the user to see
  the overall trend of the unnormal number of extreme whhether days form 1910 to
  2019. This visualization is based on <a href=
  \"https://www.ncdc.noaa.gov/extremes/cei/graph\">this</a> NOAA data.</p>
</div>"

# main panel to hold map
cei_main <- mainPanel(
  plotlyOutput("us_cei_graph")
)

# combined panels into tab, used in UI
cei_panel <- tabPanel(
  "Climate Extreme Index",
  fluidPage(HTML(us_cei_graph_html)),
  cei_sidebar,
  cei_main
)
