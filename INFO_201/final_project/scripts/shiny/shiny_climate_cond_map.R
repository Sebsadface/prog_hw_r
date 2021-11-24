library(plotly)
library(stringr)

# this function formats the relevant data
format_df <- function(df) {
  df <- df %>%
    # rename and add state abbreveiations
    mutate(Location = sub(" CD (.*)", "", Location)) %>%
    mutate(code = setNames(state.abb, state.name)[Location]) %>%
    # format dates to be by year
    mutate(Date = trunc(Date / 100)) %>%
    # set up the columns, grouping yearly stats
    group_by(code, Date) %>%
    summarize(
      Temperature = mean(Value, na.rm = TRUE),
      Rank = mean(Rank, na.rm = TRUE),
      Anomaly = mean(Anomaly..1901.2000.base.period., na.rm = TRUE),
      Mean = mean(X1901.2000.Mean, na.rm = TRUE)
    )
  return(df)
}

# reads in the data
monthly_climate_cond_df <- read.csv(
  "data/Monthly Climate Conditions.csv",
  stringsAsFactors = FALSE
) %>%
  format_df()

# this list calculates the total min and max for various attributes, so that
# when the color key is made, it doesn't change scale when the year's min
# and max do, allowing for more clear presentation!
limits_list <- list(
  Temperature = c(
    min(monthly_climate_cond_df$Temperature, na.rm = TRUE),
    max(monthly_climate_cond_df$Temperature, na.rm = TRUE)
  ),
  Rank = c(
    min(monthly_climate_cond_df$Rank, na.rm = TRUE),
    max(monthly_climate_cond_df$Rank, na.rm = TRUE)
  ),
  Anomaly = c(
    min(monthly_climate_cond_df$Anomaly, na.rm = TRUE),
    max(monthly_climate_cond_df$Anomaly, na.rm = TRUE)
  ),
  Mean = c(
    min(monthly_climate_cond_df$Mean, na.rm = TRUE),
    max(monthly_climate_cond_df$Mean, na.rm = TRUE)
  )
)

# this function is used in the server to build the map based on selected data
# takes chr for col and an int for year
# returns a plotly map
build_map <- function(df, col, year) {
  # filter and set up data to be used
  df <- df %>%
    filter(Date == year) %>%
    select(col) %>%
    rename(sel_col = col)

  # build map
  # references: https://plot.ly/r/choropleth-maps/

  # define lists for outline arguments and map projection
  l <- list(color = toRGB("white"), width = 2)
  g <- list(
    scope = "usa",
    projection = list(type = "albers usa")
  )

  # creates the map
  p <- plot_geo(df, locationmode = "USA-states") %>%
    add_trace(
      z = ~sel_col,
      locations = ~code,
      color = ~sel_col,
      colors = "Reds",
      marker = list(line = l)
    ) %>%
    colorbar(title = col, limits = limits_list[[col]]) %>%
    layout(
      title = paste0("State ", col, " in October, ", year),
      geo = g
    )

  return(p)
}

# side panel to select data
map_sidebar_content <- sidebarPanel(
  selectInput(
    "map_col",
    "Map Data",
    c("Temperature", "Anomaly", "Mean")
  ),
  sliderInput(
    "map_year",
    "Year",
    min = 1895,
    max = 2020,
    value = 2000,
    step = 1
  )
)

climate_cond_map_html <- "<div id=\"tab_desc\">
  <h2>Climate Conditions Map</h2>
  <p>This page revolves around the question: how have the average temperatures
  of each state changed over the last 50+ years? We've chosen to answer this
  question by providing an interactive chloropleth map that allows the user to
  investigate state temperatures, their divisional temperature anomaly, and
  their divisional average temperature 1901-2000 mean in October for the years
  1895 to 2020. This visualization is based on <a href=
  \"https://www.ncdc.noaa.gov/cag/divisional/mapping/110/tavg/202010/1/mean\">
  this</a> NOAA data.</p>
</div>"

# main panel to hold map
map_main_content <- mainPanel(
  plotlyOutput("monthly_climate_cond_map")
)

# combined panels into tab, used in UI
map_panel <- tabPanel(
  "Monthly Climate Conditions",
  fluidPage(HTML(climate_cond_map_html)),
  map_sidebar_content,
  map_main_content
)
