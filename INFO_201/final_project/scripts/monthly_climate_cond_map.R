library(leaflet)
library(dplyr)
library(stringr)
library(tigris) # install this to cook the rmd!

states <- states(cb = TRUE)

# this function creates a leaflet map of the US to show different states
# changes in temperature from 1970 to 2020

# It calculates the mean temp in 1970 and 2020 for each state, then finds
# how much the mean temp has changed. It then maps that difference.
# Note - it takes a dataframe (of the relevant csv) as a parameter!
# (I referenced andrewbtran.github.io/NICAR/2017/maps/leaflet-r.html for making
# my choropleth map, since its a bit different from just plotting points!)

monthly_climate_cond_map <- function() {
  monthly_climate_conditions <- read.csv("data/Monthly Climate Conditions.csv",
    stringsAsFactors = FALSE
  )

  state_temp_change <- monthly_climate_conditions %>%
    # change location format to just be state
    mutate(Location = sub(" CD (.*)", "", Location)) %>%
    group_by(Location) %>%
    summarize(temp = Value, Date = Date) %>%
    # combine years, then find average temps in 1970 and 2020
    mutate(Date = trunc(Date / 100)) %>%
    filter(Date == 1970 | Date == 2020) %>%
    group_by(Location, Date) %>%
    summarize(temp = mean(temp)) %>%
    # find change in average temp between 1970 and 2020
    group_by(Location) %>%
    summarize(temp_change = max(temp) - min(temp))

  # make data legible for leaflet
  state_temp_change_geo <- geo_join(
    states,
    state_temp_change,
    "NAME",
    "Location"
  ) %>%
    filter(!is.na(temp_change)) %>%
    mutate(label = paste0(
      "Temperature Change: ",
      round(temp_change, 2),
      " degrees F"
    ))
  pal <- colorNumeric("Reds", domain = state_temp_change$temp_change)

  # create map
  map <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      data = state_temp_change_geo,
      fillColor = ~ pal(temp_change),
      fillOpacity = 1,
      color = "black",
      weight = 0.5,
      popup = ~label
    ) %>%
    addLegend(
      pal = pal,
      values = state_temp_change_geo$temp_change,
      title = "Average Temperature Change (from 1970 to 2020, in degrees F)",
      position = "bottomleft"
    )
  return(map)
}
