library("shiny")

overview <- tabPanel(
  "Overview",
  fluidPage(
    h2("Climate Change in the United States: An Overview"),
    p(em("AA-4: Katie Hancock, Keaton Kowal, Bowen Liu, Sebastian Liu")),
    p(
      "For our project, we were interested in addressing and understanding",
      strong("climate change in the United States"), "because this is a pressing
      issue that affects everyone across the world. Our climate significantly
      affects our lives, including our food sources, transportation, air
      quality, and overall health. We performed data analysis to determine what
      effects havemalready taken place. We selected a few questions to answer in
      this project in order to better understand the severity of the
      situation."
    ),
    p(
      "The first question we wanted to answer was", em("how many extreme weather
      days"), "the United States had over the last 50 years. Climate change
      exacerbates the frequency, intensity, and impacts of extreme weather
      events, so we thought this would be a strong indicator. We used the",
      a(
        href = "https://www.ncdc.noaa.gov/extremes/cei/graph",
        "U.S. Climate Extremes Index (CEI)"
      ),
      "data set because it lists the percent of days with extreme weather
      per year since 1910, and we can use it to observe whether conditions have
      improved or worsened over time."
    ),
    p(
      "The second question we wanted to answer was", em("how the average
      temperatures of each state has changed"), "over the past 50 years. Usually
      climate change causes the land and oceans to increase in temperature
      rather than decrease, so we looked out for trends. We used the",
      a(
        href = "https://www.ncdc.noaa.gov/cag/divisional/mapping",
        "Monthly Climate Conditions"
      ),
      "data set because it shows the monthly change in temperature for
      each state which allows us to easily identify these trends."
    ),
    p(
      "The last question we wanted to answer was", em("how the average
       precipitation of each state in the U.S. has changed"), "in the past 50
       years. As climate change increases the temperature of the air, the air
       can hold more water vapor. More moisture in the air can cause more
       intense precipitation events to occur, so identifying any trends in
       increased or decreased precipitation over the years will indicate the
       severity of climate change during that time. We used the",
      a(
        href = "https://www.ncdc.noaa.gov/cag/national/time-series",
        "Temperature and Precipitation Trends"
      ),
      "data set because it shows the average precipitation per year, which
      we can use to identify long-term trends."
    ),
    img(
      src = paste0(
        "https://pharmaphorum.com/wp-content/uploads/2020/03/How-",
        "pharma-can-adapt-to-climate-change-605x340.jpg"
      ),
      align = "center", width = "75%"
    )
  )
)

conclusion <- tabPanel(
  "Conclusions",
  fluidPage(
    h2("Conclusions"),
    h3("U.S. Temperatures Have Risen"),
    p("The Monthly Climate Conditions map provides insight into the trends
      regarding temperature in the US. By interacting with the map and observing
      the data, we can conclude that temperatures have trended upwards overtime.
      The broader implication is that climate change is already impacting our
      lives, therefore, solutions should be researched."),
    h3("Extreme Weather Days Have Increased over The Last 110 Years"),
    p("The United States Climate Extremes Index graph illustrates the overall
      trend in terms of the percentage of extreme weather days (the temperature
      was extremely high or extremely low) in each year from 1910 to 2019. It
      can be seen from the graph that the actual percentage of the
      extreme weather days has been increasing since 1965, while the normal
      weather days, which are represented by below normal percentage, keeps
      decreasing over the last 100 years. Broadly speaking, climate change has
      brought us more extreme weather days, and the percentage of abnormal
      weather days will keep increasing if we don't take actions."),
    h3("Precipitation Averages Will Increase In the Near Future"),
    p("The interactive Temperature and Precipitation graph shows the average
      precipitation of the contiguous US every year. When looking at the map and
      the data points over time, we can see that there is a wave pattern that
      goes up and down, and we are in the upswing motion right now. Combined
      with just a general increase in temperature, and you can see that our
      nation's temperature will continue to climb. The years 2014 - 2016 are all
      in the top 6 of highest average temperatures, and we will probably see
      points that surpass them. This means that temperatures are already going
      up, and people need to actively use methods to counteract this future.")
  )
)

ui <- fluidPage(
  includeCSS("style.css"),
  # Pass each page to a multi-page layout
  navbarPage(
    "Climate Change in the U.S.",
    overview,
    map_panel,
    cei_panel,
    temp_panel,
    conclusion
  )
)
