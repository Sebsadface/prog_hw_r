library(shiny)
library(dplyr)

# run page scripts
# this/these call(s) runs the script that defines the UI and server variables
# and functions used to create the interactive tab(s)
source("scripts/shiny/shiny_climate_cond_map.R")
source("scripts/shiny/shiny_us_cei_graph.R")
source("scripts/shiny/shiny_temp_precipt_trend.R")

# source ui and server
source("scripts/shiny/app_ui.R")
source("scripts/shiny/app_server.R")

shinyApp(ui = ui, server = server)
