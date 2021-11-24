library("dplyr")
library("ggplot2")
library("shiny")
library("rsconnect")
library("plotly")

source("app_ui")
source("app_server")

shinyApp(ui = ui, server = server)

rsconnect::setAccountInfo(name='sebsadface', 
                          token='67000287C1DF4977DB1B7629BAF19E82', 
                          secret='b9a681nufFo2YgWLTzeyf6LVUrojr59w7ysuePVu')
