library("dplyr")
library("ggplot2")
library("shiny")
library("rsconnect")
library("plotly")
data("midwest")

page_one <- tabPanel(
  "Introduction to Mid-west Data",
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      
    )
  )
  
)

page_two <- tabPanel(
  "Mid-west Data Visualization",
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      
    )
  )
  
)

page_three <- tabPanel(
  "Mid-west Data Summary",
  fluidPage(
    h2("The Graph"),
    textInput(
      inputId = "graph_title",
      lable = h3("Enter the title you want for the graph.")
    ),
    plotlyOutput(
      outputId = "the_graph",
      
    )
  )
  
)

ui <- navbarPage(
  "Mid-west Data Report",
  page_one,
  page_two,
  page_three
)