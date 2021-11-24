server <- function(input, output) {
  output$monthly_climate_cond_map <- renderPlotly({
    build_map(
      monthly_climate_cond_df,
      input$map_col,
      input$map_year
    )
  })

  output$us_cei_graph <- renderPlotly({
    cei_graph(
      cei_info,
      input$index_category
    )
  })

  output$temp_precipt_trend_graph <- renderPlotly({
    temp_precipt_trend_graph(
      temp_precipt_df,
      input$year
    )
  })
}
