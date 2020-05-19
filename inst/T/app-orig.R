#  t-test app
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(miniUI)
library(ggplot2)
library(LittleApp2)
library(triola)
library(sullystats6e)
library(mosaicData)
library(StatsUsingTechnologyData)
library(openintro)
library(Lock5Data)

options(warn = -1) # suppress  warnings so log doesn't get crowded

ui <- ui_main(
  ui_top_controls(),
  data_tab(covar = FALSE),
  graph_panel(),
  compare_panel(),
  stats_panel()
  )


server <- function(input, output, session) {


  source(system.file("data_services.R",  package = "LittleApp2"), local = TRUE)
  source(system.file("output_services.R", package = "LittleApp2"), local = TRUE)
  source("app-specific-services-t.R", local = TRUE)


  output$debug1 <- renderText({
    n_size()
    input$new_sample
    paste(capture.output(head(current_sample())), collapse = "\n")
    })
  output$debug2 <- renderText({nrow(current_sample())})




}

options(shiny.reactlog = TRUE)

shinyApp(ui, server)
