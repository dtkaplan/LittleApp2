#  t-test app
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(miniUI)
library(ggplot2)
library(LittleApp2)

# Documentation for the app
explain_text <- paste(
  readLines("www/explain-t.html"),
  collapse = "\n"
)

ui <- ui_main(
  ui_top_controls(),
  ui_explain_tab("F", "info-circle"),
  data_tab(),
  graph_panel(),
  compare_panel(),
  stats_panel(),
  codebook_panel(),
  debug_panel()
  )


server <- function(input, output, session) {


  source(system.file("data_services.R",  package = "LittleApp2"), local = TRUE)
  source(system.file("output_services.R", package = "LittleApp2"), local = TRUE)
  source("app-specific-services-t.R", local = TRUE)

  output$explain_text <- renderText({HTML(explain_text)})




  output$debug1 <- renderText({
    input$sample_size
    input$new_sample
    paste(capture.output(head(current_sample())), collapse = "\n")
    })
  output$debug2 <- renderText({nrow(current_sample())})




}

options(shiny.reactlog = TRUE)

shinyApp(ui, server)
