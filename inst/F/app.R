library(shiny)
library(shinyWidgets)
library(shinyjs)
library(miniUI)
library(ggplot2)
library(ggformula)
library(LittleApp2)
library(dplyr)
library(triola)
library(mosaicData)
library(MAT160)
library(openintro)
library(Lock5Data)

options(warn = -1) # suppress  warnings so log doesn't get crowded



ui <- ui_main(
  ui_top_controls(),
  data_tab(),
  graph_panel(),
  compare_panel(),
  stats_panel()  #, debug_panel()
  )


server <- function(input, output, session) {

  observe({
    # These are here so that all  the Little Apps can eventually be folded into
    # a single URL
    source(system.file("data_services.R", package="LittleApp2"), local = TRUE)
    source(system.file("output_services.R", package = "LittleApp2"), local = TRUE)
    source("app_specific_services.R", local = TRUE)

  })

  output$debug1 <- renderText({
    n_size()
    input$new_sample
    paste(capture.output(head(current_sample())), collapse = "\n")
    })
  output$debug2 <- renderText({nrow(current_sample())})

}

#options(shiny.reactlog = TRUE)

shinyApp(ui, server)
