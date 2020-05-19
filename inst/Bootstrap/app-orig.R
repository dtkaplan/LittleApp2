library(shiny)
library(shinyWidgets)
library(shinyjs)
library(miniUI)
library(ggplot2)
library(ggformula)
library(LittleApp2)
library(dplyr)
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
  stats_panel()  #, debug_panel()
  )


server <- function(input, output, session) {

  observe({
    # These are here so that all  the Little Apps can eventually be folded into
    # a single URL
    source(system.file("data_services.R", package="LittleApp2"), local = TRUE)
    source(system.file("output_services.R", package = "LittleApp2"), local = TRUE)
    source("Bootstrap_services.R", local = TRUE)

  })

}

#options(shiny.reactlog = TRUE)

shinyApp(ui, server)
