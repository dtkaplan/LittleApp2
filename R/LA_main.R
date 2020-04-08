#' Construct the Little App server function
#'
#' @export
LA_main <- function(app_specifics, explan = TRUE, covar=FALSE) {
  library(shiny)
  library(shinyWidgets)
  library(shinyjs)
  library(miniUI)
  library(ggplot2)
  library(LittleApp2)
  library(triola)
  library(mosaicData)
  library(StatsUsingTechnologyData)
  library(openintro)
  library(Lock5Data)

  options(warn = -1) # suppress  warnings so log doesn't get crowded

  res <- list()
  res$ui <- ui_main(
    ui_top_controls(),
    data_tab(explan = explan, covar = covar),
    graph_panel(),
    compare_panel(),
    stats_panel()
  )


  res$server <- function(input, output, session) {
    source(system.file("data_services.R",  package = "LittleApp2"), local = TRUE)
    source(system.file("output_services.R", package = "LittleApp2"), local = TRUE)
    source(app_specifics, local = TRUE)
  }

  res
}
