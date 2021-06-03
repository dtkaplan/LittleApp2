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

components <- LA_main(app_specifics = "regression_services.R",
                      covar = TRUE)

shinyApp(components$ui, components$server)
