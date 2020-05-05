library(LittleApp2)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(miniUI)
library(ggplot2)
library(triola)
library(sullystats6e)
library(mosaicData)
library(StatsUsingTechnologyData)
library(openintro)
library(Lock5Data)

components <- LA_main(app_specifics = "density-services.R",
                      explan = FALSE, covar = FALSE)

shinyApp(components$ui, components$server)
