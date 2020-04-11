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

components <- LA_main(app_specifics = "t-services.R",
                      covar = FALSE)

shinyApp(components$ui, components$server)
