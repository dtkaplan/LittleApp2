library(shiny)
library(dplyr)
library(LittleApp2)

ui <- fluidPage(
  sliderInput("one",  "N pts", min = 1, max = 100, value = 50),
  plotOutput("main")
)

server <- function(input, output, session) {
  output$main <- renderPlot({
    plot(rnorm(input$one))
  })
}

shinyApp(ui, server)
