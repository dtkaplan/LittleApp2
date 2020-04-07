library(LittleApp2)

components <- LA_main(app_specifics = "t-services.R",
                      covar = FALSE)

shinyApp(components$ui, components$server)
