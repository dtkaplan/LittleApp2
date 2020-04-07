library(LittleApp2)

components <- LA_main(app_specifics = "Center_spread_services.R",
                      covar = FALSE)

shinyApp(components$ui, components$server)
