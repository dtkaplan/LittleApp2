library(LittleApp2)

components <- LA_main(app_specifics = "Bootstrap_services.R",
                      covar = FALSE)

shinyApp(components$ui, components$server)
