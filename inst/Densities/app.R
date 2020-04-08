library(LittleApp2)

components <- LA_main(app_specifics = "density-services.R",
                      explan = FALSE, covar = FALSE)

shinyApp(components$ui, components$server)
