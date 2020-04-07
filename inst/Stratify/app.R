library(LittleApp2)

components <- LA_main(app_specifics = "Stratify_services.R",
                      covar = TRUE)

shinyApp(components$ui, components$server)
