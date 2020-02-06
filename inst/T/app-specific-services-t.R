# app-specific services for t-test app

main_calculation <- reactive({
  req(ncol(current_sample()) %in% 1:2)
  modf <- model_formula()
  data <- with_dicotomous_response()
  yvals <- raw_data()[[input$response]]

  yrange  <- range(as.numeric(yvals), na.rm = TRUE)


  t_test_calcs(modf, data,
               level = Common$conf_level,
               show_mean = Common$show_mean,
               show_ci = Common$show_ci,
               show_t = Common$show_t,
               var_equal = Common$var_equal,
               y_range = NULL)
})

model_formula <- reactive({
  req(current_sample())
  vars <-  names(current_sample())
  get_model_formula(current_sample(), 1, FALSE, "lm")
})

# For  every input  widget in  the  models, add a corresponding
# component to Common in  the server function and the appropriate
# reactive to set  the Common  element each time the input  widget
# changes value.
# The annotation modal not used in this app

Common <- reactiveValues(
  show_mean = FALSE,
  show_ci = FALSE,
  show_t = FALSE,
  var_equal = FALSE,
  conf_level = 0.95
)

observeEvent(input$show_annotations, {
  showModal(
    modalDialog(
      title = "Statistical Annotations", easyClose  = TRUE,
      checkboxInput("show_mean", "Show mean",  Common$show_mean),
      checkboxInput("show_ci", "Show conf interval", Common$show_ci),
      checkboxInput("show_t", "Show t-interval",  Common$show_t),
      checkboxInput("var_equal", "Assume equal variances", Common$var_equal),
      selectInput("conf_level", "Confidence level",
                  choices  = c(0.5, 0.8, 0.9, 0.95, 0.99, 0.999),
                  selected = Common$conf_level),
      size = "s"
    )
  )
})

#  Store the modal inputs in the Common area
observeEvent(input$show_mean, {
  Common$show_mean <-  input$show_mean
})
observeEvent(input$show_ci, {
  Common$show_ci <-  input$show_ci
})
observeEvent(input$show_t, {
  Common$show_t <-  input$show_t
})
observeEvent(input$show_var_equal, {
  Common$var_equal <-  input$var_equal
})
observeEvent(input$conf_level, {
  Common$conf_level <-  as.numeric(input$conf_level)
})

format_t_stats <- function(stats) {
  #HTML(p("Just a constant for now."))
  browser()
  "Really?"
}

current_stats  <- reactive({
  req(main_calculation())

  format_t_stats(main_calculation()$stats)
})
frozen_stats <- reactive({
  format_t_stats(frozen_calculation()$stats)
})
