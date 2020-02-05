# Contains definitions of control modals, that is,  pop ups

# The `main_calculation` reactive carries out the necessary calculations
#  and returns them  in  a list.
# THIS IS THE COMPUTATIONAL CORE OF THE APP
main_calculation <- reactive({
  req(ncol(current_sample()) > 1)
  # These two assignments are to avoid lazy evaluation of the reactives
  # when calling F_app_plot()
  modf <- model_formula()
  data <- with_dicotomous_response()

  yrange  <- range(raw_data()[[input$response]], na.rm = TRUE)

  F_app_plot(modf, data = data, yrange = yrange )
})



model_formula <- reactive({
  req(current_sample())
  vars <-  names(current_sample())
  req(length(vars) >= 2)
  get_model_formula(current_sample(), Common$model_order,
                    Common$interaction_term,
                    Common$model_type)
})



# For  every input  widget in  the  models, add a corresponding
# component to Common in  the server function and the appropriate
# reactive to set  the Common  element each time the input  widget
# changes value.
# The annotation modal not used in this app

Common <- reactiveValues(
  model_type = "lm",
  model_order = 1,
  interaction_term = TRUE,
  selected_category = character(0)
)

# One of the two modals you define

observeEvent(input$show_annotations, {
  showModal(
    modalDialog(
      title = "Statistical Annotations", easyClose  = TRUE,
      p("No annotations in this app."))
  )
})

# The second of the two modals that you define in the app.

observeEvent(input$show_model, {
  showModal(
    modalDialog(
      title = "Model params", easyClose  = TRUE,
      pickerInput(
        inputId = 'model_type',
        label = "Type:",
        choices = list("linear combinations" = "lm",
                       "logistic" = "logistic"),
        selected = Common$model_type),

      prettyCheckbox(
        inputId = "interaction_term",
        label = "Interactions",
        value = as.logical(Common$interaction_term)),

      radioGroupButtons(
        inputId = 'model_order',
        label = "Model order:",
        choices = 0:6,
        selected = as.numeric(Common$model_order)),

      if (isTruthy(current_sample()) && isTruthy(input$response) &&
          nrow(current_sample()) > 2 && !is.numeric(current_sample()[input$response])) {
        selectInput(
          inputId = "selected_category",
          label = "Response ref. level:",
          choices = unique(current_sample()[input$response]),
          selected = Common$selected_category)
      }
    )
  )
})

format_stats <- function(stats) {
  # this doesn't need to be a reactive. It merely takes the <stats> output
  # from the main calculation and formats it.
  stats$F <- with(stats,  ((n-dflex)/dflex)*(R2/(1-R2)))
  res <- with(stats, glue::glue("
  <ul>
  <li>n = {n}</li>\n\n
  <li>dflex = {dflex}</li>\n\n
  <li>var_raw = {signif(var_raw,4)}</li>\n\n
  <li>nvar_model = {signif(var_model,4)}</li>\n\n
  </ul>
  <p>thus ...</p>\n\n
  <ul>
  <li>R-sq = {signif(R2, 3)}</li>\n\n
  <li>F = {signif(F, 2)}</li>\n\n
  </ul>"))

  HTML(res)
}


current_stats  <- reactive({
  format_stats(main_calculation()$stats)
})
frozen_stats <- reactive({
  format_stats(frozen_calculation()$stats)
})



observeEvent(input$model_type,  {
  Common$model_type <<- input$model_type
})
observeEvent(input$interaction_term, {
  cat("setting interaction term\n")
  Common$interaction_term <<- input$interaction_term
  cat("Set interaction to  ", Common$interaction_term, "\n")
})
observeEvent(input$model_order, {
  Common$model_order <<- input$model_order
})
observeEvent(input$selected_category,  {
  Common$selected_category <<- input$selected_category
})
