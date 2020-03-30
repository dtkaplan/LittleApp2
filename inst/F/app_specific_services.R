# Contains definitions of control modals, that is,  pop ups

# The `main_calculation` reactive carries out the necessary calculations
#  and returns them  in  a list.
# THIS IS THE COMPUTATIONAL CORE OF THE APP
main_calculation <- reactive({
  req(ncol(current_sample()) > 1)
  # These two assignments are to avoid lazy evaluation of the reactives
  # when calling F_main_calc()
  modf <- model_formula()
  data <- current_sample()
  yvals <- raw_data()[[response_name()]]
  # dicotomize any categorical response variable
  if (! is.numeric(data[1])) {
     res <- dicotomize(data[[1]], yvals,
                           force = FALSE, to_numeric = TRUE)
     labels <- attr(res,  "levels") # will be NULL if there aren't any
     data[1] <- res
  }



  if (is.numeric(yvals)) {
    yrange  <- range(yvals, na.rm = TRUE)
  } else  {
    yrange  <- NULL
  }

  # Construct the plots,
  # making sure that the data are adequate.
  if (is_sample_plotable()) {
     F_main_calc(modf, data = data, yrange = yrange, labels = labels )
  } else {
     list(main = gf_blank(modf, data = head(data,0)) %>%
            gf_label(1 ~ 1, label="Not enough variation in this sample\nTry sampling again,\n perhaps with a larger sample size."),
          stats = HTML(
            "<p>No variation in this sample.</p>
             <p>Try sampling again perhaps with a larger sample size.</p>"
          ),
          side = NULL
     )
  }
})

observeEvent(response_name(), {
  vals <- raw_data()[[response_name()]]
  if (is.numeric(vals)) {
    output$discrete_response  <- renderText({NULL})
  } else {
    output$discrete_response <- renderText({
      "Categorical response variable converted to a 0/1 indicator variable."
    })
  }
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

# The modal that you define in the app.

observeEvent(input$show_app_params, {
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

      if (isTruthy(current_sample()) && isTruthy(response_name()) &&
          nrow(current_sample()) > 2 && !is.numeric(current_sample()[response_name()])) {
        selectInput(
          inputId = "selected_category",
          label = "Response ref. level:",
          choices = unique(current_sample()[response_name()]),
          selected = Common$selected_category)
      }
    )
  )
})

#---
# App specific translation between main_calculation()$stats and
# HTML to be displayed in the stats tab.
# This is not a reactive, but still needs to be defined  in
# app_specific_services since it will be different for each app.
format_stats <- function(stats) {
  # this doesn't need to be a reactive. It merely takes the <stats> output
  # from the main calculation and formats it.
  stats$F <- with(stats,  ((n-(1+dflex))/dflex)*(R2/(1-R2)))
  res <- with(stats, glue::glue("
  <ul>
  <li>n = {n}</li>\n\n
  <li>dflex = {dflex}</li>\n\n
  <li>var_raw = {signif(var_raw,4)}</li>\n\n
  <li>var_model = {signif(var_model,4)}</li>\n\n
  </ul>
  <p>thus ...</p>\n\n
  <ul>
  <li>R-sq = {signif(R2, 3)}</li>\n\n
  <li>F = {signif(F, 2)}</li>\n\n
  </ul>"))

  HTML(res)
}

# Managing parameters that make sense only in the context
# of this specific app. These parameters are set in the
# app-specific parameter modal.
# The inputs in the modal, are transient, existing only when the
# modal is being displayed. These observers take the transient values
# and  store them persistently in <Common>


observeEvent(input$model_type,  {
  Common$model_type <<- input$model_type
})
observeEvent(input$interaction_term, {
  Common$interaction_term <<- input$interaction_term
})
observeEvent(input$model_order, {
  Common$model_order <<- input$model_order
})
observeEvent(input$selected_category,  {
  Common$selected_category <<- input$selected_category
})
