# for the Bootstrap Little App
library(ungeviz) # for the special graphics

app_title <- reactive({
  "Resampling"
})
#-------------
ntrials <- reactiveVal(5)
model_order <- reactiveVal(1)

# A top-line control for a new resampling trial
output$next_to_new_sample <- renderUI({
  span("  ",
       actionBttn(
         inputId = "new_trial",
         label = "resample", icon = icon("redo"),
         size= "md")
  )
})
#-------------
# App specific controls
observeEvent(input$show_app_params, {
  showModal(
    modalDialog(
      title = "Parameters for display", easyClose  = TRUE,
      p("The main plot always shows a single resampling trial
        (along with the sample itself). Pressing ", icon("redo"),
        "draws a new random resampling trial, while leaving the sample
        as it was."),
      p("The auxiliary plot shows multiple resampling trials of the same sample.
        The spread of these is an indication of the size of sampling variability."),
      pickerInput(
        inputId = 'number_of_trials',
        label = "Number of resampling trials:",
        choices = c(2, 5, 10, 20, 100),
        selected = ntrials()),
      br(),
      p("When the explanatory variable is numerical, you can adjust
        the degrees of flexibility of the model constructed of the
        response variable as a function of the explanatory variable.
        1 corresponds to a straight line, 2 is curvier, ..."),
      radioGroupButtons(
        inputId = 'model_order',
        label = "Model order:",
        choices = 0:6,
        selected = as.numeric(model_order()))
    )
  )
})

observeEvent(input$number_of_trials, {
  ntrials(input$number_of_trials)
})
observeEvent(input$model_order, {
  model_order(input$model_order)
})


#--------------
main_calculation <- reactive({
  req(ncol(current_sample() >= 1))
  input$new_trial # for the dependency
  mod_formula <- as.formula(paste(response_name(), "~", explanatory_name()))
  sample <- current_sample()


  P <- if (is.numeric(sample[[2]]))
    show_bootstrap_sample_cont(sample, mod_formula,
                               ns = as.numeric(model_order()))
  else
    show_bootstrap_sample_disc(sample, mod_formula)

  S <- if (is.numeric(sample[[2]]))
    show_bootstrap_ensemble_cont(sample, mod_formula,
                                 ns = as.numeric(model_order()),
                                 ntrials = as.numeric(ntrials()))
  else
    show_bootstrap_ensemble_disc(sample, mod_formula,
                                 ntrials = as.numeric(ntrials()))

  stats <- list(a = 1, b = 2) # dummy


  list(main = P, side = S, stats = stats, arrange = arrange)
})

plot_arrangement <- function(main, aux) {
  gridExtra::grid.arrange(main, aux, nrow = 1, widths = c(2, 2))
}
#-----------------
format_stats <- function(stats) { # A muggle function
  res <- with (stats, {
    glue::glue("There is no numerical display in this Little App.")
  })

  return(HTML(res))
}
