# app-specific services for t-test app

main_calculation <- reactive({
  data <- current_sample()
  req(ncol(data) %in% 1:2)
  modf <- model_formula()

  # make sure response is numeric and explanatory as 2-levels
  data[1] <- resp <- dicotomize(data[[1]], force = FALSE, to_numeric = TRUE)
  if ("levels" %in% names(attributes(resp))) {
    ylabels <- attr(resp, "levels")
  } else {
    ylabels <- NULL
  }
  data[2] <- dicotomize(data[[2]], force = TRUE)
  yvals <- raw_data()[[input$response]]

  yrange  <- range(as.numeric(yvals), na.rm = TRUE)

  res <- t_test_calcs(modf, data,
               level = Common$conf_level,
               show_mean = Common$show_mean,
               show_ci = Common$show_ci,
               show_t = Common$show_t,
               var_equal = Common$var_equal,
               y_range = NULL,
               null_hypothesis = as.numeric(Common$mu),
               y_labels = ylabels)

  res
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
  conf_level = 0.95,
  mu = 0
)

observeEvent(input$show_app_params, { #annotations, {
  if (is.numeric(current_sample()[[1]])) {
    range <- range(current_sample()[[1]])
  } else {
    range <- c(0, 1) # when response is dicotomized
  }

  null_value <- Common$mu
  if (null_value > range[2] |
      null_value < range[1]) null_value <- mean(range, na.rm = TRUE)

  showModal(
    modalDialog(
      title = "Statistical Annotations", easyClose  = TRUE,
        tagList(
          checkboxInput("show_mean", "Show mean",  Common$show_mean),
          checkboxInput("show_ci", "Show conf interval", Common$show_ci),
          checkboxInput("show_t", "Show t-interval",  Common$show_t),
          checkboxInput("var_equal", "Assume equal variances", Common$var_equal),
          selectInput("conf_level", "Confidence level",
                      choices  = c(0.5, 0.8, 0.9, 0.95, 0.99, 0.999),
                      selected = Common$conf_level),
          sliderInput("mu", "Null hypothesis value", min  = min(0, range ),
                      max = max(0, range), value = null_value)
      ),
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
observeEvent(input$var_equal, {
  Common$var_equal <-  input$var_equal
})
observeEvent(input$conf_level, {
  Common$conf_level <-  as.numeric(input$conf_level)
})
observeEvent(input$mu, {
  Common$mu <-  as.numeric(input$mu)
})

format_t_stats <- function(stats) {
  n_samp <- nrow(current_sample())
  if ((n_samp - stats$parameter) == 1) {
    my_estimate <- glue::glue("<li>mean = {signif(stats$estimate,  3)}</li>")
    my_method <- glue::glue("One-sample t-test with mu_0 = {Common$mu}")
  } else {
    groups <- gsub("mean in group", "", names(stats$estimate))
    my_estimate <- glue::glue(
    '<li>means: {groups[1]} = {signif(stats$estimate[1],3)} vs {groups[2]} = {signif(stats$estimate[2],3)}</li>
     <li>difference in means: {signif(diff(stats$estimate), 3)}</li>'
    )
    my_method <- glue::glue('method: {ifelse(Common$var_equal, "equal", "unequal")} variance two-sample t test')
  }

  res <- with(stats,
       glue::glue('<ul>
  <li>n = {n_samp}</li>
  {my_estimate}
  <li>CI = {signif(stats$conf.int[1],4)} to {signif(stats$conf.int[2], 4)} at
  {100*attr(stats$conf.int, "conf.level")}% level.</li>
  <li>t-statistic: {signif(stats$statistic, 3)}</li>
  <li>F-statistic: {signif(stats$statistic^2, 3)}</li>
  <li>Degrees flexibility: {signif(n_samp - (1+stats$parameter),3)}</li>
  <li>Degrees of freedom: {signif(stats$parameter,3)}</li>
  <li>{nice_p(stats$p.value)}</li>
  <li>{my_method}</li>
  </ul>'
       )
  )

  HTML(res)



}

current_stats  <- reactive({
  req(main_calculation())

  format_t_stats(main_calculation()$stats)
})
frozen_stats <- reactive({
  format_t_stats(frozen_calculation()$stats)
})
