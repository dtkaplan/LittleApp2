# app-specific services for t-test app

app_specific_data <- reactive({
  data <- current_sample()

  # force response to be numeric and explanatory as 2-levels
  data[1] <- resp <- dicotomize(data[[1]], force = FALSE, to_numeric = TRUE)
  if ("levels" %in% names(attributes(resp))) {
    ylabels <- attr(resp, "levels")
  } else {
    ylabels <- NULL
  }
  data[2] <- dicotomize(data[[2]], force = TRUE)

  list(data = data, labels = attr(resp, "levels"))
})

app_specific_plotable <- reactive({
  data <- app_specific_data()$data
  min(table(data[[2]])) > 1 && is.numeric(data[[1]])
})

main_calculation <- reactive({
  if (exists("app_specific_data")) {
    res <- app_specific_data()
    data <- res$data
    ylabels <- res$labels
  }
  else data <- current_sample()

  req(ncol(data) %in% 1:2)
  modf <- model_formula()

  if (!is_sample_plotable()) {
    blank_plot <-
      ggformula::gf_text(
        ggformula::gf_blank(modf, data = head(data,0)),
               1 ~ 1,
               label="Not enough variation in this sample\n
               Try sampling again,\n
               perhaps with a larger sample size.")
    res <- list(main = blank_plot,
                stats = HTML(
                  "<p>No variation in this sample.</p>
             <p>Try sampling again perhaps with a larger sample size.</p>"
                ),
                side = NULL
    )
    return(res)
  }

  yvals <- raw_data()[[response_name()]]

  if (is.numeric(yvals)) {
    yrange  <- range(as.numeric(yvals), na.rm = TRUE)
  }
  else {
    yrange <- c(0, 1)
  }
  res <-
    try(t_test_calcs(modf, data,
                      level = Common$conf_level,
                      show_mean = Common$show_mean,
                      show_ci = Common$show_ci,
                      show_t = Common$show_t,
                      var_equal = Common$var_equal,
               y_range = NULL,
               null_hypothesis = as.numeric(Common$mu),
               y_labels = ylabels,
               one_sample = Common$one_sample),
        silent = TRUE)
  if (inherits(res, "try-error")) {
    somethings_wrong_with_data()
    }
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
  one_sample = FALSE,
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
  testsize <- ifelse(Common$one_sample, 1, 2)

  showModal(
    modalDialog(
      title = "Statistical Annotations", easyClose  = TRUE,
        tagList(
          checkboxInput("show_mean", "Show mean",  Common$show_mean),
          checkboxInput("show_ci", "Show conf interval", Common$show_ci),
          checkboxInput("show_t", "Show t-interval",  Common$show_t),
          selectInput("conf_level", "Confidence level",
                      choices  = c(0.5, 0.8, 0.9, 0.95, 0.99, 0.999),
                      selected = Common$conf_level),
          checkboxInput("one_sample", "Ignore explanatory variable", value = Common$one_sample),
          #conditionalPanel("testsize == 1",
            sliderInput("mu", "Null hypothesis value", min  = min(0, range ),
                      max = max(0, range), value = null_value),
          #),
          #conditionalPanel("testsize == 2",
            checkboxInput("var_equal", "Assume equal variances", Common$var_equal)
          #)
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
observeEvent(input$one_sample, {
  Common$one_sample <-  input$one_sample
})
observeEvent(input$mu, {
  Common$mu <-  as.numeric(input$mu)
})

format_stats <- function(stats) {
  n_samp <- stats$sample_size
  if (length(stats$estimate) == 1) { # One-sample t-test
    my_estimate <- glue::glue("<li>mean = {signif(stats$estimate,  3)}</li>")
    my_method <- glue::glue("One-sample t-test with mu_0 = {stats$null.value}")
  } else {
    groups <- gsub("mean in group", "", names(stats$estimate))
    my_estimate <- glue::glue(
    '<li>means: {groups[1]} = {signif(stats$estimate[1],3)} vs {groups[2]} = {signif(stats$estimate[2],3)}</li>
     <li>difference in means: {signif(diff(stats$estimate), 3)}</li>'
    )
    my_method <- glue::glue('method: {stats$method} with {ifelse(stats$var.equal, "equal", "unequal")} variance')
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
  </ul>'   #with μ_0 = {stats$null.value}</li> for one-sample test.
       )
  )

  HTML(res)
}

