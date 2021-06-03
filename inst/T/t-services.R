# app-specific services for t-test app

app_title <- reactive({"t-test"})
shinyjs::hide("side_display")
shinyjs::hide("compare_what")

observeEvent(input$explanatory, {
  Common$one_sample <- input$explanatory == ""
})

plot_arrangement <- function(main, aux) {
  gridExtra::grid.arrange(main, aux, nrow = 1, widths = c(2, 2))
}

app_specific_data <- reactive({
  data <- current_sample()
  # get rid of the shuffling column, if any
  data <- data[, names(data) != ".orig.order."]
  response_census <- raw_data()[[response_name()]]
  # force response to be numeric and explanatory as 2-levels
  data[1] <- resp <-
    dichotomize(data[[1]], response_census,
               force = FALSE, to_numeric = TRUE)
  if ("levels" %in% names(attributes(resp))) {
    output$explain_response <-
      renderText({
        glue::glue("For t-test, response variable must be numeric.
                   Converted {response_name()} into 0/1 variable
                   where 1 means <{attr(resp, 'levels')[2]}>.")
      })
  } else {
      output$explain_response  <- renderText({NULL})
  }
  # Check whether the variable was originally categorical. If so
  # remember the levels corresponding to 0 and 1 so that the plot
  # can be made accordingly.
  if ("levels" %in% names(attributes(resp))) {
    ylabels <- attr(resp, "levels")
  } else {
    ylabels <- NULL
  }
  explan_census <- raw_data()[[explanatory_name()]]
  data[2] <- dichotomize(data[[2]], explan_census, force = TRUE)

  if ((!input$explanatory=="") && length(unique(explan_census)) > 2) {
    output$explain_explanatory <- renderText({
        glue::glue("The explanatory variable in a t test must be
        categorical with two levels. Converted the variable '{explanatory_name()}'
        into two levels:
        {paste(paste0('<', attributes(data[[2]])$levels, '>'), collapse = ' and ')}.")
      })
  } else {
    output$explain_explanatory  <- renderText({NULL})
  }
  list(data = data, labels = attr(resp, "levels"))
})

app_specific_plotable <- reactive({
  data <- app_specific_data()$data
  min(table(data[[2]])) > 1 && is.numeric(data[[1]])
})

# Reset the null hypothesis value when response variable changes
observeEvent(response_name(), {
  null_value_memory(0)
})

main_calculation <- reactive({
  input$randomize # for the dependency
  res <- app_specific_data()
  data <- res$data
  ylabels <- res$labels

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
               null_hypothesis = as.numeric(null_value_memory()),
               y_labels = ylabels,
               one_sample = Common$one_sample),
        silent = TRUE)
  if (inherits(res, "try-error")) {
    somethings_wrong_with_data()
  }

  res$main <- res$main %>%
    gf_theme(theme_minimal()) %>%
    gf_theme(legend.position="none")

  res
})

plot_arrangement <- function(main, aux) {
  gridExtra::grid.arrange(main, aux,
                          nrow=2,  ncol=1,  heights = c(2, 1))
}

model_formula <- reactive({
  req(current_sample())
  vars <-  setdiff(names(current_sample()), ".orig.order.")
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
  one_sample = FALSE
)

null_value_memory <- reactiveVal(0)

observeEvent(input$show_app_params, { #annotations, {
  if (is.numeric(current_sample()[[1]])) {
    resp_range <- range(current_sample()[[1]])
    if (resp_range[1] > 0) resp_range[1] <- 0
    if (resp_range[2] < 0) resp_range[2] <- 0
  } else {
    resp_range <- c(0, 1) # when response is dichotomized
  }

  if (null_value_memory() < resp_range[1]) null_value_memory(0)
  if (null_value_memory() > resp_range[2]) null_value_memory(0)

  testsize <- ifelse(Common$one_sample, 1, 2)


  showModal(
    modalDialog(
      title = "Statistical Annotations", easyClose  = TRUE,
          p("The t-test is fundamentally about means."),
          checkboxInput("show_mean", "Show mean",  Common$show_mean),
          p("The confidence interval on the mean reflects uncertainty
            due to random sampling. There are other sources of uncertainty,
            such as *sampling bias* and *confounding*, that are not incorporated into the
            confidence interval."),
          checkboxInput("show_ci", "Show conf interval", Common$show_ci),
          if (!Common$one_sample) checkboxInput("show_t", "Show t-interval",  Common$show_t),
          tags$hr(),
          p("The confidence level is typically set at 0.95. We give you a choice
            here so that you can explore how the intervals depend on the
            confidence level."),
          selectInput("conf_level", "Confidence level",
                      choices  = c(0.5, 0.8, 0.9, 0.95, 0.99, 0.999),
                      selected = Common$conf_level),
          tags$hr(),
          tags$h3('The "one-sample" test'),
          p("In a one-sample t-test, there is no explanatory variable
            and so there is just a single mean being tested. If you want
            to explore one-sample t-tests, check the box to ignore
            the explanatory variable. You can also set a level for the
            Null Hypothesis value, which is a value (typically 0) that
            you would be interested in rejecting."),
          if (Common$one_sample) {
            p("You're doing a one-sample test now. If you want to switch to a
              two-sample test, go to the data tab and choose and explanatory variable.")
          } else {
            p("To implement a one-sample test, set the explanatory
              variable in the Data tab to the top-most choice: 'set explanatory variable'.
              This effectively turns off the explanatory variable.")
          },
          # checkboxInput("one_sample", "Ignore explanatory variable", value = Common$one_sample),
          if (Common$one_sample)
            numericInput("mu", "Null hypothesis mu:", min  = resp_range[1],
                      max = resp_range[2], value = null_value_memory(),
                     width = "100%"),
          tags$hr(),

      if (!Common$one_sample)
        p("Some people, when doing a two-sample t-test, prefer a refined
            calculation that doesn't assume that the response variable
            has the same variance in the two groups. You can explore whether
            there is a good reason for this refined calculaton or whether
            the assumption of equal variances gives and adequate approximation."),
      if (!Common$one_sample)
        checkboxInput("var_equal", "Assume equal variances",
                      Common$var_equal),

      footer = modalButton("Go back to the app ...")
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
  Common$one_sample <- input$one_sample
})
observeEvent(input$mu, {
  null_value_memory(input$mu)
})

format_stats <- function(stats) {
  n_samp <- stats$sample_size
  if (length(stats$estimate) == 1) { # One-sample t-test
    my_estimate <- glue::glue("<li>mean = {signif(stats$estimate,  3)}</li>")
    my_method <- glue::glue("One-sample t-test with mu_0 = {stats$null.value}")
  } else {
    groups <- gsub("mean in group", "", names(stats$estimate))
    my_estimate <- glue::glue(
    '<li>means of {stats$response}:<ul><li>{stats$explanatory} = {groups[1]} = {signif(stats$estimate[1],3)} vs </li><li>{stats$explanatory} = {groups[2]} = {signif(stats$estimate[2],3)}</li></ul></li>
     <li>difference in means: {signif(- diff(stats$estimate), 3)}</li>
    <li>std error on difference: {signif(stats$stderr, 3)}</li>'
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

# Source the the explanation document
output$explain_text <- renderText({
  HTML(
    paste(
      readLines("www/explain-t.html"),
      collapse = "\n")
  )
})

