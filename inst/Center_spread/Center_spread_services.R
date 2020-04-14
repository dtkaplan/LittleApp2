# Center and Spread Little App

app_title <- reactive({
  "Center and Spread"
})
#-------------
ntrials <- reactiveVal(5)
model_order <- reactiveVal(1)

#-------------
# Display messages when variable conversion is being done
app_specific_data <- reactive({
  input$new_sample # for the dependency
  data <- current_sample()
  response_census <- raw_data()[[response_name()]]
  if (!is.numeric(response_census)) { # categorical response variable
    data[1] <- resp <-
      dichotomize(data[[1]], response_census,
                  force = FALSE, to_numeric = TRUE)
    if ("levels" %in% names(attributes(resp))) {
      yrange <- c(-.1, 1.1)
      ylabels <- attr(resp, "levels")
      output$explain_response <-
        renderText({
          glue::glue("Converted {response_name()} into 0/1 variable
                   where 1 means <{attr(resp, 'levels')[2]}>. That's because
                   this statistical display makes sense only for quantitative response variables.")
        })
    } else {
      error("Shouldn't get here. Contact developer. Missing levels.")
    }

  } else { # Numerical response
    ylabels <- NULL
    yrange <- range(response_census, na.rm = TRUE)
  }

  explan_census <- raw_data()[[explanatory_name()]]
  if (is.numeric(data[[2]])) {
    data[2] <- dichotomize(data[[2]], explan_census, force = TRUE)
    output$explain_explanatory <- renderText({
      "The explanatory variable was originally numeric, but
      has been converted to a categorical variable for this display,
      which is about 'groupwise' statistics."})
  } else {
    output$explain_explanatory <- renderText({NULL})
  }

  list(data = data, labels = ylabels, yrange = yrange,
       ylabels = ylabels)
})

#-------------
# App specific controls
Annots <- reactiveValues( # defaults
  show_mean = FALSE,
  show_median = FALSE,
  show_sd = FALSE,
  show_ci = FALSE,
  show_summary_interval = FALSE,
  show_violin = FALSE,
  prob_level = 0.95
)

# Synchronize the control values with the input widgets
observeEvent(input$show_mean, {Annots$show_mean <- input$show_mean})
observeEvent(input$show_median, {Annots$show_median <- input$show_median})
observeEvent(input$show_sd, {Annots$show_sd <- input$show_sd})
observeEvent(input$show_ci, {Annots$show_ci <- input$show_ci})
observeEvent(input$show_summary_interval, {
  Annots$show_summary_interval <- input$show_summary_interval})
observeEvent(input$show_violin, {Annots$show_violin <- input$show_violin})
observeEvent(input$prob_level, {Annots$prob_level <- as.numeric(input$prob_level)})

observeEvent(input$show_app_params, {
  showModal(
    modalDialog(
      title = paste("Parameters for", app_title(), "display"), easyClose  = TRUE,

      p("Whether to show various statistics on the response variable:"),
      tags$ul(
        tags$li(checkboxInput("show_mean", "Show mean", value = Annots$show_mean)),
        tags$li(checkboxInput("show_median", "Show median", value = Annots$show_median)),
        tags$li(checkboxInput("show_sd", "Show standard deviation as ruler", value = Annots$show_sd)),
      ),
      p("The standard deviation is a number that describes a distance. The app
        shows a 'ruler' with the standard deviation as the unit of distance."),
      tags$hr(),
      p("Measures incorporating a probability level:"),
      selectInput("prob_level", "Coverage/confidence level:",
                  choices = c(0.5, .75, 0.8, 0.85, 0.9, 0.95, 0.99, 0.999),
                  selected = Annots$prob_level),
      tags$ul(
        tags$li(
          p("The", strong("summary interval"), "covers the central fraction of
                  the data. That fraction is called the",
                  em("coverage level"), ". The summary interval tells about
            the spread of the individual data points."),
          checkboxInput("show_summary_interval", "Show summary interval",
                        value = Annots$show_summary_interval)),
        tags$li(
          p("The", strong("confidence interval"), "is not about the individual
            data points, it is about the", em("precision"), "of the estimate
            of the mean. For large n, most of the individual data points lie
            outside the confidence interval. The length of the confidence interval
            is roughly the length of the summary interval divided by sqrt(n).
            Here, n =", n_size(), "so sqrt(n) = ", signif(sqrt(as.numeric(n_size())), 2), "."),
          checkboxInput("show_ci", "Show confidence interval.",
                        value = Annots$show_ci))
      ),
      tags$hr(),
      p("Density display:"),
      checkboxInput("show_violin", "Show violin plot.",
                    value = Annots$show_violin),
      ,
      footer = modalButton("Go back to the app ...")

    )
  )
})

#--------------
main_calculation <- reactive({
  res <- app_specific_data()
  mod_formula <- as.formula(paste(response_name(), "~", explanatory_name()))

  With_explanatory <-
    center_spread_graph(mod_formula, data = res$data,
                        yrange <- res$yrange,
                        labels =  res$labels,
                        annots = list(
                          show_median = Annots$show_median,
                          show_mean = Annots$show_mean,
                          show_ci = Annots$show_ci,
                          show_violin = Annots$show_violin,
                          show_sd = Annots$show_sd,
                          prob_level = Annots$prob_level,
                          show_summary_interval = Annots$show_summary_interval
                        ))

  simple_formula <- mod_formula
  simple_formula[[3]] <- 1
  Without_explanatory <- center_spread_graph(simple_formula, data = res$data,
                                             yrange <- res$yrange,
                                             labels =  res$labels,
                                             annots = list(
                                               show_median = Annots$show_median,
                                               show_mean = Annots$show_mean,
                                               show_ci = Annots$show_ci,
                                               show_violin = Annots$show_violin,
                                               show_sd = Annots$show_sd,
                                               prob_level = Annots$prob_level,
                                               show_summary_interval = Annots$show_summary_interval
                                             ))

  stats <- With_explanatory$S

  stats$raw <- Without_explanatory$S$resid


  list(main = With_explanatory$P %>%
         gf_theme(theme_minimal()),
       side = Without_explanatory$P %>%
         gf_theme(theme_minimal()),
       stats = stats)
})

plot_arrangement <- function(main, aux) {
  gridExtra::grid.arrange(main, aux, nrow = 1, widths = c(3, 1))
}
#-----------------
format_stats <- function(stats) { # A muggle function
  res <- with (stats, {
    glue::glue("For response variable '{response_name()}':\n
    <ul><li>standard deviation (residuals) = {signif(stats$raw, 3)}</li></ul>
    broken down by explanatory variable '{explanatory_name()}':\n
    <ul><li>standard deviation @ means: {signif(stats$fitted, 3)}</li>
    <li>standard deviation of resids: {signif(stats$resid, 3)}</li>
    <li>R^square = {signif(stats$fitted/stats$raw, 3)}</li></ul>"

    )
  })

  return(HTML(res))
}
