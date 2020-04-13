# Contains definitions of control modals, that is,  pop ups

# The `main_calculation` reactive carries out the necessary calculations
#  and returns them  in  a list.

app_title  <- reactive({"Stratification and confounding"})

shinyjs::hide("side_display")
shinyjs::hide("compare_what")


app_specific_data <- reactive({
  data <- current_sample()
  ylabels <- yrange <- NULL #initializing

  if (ncol(data) == 3) { #there is a covariate
    # discretize it as needed
    if (is.numeric(data[[3]])) {
      data[[3]] <- mosaic::ntiles(data[[3]],
                                  n = as.numeric(Common$nstrata))
    } else {
      data[[3]] <- forcats::fct_lump(data[[3]],
                                     n = as.numeric(Common$nstrata) - 1)
    }
  }

  # Make sure the response variable is numeric
  response_census <- raw_data()[[response_name()]]
  if (!is.numeric(response_census)) { # categorical response variable
    data[1] <- resp <- # resp holds the levels as an attribute
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
      stop("Shouldn't get here. Contact developer. Missing levels.")
    }
  }

  list(data = data, labels = ylabels, yrange = yrange)
})

main_calculation <- reactive({
  res <- app_specific_data()
  data <- res$data
  response_labels <- res$labels # labels for response if converted from factor

  big_mod_formula <- model_formula() # with the covariate

  if (explanatory_name() == response_name()) {
    output$explain_explanatory <- renderText({
      "Explanatory variable must be different from response variable."
    })

    req(FALSE)
  } else {
    output$explain_explanatory <- renderText({""})
  }

  small_mod_formula <-
    if(Common$model_order == 1 || !is.numeric(data[[2]]))
      as.formula(paste(response_name(), "~", explanatory_name()))
    else if(Common$model_order == 0)
      as.formula(paste(response_name(), "~ 1"))
    else
      as.formula(glue::glue(
        "{response_name()} ~ ns({explanatory_name()}, {Common$model_order})")
      )
  if (!is.na(covariate_name())) {
    plot_formula <- as.formula(glue::glue("{response_name()} ~ {explanatory_name()} | {covariate_name()}"))
    color_formula <- as.formula(glue::glue("~ {covariate_name()}"))
  } else {
    plot_formula <- as.formula(glue::glue("{response_name()} ~ {explanatory_name()}"))
    color_formula <- "black"
  }

  mod_family <-
    if (max(data[[1]]) <= 1 && min(data[[1]]) >= 0) binomial() # logistic model
    else gaussian() # linear model

  small_model <- try(
    glm(small_mod_formula, data = data, family = mod_family),
    silent = TRUE)
  big_model <- try(glm(big_mod_formula, data = data, family = mod_family),
                   silent = TRUE)
  if (inherits(small_model, "try-error")) {
    output$explain_explanatory <- renderText({
      "Not enough variation in the explanatory variable!!"
    })
  } else {
    output$explain_explanatory <- renderText({NULL})
    if (inherits(big_model, "try-error")) {
      output$explain_covariate <- renderText({
        "The covariate doesn't show sufficient variation."
      })
    } else output$explain_covariate <- renderText({NULL})
  }
  if (isTruthy(covariate_name())) {
    if (explanatory_name() == covariate_name() ||
        response_name() == covariate_name()) {
      output$explain_covar <- renderText({
        "It makes no sense to have the covariate
                  be the same as the response or explanatory variables."
      })

      req(FALSE)
    } else {
      output$explain_covar <- renderText({""})
    }
  }

  req(small_model)
  req(big_model)
  # set graph scale based on raw data
  yvals <- raw_data()[[response_name()]]
  yrange <-
    if (is.numeric(yvals)) range(yvals, na.rm = TRUE)
    else  c(-.1, 1.1)

  # find values for the model-evaluation grid
  x_grid <- if (is.numeric(data[[2]])) {
    span <- range(data[[2]])
    seq(span[1],span[2], length = 100)
  } else {
    unique(data[[2]])
  }

  if (ncol(data) < 3) Current_vars(Current_vars()[1:2])

  if(is.na(covariate_name())) {
    new_grid <- data.frame(x_grid, stringsAsFactors = FALSE)
    names(new_grid) <- explanatory_name()
  } else {
    covar_grid <- unique(data[[3]])
    new_grid <- expand.grid(x_grid, covar_grid)
    names(new_grid) <- c(explanatory_name(), covariate_name())
  }
  jitter_height <-
    if (length(unique(data[[1]])) > 2) 0
    else 0.1
  jitter_width <-
    if (is.numeric(data[[2]]) && length(unique(data[[2]])) > 2) 0
    else 0.1

  P <- gf_jitter(plot_formula, data = data,
                      color = color_formula,
                      alpha = point_alpha(nrow(data)),
                      height = jitter_height,
                      width = jitter_width) %>%
    gf_theme(legend.position="none",
             axis.text.x = element_text(angle = 30, hjust = 1))
  # Need to add the labels, if any exist
  if ( !is.null(response_labels)) {
    P <- P %>%
      gf_refine(
        scale_y_continuous(
          response_name(), breaks  =  c(0, .25, .5, .75, 1),
          labels = c(response_labels[1],
                     .25,  .5, .75,
                     response_labels[2]),
          limits = yrange))
  } else if (!is.null(yrange)) {
    P <- P %>% gf_lims(y  = yrange)
  }

  small_model_values <- predict(small_model, newdata = new_grid,
                                type = "response", se.fit = TRUE)
  small_model_values <- with(small_model_values,
    data.frame(lower = fit - 2 * se.fit,
               upper = fit + 2 * se.fit))
  big_model_values <- predict(big_model, newdata = new_grid,
                                type = "response")
  small_model_values <- cbind(new_grid, small_model_values)
  big_model_values <- cbind(new_grid, data.frame(mod_value = big_model_values))

  formula_for_big_mod <- formula_for_small_mod <- plot_formula
  formula_for_small_mod[[2]] <-
    as.formula("~ lower + upper")[[2]]

  if (is.numeric(data[[2]])) {
    formula_for_big_mod[[2]] <- as.name("mod_value")

    P <- P %>%
      gf_ribbon(formula_for_small_mod, data = small_model_values,
              color = NA, fill="black", inherit = FALSE) %>%
      gf_line(formula_for_big_mod, data = big_model_values,
              color = color_formula, inherit = FALSE,
              alpha = 1, size = 2)
  } else {
    formula_for_big_mod[[2]] <-
      as.formula("~ mod_value + mod_value")[[2]]
    P <- P %>%
      gf_errorbar(formula_for_small_mod, data = small_model_values,
            color = "black", inherit = FALSE, width = 0.2) %>%
      gf_errorbar(formula_for_big_mod, data = big_model_values,
              color = color_formula, inherit = FALSE,
              alpha = 1)
  }

  P <- P %>%
    gf_theme(theme_light(),
             axis.text.x = element_text(angle = 30, hjust = 1))
  return(list(main = P,
              side = NULL,
              stats = list(small = small_model,
                           big = big_model,
                           raw = data[[1]])))

})

plot_arrangement <- function(main, aux) {
  gridExtra::grid.arrange(main, aux, nrow = 1, widths = c(3,1))
}

observeEvent(response_name(), {
  vals <- raw_data()[[response_name()]]
  if (is.numeric(vals)) {
    output$explain_response  <- renderText({NULL})
  } else {
    output$explain_response <- renderText({
      "Categorical response variable converted to a 0/1 indicator variable."
    })
  }
})

model_formula <- reactive({
  req(current_sample())
  vars <-  names(current_sample())
  req(length(vars) >= 2)
  model_order <-
    if(! is.numeric(current_sample()[[2]])) 1
    else Common$model_order
  get_model_formula(current_sample(), model_order,
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
  selected_category = character(0),
  nstrata = 3
)

# The modal that you define in the app.

observeEvent(input$show_app_params, {
  showModal(
    modalDialog(
      title = "Model params", easyClose  = TRUE,
      p("Stratification means 'layering,' that is, to divide into layers. In this app,
        we divide the covariate into layers. That way, we can show the relationship
        between the response and the primary explanatory variable separately for
        each layer of the covariate. This control sets the number of layers
        to use when converting a continuous numerical variable into discrete
        levels."),
      radioGroupButtons(
        inputId = 'nstrata',
        label = "Number of strata for covariate:",
        choices = 2:6,
        selected = as.numeric(Common$nstrata)),
      tags$hr(),
      p("These controls govern the 'shape' of the model fit to the
        data. When there is an interaction term, the relationship
        between the response and primary explanatory variable can be
        different for each level of the covariate. Without an interaction,
        all the model functions will have the same shape."),
      prettyCheckbox(
        inputId = "interaction_term",
        label = "Interactions",
        value = as.logical(Common$interaction_term)),
      tags$br(),
      p("The model order sets how 'curvy' the model function can be. 1 means
        a straight line. (This applies only when the primary explanatory
        variable is quantitative.)"),
      radioGroupButtons(
        inputId = 'model_order',
        label = "Model order:",
        choices = 0:6,
        selected = as.numeric(Common$model_order)),
      tags$hr(),
      p("This control (not yet installed) allows you to set which level
        of a categorical response variable to assign to 1 when turning the variable
        into a 0/1 format for model building."),
      if (isTruthy(current_sample()) && isTruthy(response_name()) &&
          nrow(current_sample()) > 2 && !is.numeric(current_sample()[response_name()])) {
        selectInput(
          inputId = "selected_category",
          label = "Response ref. level:",
          choices = unique(current_sample()[response_name()]),
          selected = Common$selected_category)
      },
      footer = modalButton("Go back to the app ...")
    )
  )
})

# Source the the explanation document
output$explain_text <- renderText({HTML(
  paste(
    readLines("www/explain-F.html"),
    collapse = "\n")
)})


#---
# App specific translation between main_calculation()$stats and
# HTML to be displayed in the stats tab.
# This is not a reactive, but still needs to be defined  in
# app_specific_services since it will be different for each app.
format_stats <- function(stats) {
  small_fitted <- fitted(stats$small)
  big_fitted <- fitted(stats$big)
  small_deg_flex <- length(coef(stats$small))
  big_deg_flex <- length(coef(stats$big))
  small_resids <- var(stats$raw - small_fitted)
  big_resids   <- var(stats$raw - big_fitted)
  raw_var <- var(stats$raw)

  R2_small <- 1 - small_resids / raw_var
  R2_big   <- 1 - big_resids   / raw_var

  F_small <- ((length(stats$raw) - small_deg_flex)* (R2_small)/(1-R2_small))/(small_deg_flex - 1)
  F_big <- ((length(stats$raw) - big_deg_flex)* (R2_big)/(1-R2_big))/(big_deg_flex - 1)


  HTML(glue::glue(
  "<ul>
      <li>Variance of response: {signif(raw_var, 3)}</li>
      <li>R-squared small model: {signif(R2_small, 3)} with {small_deg_flex-1} degrees of flexibility
        <ul><li>giving F = {signif(F_small, 3)}</li></ul>
        </li>
      <li>R-squared big model: {signif(R2_big, 3)} with {big_deg_flex-1} degrees of flexibility
        <ul><li>giving F = {signif(F_big, 3)}</li></ul>
        </li>

  </ul>"))
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
observeEvent(input$nstrata,  {
  Common$nstrata <<- input$nstrata
})
