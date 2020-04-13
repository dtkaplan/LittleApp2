#' Main calculations for the F app
#'
#' Main calculations for any app consist of ...
#' - (1) The main plot `main`
#' - (2) The  side plot `side`. Can be null is none is needed.
#' - (3) `stats` A data  frame or list with statistics on the system

#' list(main = P1 %>% gf_theme(legend.position  =  "left"),
#'
#' A basic plot showing two or three variables:
#' - a response
#' - an explanatory variable
#' - a covariate (optional)
#' Constructs two graphics, one showing the data
#' and (optionally) the fitted model, the other showing
#' showing the side plot comparing the distribution
#' of raw to  model values
#' @param formula of form A ~ B or A ~ B + C. You can also
#' use,  say, ns(B,2) or * instead of +. But at most two
#' explanatory variables.
#' @param data a data frame with the sample of data to display
#' @param show_model flag to draw the model
#' @param yrange a y range to override that implied by the data
#' @param labels Optionally, a length-2 character giving the labels for 0 and 1
#' when the response variable is an indicator variable.
#' @param sd  logical flag to show the plus-or-minus 1 sd in  the side plot
#' @param R2 logical flag to show the R-squared in the side plot
#' @param F  logical flag to show the F value in the side plot
#'
#' @import ggformula
#' @importFrom splines ns
#' @importFrom stats as.formula fitted lm median var
#' @importFrom utils capture.output data
#' @import forcats
#' @importFrom glue glue
#' @import miniUI
#' @import rlang
#' @import tidyr
#'
#' @examples
#' tmp <- F_app_plot(wage ~ splines::ns(educ, 2)*sex,  data = mosaicData::CPS85)
#' @return  A list with the two plots (P1 and P2),
#' the fitted model, and the stats.
#' @export

F_main_calc <- function(formula, data, yrange = NULL,
                        labels = NULL,
                        show_model = TRUE,
                        sd  =  TRUE) {
  # Don't really need the last  few arguments. I wanted to have them
  # so that I could design the app. Perhaps I'll add controls about
  # what to show and use them  to  replace the static TRUE/FALSEs

  response <- data[[formula[[2]]]]
  response_name <- as.character(formula[[2]])
  top  <- NULL # a flag

  explanatory_vars <- all.vars(formula[[3]])

  covariate <- NULL
  explanatory <- data[[explanatory_vars[1]]]
  explanatory_name <- explanatory_vars[[1]]
  # is there  a  covariate
  if (length(explanatory_vars) > 1) {
    covariate <- data[[explanatory_vars[2]]]
    covariate_name <- explanatory_vars[2]
  }
  model <- lm(formula, data)
  data$model_output  <- predict(model)
  mod_data <- data  # a copy which will be perhaps altered later
  if (!is.null(covariate) && is.numeric(covariate)) {
    # discretize the covariate for the displayed version  of the
    # model (but not the version used for the  F calculations)
    discrete_values <- seq(min(covariate), max(covariate),
                           length = 4)
    # Find the discrete_value that's closest to each element of
    # the covariate. Use that for  getting the model values.

    mod_data[[3]] <- round_to_closest(covariate,  discrete_values)
    mod_data$model_output <- predict(model, newdata = mod_data)
  }


  #  pull  out the  spatial  formula
  plot_formula <- as.formula(paste(response_name, "~",
                                   explanatory_name))
  color_formula <-
    if  (is.null(covariate)) "black"
  else as.formula(paste("~", covariate_name))
  alpha <- point_alpha(nrow(data))
  P1 <-
    if (is.numeric(explanatory) &&
        length(unique(explanatory)) == 2 ) {
        gf_point(plot_formula, data  = data,
                 color  = color_formula, alpha = alpha)
    } else {
      gf_jitter(plot_formula,  data = data,
                color  = color_formula,
                alpha = alpha,
                width  =  0.2, height = 0.1)
    }
  if (!is.null(labels)) {
    # format the  y  scale nicely  for a dichotomous
    # variable
    P1 <- P1 %>%
      gf_refine(
        scale_y_continuous(
          response_name, breaks  =  c(0, .25, .5, .75, 1),
          labels = c(labels[1], .25,  .5, .75, labels[2]),
          limits = c(-.1, 1.1)))
  } else if (!is.null(yrange)) {
    # put the y scale on a common footing for all samples
    P1 <- P1 %>% gf_lims(y  = yrange)
  }

  if (show_model) {
    color_formula <-
      if  (is.null(covariate)) "blue"
    else as.formula(paste("~", covariate_name))

    if (is.numeric(explanatory)) {
      mod_plot_formula <-
        as.formula(paste("model_output ~",
                         explanatory_name))
      P1  <- P1 %>%
        gf_line(mod_plot_formula, data  = mod_data,
                color = color_formula, group  = color_formula)
    } else {
      mod_plot_formula <-
        as.formula(paste("model_output  + model_output ~",  explanatory_name))
      P1 <- P1 %>%
        gf_errorbar(mod_plot_formula,
                    data = mod_data,
                    color  =  color_formula)
    }
  }
  P1 <- P1 %>%
    gf_theme(theme_minimal())

  P2 <- F_side_plot(response,
                    data$model_output,
                    explan = explanatory,
                    dflex = model$rank -  1,
                    sd = sd)

  auxplot <- P2$P %>%
    gf_theme(theme_minimal()) %>%
    gf_theme(legend.position = "none")

  if (!is.null(labels)) {
    # format the  y  scale nicely  for a dichotomous
    # variable
    auxplot <- auxplot %>%
      gf_refine(
        scale_y_continuous(
          response_name, breaks  =  c(0, .25, .5, .75, 1),
          labels = c(labels[1], .25,  .5, .75, labels[2]),
          limits = c(-.1, 1.1)))
  } else if (!is.null(yrange)) {
    # put the y scale on a common footing for all samples
   auxplot <- auxplot %>% gf_lims(y  = yrange)
  }

  if (!is.null(top)) {
    P2$P <- P2$P  %>% gf_lims(y  = c(-.1, 1.1))
  } else if (!is.null(yrange)) {
    P2$P <- P2$P %>% gf_lims(y = yrange)
  }

  list(main = P1 %>% gf_theme(legend.position  =  "left"),
       side  = auxplot,
       stats = P2$stats)
}
