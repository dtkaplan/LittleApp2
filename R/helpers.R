#' Helper functions outside reactive context
#'
#' Get the names of frames available in the package
#'
#' @param frame character string name of a data frame
#' @param sample a data frame
#' @param model_order integer, the order of spline to use for the primary explanatory variable
#' when it is quantitative
#' @param interaction logical flag. If `TRUE` include interactions among the explanatory
#' variables
#' @param model_type character string specifying the model architecture. Either `"lm"` or `"logistic"`

#' construct the model formula
#' @export
get_model_formula <- function(sample, model_order=1, interaction=TRUE,
                              model_type = "lm") {

  # interactions only for linear and logistic models
  # add other glms as required
  interaction_symbol <-
    ifelse(interaction && model_type %in% c("lm", "logistic"),
      "*",  "+")

  vars <- names(sample)

  if (length(vars) == 1) {
    return(as.formula(glue("{vars[1]} ~ 1")))
  }

  explan <- vars[2]
  if (model_order > 1) explan <- glue("ns({explan}, {model_order})")

  if (length(vars) == 2) {
    return(as.formula(glue("{vars[1]} ~ {explan}")))
  }
  if (length(vars) == 3) {
    return(as.formula(glue("{vars[1]} ~ {explan} {interaction_symbol} {vars[3]}")))
  }
  if (length(vars) == 4) {
    return(as.formula(glue(
      "{vars[1]} ~ {explan} {interaction_symbol} {vars[3]} {interaction_symbol} {vars[4]}")))
  }
  stop("Should never get here")
}

#' @export
nice_p <- function(p, n=2) {
  order <- floor(log10(p))
  if (order < -4) return("p value < 0.0001")
  first_two_digits <- round(p * 10^-(order-(n-1)))
  paste0("p value: 0.", paste0(rep("0", -(order+1)), collapse=""), first_two_digits)
}












