#' Helper functions outside reactive context
#'
#' Get the names of frames available in the package
#'
#' @param package character string name of a package
#' @param frame character string name of a data frame
#' @param sample a data frame
#' @param model_order integer, the order of spline to use for the primary explanatory variable
#' when it is quantitative
#' @param interaction logical flag. If `TRUE` include interactions among the explanatory
#' variables
#' @param model_type character string specifying the model architecture. Either `"lm"` or `"logistic"`
#'
#' @export
get_package_frames <- function(package) {
  if (package == "UPLOAD")  return("uploaded_data")
  listing <- data(package = package)
  as.character(listing$results[ ,3])
}
#' @export
get_frame_var_names <- function(frame) {
  names(frame)
}

#' @export
get_new_sample <- function() {
  # Get  this sorted out for stratification.

  sample_size <- if (input$sample_size == "All") {
    nrow(raw_data())
  } else {
    as.numeric(input$sample_size)
  }

  nmax <- min(sample_size, nrow(raw_data))

  if  (length(the_variables) > 1) grouping_var <- Raw_data[[the_variables[2]]]

  if  (req(input$stratify) &&
       length(unique(grouping_var)) < 10) {

    Res <- NULL
    groups <- unique(grouping_var)
    counts_in_group <- numeric(length(groups))
    for (k in 1:length(groups)) {
      group <- groups[k]
      This_group <- Raw_data[group == groups[k]]
      nmax <- min(sample_size, nrow(This_group))
      counts_in_group[k] <- nmax
      Res <- bind_rows(Res, sample_n(This_group, size = nmax))
    }
    samp_size_message  <-
      paste(paste0(groups,
                   "[n=",  counts_in_group, "]"),
            collapse = " : ")
    output$sample_message <- renderText({samp_size_message})
  } else {
    # simple sample
    Res <- sample_n( Raw_data, size = max)
  }

}

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












