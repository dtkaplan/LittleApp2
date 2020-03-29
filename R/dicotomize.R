#' Dicotomize a variable
#'
#' Split a variable into two levels. Logical variables
#' will be converted to a TRUE/FALSE factor. Numerical variables
#' will be split at the median and returned as a factor. Factors will be
#' lumped into the most populated and all the others.
#'
#' @param x the variable (as a vector) to be split
#' @param force a logical flag. If `TRUE`, even
#' numerical variables will be split.
#' @param to_numeric a logical flag. If `TRUE`, dicotomized factors
#' will be reported as 0/1 numbers with the levels in the attributes.
#'
#' @export
#'
dicotomize <- function(x, force=TRUE, to_numeric = FALSE) {

  if (is.logical(x)) x <- return(as.numeric(x))
  if (is.numeric(x)) {
    if (force) {
      res <- try(factor(
        x >= median(x, na.rm = TRUE),
        labels = c("median or less", "greater than median"))
      )
      if (inherits(res, "tryError")) {
        return(x)
      }
      return(res)
    } else {
      return(x)
    }
  }
  if (inherits(x, "Date")) {
    if (force) {
      when <- median(x, na.rm = TRUE)
      x <- factor(
        x >= when,
        labels = c("on or before", paste("after", when)))
    }
  }
  else { # character or factor
    if (length(unique(x)) > 2) {
      x <- forcats::fct_lump_n(
        x, n = 1,
        ties.method = "random")
      # put Other at the lower level
      x <- relevel(x, "Other")
      x <- factor(x, levels = rev(levels(x)))
    } else {
      x <- factor(x)
    }
    if (to_numeric) {
      # convert to numeric 0/1 and store levels in attributes.
      levs <- levels(x)
      x <- as.numeric(x) - 1
      attr(x, "levels") <- levs
    }
    return(x)
  }

  stop("Unhandled variable type in dicotomize(). Contact developer.")
}
