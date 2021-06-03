#' dichotomize a variable
#'
#' Split a variable into two levels. Logical variables
#' will be converted to a TRUE/FALSE factor. Numerical variables
#' will be split at the median and returned as a factor. Factors will be
#' lumped into the most populated and all the others.
#'
#' @param x the variable (as a vector) to be split
#' @param pop an (optional) reference set of data (typically from a much larger sample)
#' that determines the dichotomization level.
#' @param force a logical flag. If `TRUE`, even
#' numerical variables will be split.
#' @param to_numeric a logical flag. If `TRUE`, dichotomized factors
#' will be reported as 0/1 numbers with the levels in the attributes.
#'
#' @details When `to_numeric` is true, the names of the dichotomized levels
#' will be reported as an attribute `levels` in the result. This can be used to label the
#' plot. (It will be `NULL` if the variable was already numeric.)
#'
#'
#' @export
#'
dichotomize <- function(x, pop = x, force=TRUE, to_numeric = FALSE) {

  smaller_date <- "on or before"
  smaller_number <- "median or less"

  if (is.logical(x)) {
    # treat as numerical, but plot with labels false and true
    x <- as.numeric(x)
    attr(x, "levels") <- c("false", "true")
    return(x) # we're done
  }


  if (is.numeric(x)) {
    if (force) {
      tmp <- x > median(pop, na.rm = TRUE)
      if (sd(tmp) == 0) res <- "All the same"
      else {
        res <- try(factor(
          tmp,
          labels = c(smaller_number, "greater than median"))
        )
      }
      if (! inherits(res, "try-error")) {
        x <- res
      }
    }
  } else  if (inherits(x, "Date")) {
    if (force) {
      when <- median(pop, na.rm = TRUE)
      x <- factor(
        x >= when,
        labels = c(smaller_date, paste("after", when)))
    }
  }
  else if (is.character(x) || is.factor(x)){ # character or factor
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
  } else {
    stop("Unrecognized variable type to dichotomize. Contact developer.")
  }

  if (!is.numeric(x) && to_numeric) {
      # convert to numeric 0/1 and store levels in attributes.
      levs <- levels(x)
      if ("Other" %in% levs) {
        # put other first
        if ("Other" == levs[2]) levs <- rev(levs)
        x <- ifelse(x == "Other",  0, 1)
      } else if (any(c(smaller_number, smaller_date) %in% levs)) {
        x <- ifelse(x %in% c(smaller_number, smaller_date), 0, 1)
      } else { # A two-level factor
        x <- as.numeric(x) - 1
      }
      attr(x, "levels") <- levs
  }


  return(x)
}
