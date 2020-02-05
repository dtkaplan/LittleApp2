#' Choose a suitable alpha level for the plot
#'
#' Based on the number of dots in the plot
#'
#' @param  the number of dots in the plot (that is, the sample size)
#'
#' @export
point_alpha <- function(n) {
  min(1, 0.2 + 1.6 / 2^log10(n)) # a trick?
}
