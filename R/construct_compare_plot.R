#' Lay out the plots for the compare tab
#'
#' For internal use ... to lay out the main and side plots
#' for each of the current or frozen sides of the "compare" tab.
#'
#' @param calc the results  of the main (or frozen) calculation
#' @param what whether to display the "data plot" or the "model values"
#' side plot,  or both
#' @export
construct_compare_plot <- function(calc, what) {
  if (is.null(calc$side)) return(calc$main)
  if (what == "data plot") calc$main
  else if (what == "model values") calc$side
  else {
    gridExtra::grid.arrange(calc$main,
                            calc$side,
                            nrow = 1,
                            widths = c(3,1))
  }
}
