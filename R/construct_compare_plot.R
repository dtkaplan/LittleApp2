#' Lay out the plots for the compare tab
#'
#' For internal use ... to lay out the main and side plots
#' for each of the current or frozen sides of the "compare" tab.
#'
#' @param calc the results  of the main (or frozen) calculation
#' @param what whether to display the "data plot" or the "model values"
#' side plot,  or both
#' @param plot_arrangement a function to lay out the two plots. Default is
#' side by side, with the main plot taking 3 units of width and the aux plot 1.
#' @export
construct_compare_plot <- function(calc,
                                   what = c("main", "aux", "both"),
                                   plot_arrangement = three_one) {
  match.arg(what)
  if (is.null(calc$side)) return(calc$main)
  if (what == "main") calc$main
  else if (what == "aux") calc$side
  else plot_arrangement(calc$main, calc$side)
}

# The default plot arrangement
three_one <- function(main, aux) {
  gridExtra::grid.arrange(main, aux, nrow = 1, widths = c(3,1))
}
