#' A few custom widgets
#'
#' Create a smaller version of a shiny selectInput
#'
#' @param id character string id to be assigned to the widget
#' @param choices list or vector of choices for the selectInput
#'
#' @export
smaller_select <- function(id, choices) {
  raw <- capture.output(selectInput(id, "b", choices, width="200px"))[-(1:2)]
  HTML(paste(raw[-(length(raw) - c(1, 0))], collapse = "\n"))
}

