#' A few custom widgets
#'
#' @export
smaller_select <- function(id, choices) {
  raw <- capture.output(selectInput(id, "b", choices, width="200px"))[-(1:2)]
  HTML(paste(raw[-(length(raw) - c(1, 0))], collapse = "\n"))
}

