#' Layout helpers
#'
#' @param ... shiny widgets with a name, e,.g. `freq <- sliderInput('one', ....)`
#' @export
side_by_side_table <- function(...) {
  items <- list(...)
  nms <- names(items)
  header_row <- paste(glue::glue("<th>{{nms[[{1:length(items)}]]}}</th>"), collapse="")
  widget_row <- paste(glue::glue("<td width='50%'>{{items[[{1:length(items)}]]}}</td>"), collapse="")
  whole_table <- glue::glue(
    "<table>
      <tr>{header_row}</tr>
      <tr>{widget_row}</tr>
    </table>"
  )
  HTML(glue::glue(whole_table))
}
