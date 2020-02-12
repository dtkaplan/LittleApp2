#' Layout helpers
#'
#' @param ... shiny widgets with a name, e,.g. `freq <- sliderInput('one', ....)`
#' @export
side_by_side_table <- function(...) {
  items <- list(...)
  widget_row <- paste(
    glue(
      "<td width='50%'>{{items[[{1:length(items)}]]}}</td>"),
    collapse="")
  nms <- names(items)
  if  (length(nms) == length(items)) {
    header_row <- paste(glue("<th>{{nms[[{1:length(items)}]]}}</th>"), collapse="")
    whole_table <- glue(
      "<table>
      <tr>{header_row}</tr>
      <tr>{widget_row}</tr>
    </table>"
    )
  } else {
    whole_table <- glue(
      "<table>
      <tr>{widget_row}</tr>
    </table>")
  }
  HTML(glue(whole_table))
}
