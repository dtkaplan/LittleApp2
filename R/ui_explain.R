#' Create an explain tab
#'
#' An explain tab shows a string that will be rendered as HTML
#' @param text A (short) character string naming the tab.
#' @param icon An optional icon, e.g. `icon("bicycle")`. Set to NULL to disable
#' @param text A character string containing HTML.

#' @export
ui_explain_tab <- function(title = "",
                           icon = "bicycle")  {

  res <- miniTabPanel(
    paste("Little App:", title),
    icon = icon(icon),
    miniContentPanel(padding = 5,
                     htmlOutput("explain_text")
    )
  )

  res
}
