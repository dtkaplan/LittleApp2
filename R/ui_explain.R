#' Create an explain tab
#'
#' An explain tab containing an htmlOutput
#' @param title A (short) character string naming the tab.
#' @param icon An optional icon, e.g. `"bicycle"`. Set to NULL to disable

#' @export
ui_explain_tab <- function(title = "",
                           icon = "bicycle")  {

  res <- miniTabPanel(
    paste("Little App:", title),
    icon = icon(icon),
    # miniContentPanel(padding = 5,
    #                  htmlOutput("explain_text")
    # )
  )

  res
}
