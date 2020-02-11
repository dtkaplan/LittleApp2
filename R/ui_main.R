#' Layout for the main app
#'
#' A Little App  consists of a top block of controls that stay visible aways, and
#' a set of tabs that implement different views: e.g. select data, compare current
#' to frozen plot, show statistics, show a description of the app, ...
#'
#' @param top_controls contents of the top block of controls
#' @param ... tabs to  be inserted into  the app
#'
#' @export
ui_main <- function(top_controls, ...) {
  miniUI::miniPage(
    useShinyjs(),
    include_my_css(),
    miniButtonBlock(top_controls),
    miniTabstripPanel(type = "pills", ...)
  )
}

include_my_css <- function() {
  tags$head(
    tags$style(type="text/css",
               "#data-controls select {
       width: 140pt;
       color: blue;
       margin: 5px;
               }

     label.control-label, .selectize-control.single {
         display: table-cell;
         text-align: left;
         padding-right: 0pt;
         vertical-align: middle;
      }
      label.control-label {
        padding-right: 0px;
      }
      .form-group {
        display: table-row;
      }
      .selectize-control.single div.item {
        margin-right: 100px;
        text-align: left;
        color: blue;
      }")
  )
}
