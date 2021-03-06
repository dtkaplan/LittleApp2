#' Functions for constructing the data UI
#'
#' Creates the tab with controls for selecting package, frame, response,
#' and  explanatory variables.
#'
#'
#'
#' @param package_list A named list whose values are character strings containing
#' the packages to be included and whose names are  the user-facing strings for the
#' select widget. See `default_packages_list` for an example.
#' @param covar Flag: whether to add a selector for a covariate
#' @param covar2 Flag: whether to add a selector for a second covariate (if `covar=TRUE`)
#'
#'
#' @export
data_tab <- function(package_list = default_packages_list,
                     explan = TRUE, covar = TRUE, covar2 = FALSE) {
   miniTabPanel(
     "Data", icon = icon("folder"), value = "data_panel",
     miniContentPanel(
      side_by_side_table(
        tagList(
          ui_select_data(package_list, explan, covar, covar2),
          checkboxInput("stratify", "Stratify sampling by explan vars")
                  ),
        tagList(
          plotOutput("preview_plot", width="300px", height="250px")
        )),
      verbatimTextOutput("frame_preview")
  )
   )
}


default_packages_list <-
  list("Little Apps" = "LittleApp2",
       "Open Intro" = "openintro",
       "Stats using Technology" = "StatsUsingTechnologyData",
       "mosaic" = "mosaicData",
       "Lock-5" = "Lock5Data",
       "Triola"  = "triola",
       "Sullivan" = "sullystats6e",
       "UPLOAD" = "UPLOAD")

ui_select_data <-  function(package_list = default_packages_list,
                            explan = TRUE,
                            covar = TRUE, covar2 = FALSE) {
  table_str <-
    glue(
      '<table id="data-controls">
    <col width="125pt"><col width="150pt">
    <tr><td>Source package</td><td>{smaller_select("package", package_list)}</td></tr>
    <tr><td>Data set</td><td>{smaller_select("frame", NULL)}</td></tr>
    <tr><td>Response</td><td>{smaller_select("response", NULL)}</td></tr>
    <tr><td colspan="2" class="explain_things">{textOutput("explain_response")}</td></tr>'

    )

  if (explan) {
    table_str <- paste(table_str, glue(
      '<tr><td>Explanatory</td><td>{smaller_select("explanatory", NULL)}</td></tr>
        <tr><td colspan="2" class="explain_things">{textOutput("explain_explanatory")}</td></tr>'     ))
  }

  if (covar) {
    table_str <- paste(table_str, glue(
      '<tr><td>Covar</td><td>{smaller_select("covariate", NULL)}</td></tr>
      <tr><td colspan="2">{textOutput("explain_covar")}</td></tr>'
    ))
    if (covar2) {
      table_str <- paste(table_str, glue(
        '<tr><td>Covar2</td><td>{smaller_select("covariate2", NULL)}</td></tr>
        <tr><td colspan="2">{textOutput("explain_covar2")}</td></tr>'
      ))
    }
  }

  HTML(paste(table_str), "</table>")
}

