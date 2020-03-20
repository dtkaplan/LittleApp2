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
data_tab <- function(package_list = default_packages_list,                            covar = TRUE, covar2 = FALSE) {
  miniTabPanel(
    "Data", icon = icon("folder"),
    miniContentPanel(
      side_by_side_table(
        tagList(
          ui_select_data(package_list, covar, covar2),
          actionButton("show_explain", "Explain App"),
          actionButton("show_metadata", "Show codebook"),
          checkboxInput("stratify", "Stratify by explan vars")
        ),
        tagList(
          plotOutput("preview_plot", width="300px", height="250px")
        ))
      ,
      verbatimTextOutput("frame_preview"))
  )
}


default_packages_list <-
  list("Little Apps" = "LittleApp2",
       "Open Intro" = "openintro",
       "Stats with Technology" = "MAT160",
       "mosaic" = "mosaicData",
       "Lock-5" = "Lock5Data",
       "Triola"  = "triola",
       "UPLOAD" = "UPLOAD")

ui_select_data <-  function(package_list = default_packages_list,
                            covar = TRUE, covar2 = FALSE) {
  table_str <-
    glue(
      '<table id="data-controls">
    <col width="150pt"><col width="200pt">
    <tr><td>Source package</td><td>{smaller_select("package", package_list)}</td></tr>
    <tr><td>Data set</td><td>{smaller_select("frame", NULL)}</td></tr>
    <tr><td>Response</td><td>{smaller_select("response", NULL)}</td></tr>
    <tr><td>Explan</td><td>{smaller_select("explanatory", NULL)}</td></tr>'
    )

  if (covar) {
    table_str <- paste(table_str, glue(
      '<tr><td>Covar</td><td>{smaller_select("covariate", NULL)}</td></tr>'
    ))
    if (covar2) {
      table_str <- paste(table_str, glue(
        '<tr><td>Covar2</td><td>{smaller_select("covariate2", NULL)}</td></tr>'
      ))
    }
  }

  HTML(paste(table_str), "</table>")
}



  #),
#   tags$hr(),
#   checkboxInput('header', 'Header', TRUE),
#   radioButtons('sep', 'Separator',
#                c(Comma=',',
#                  Semicolon=';',
#                  Tab='\t'),
#                ','),
#   radioButtons('quote', 'Quote',
#                c(None='',
#                  'Double Quote'='"',
#                  'Single Quote'="'"),
#                '"'),
#   tags$hr(),
#   p('If you want a sample .csv or .tsv file to upload,',
#     'you can first download the sample',
#     a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
#     a(href = 'pressure.tsv', 'pressure.tsv'),
#     'files, and then try uploading them.'
#   )
# )
