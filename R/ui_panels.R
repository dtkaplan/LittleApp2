#' panels for the UI
#'
#' Functions to make tabs for the UI: `graph_panel()`,  `compare_panel()`, `stats_panel()`,
#' `debug_panel()`
#'
#' @export
graph_panel <- function() {
  miniTabPanel(
    "Graph", icon = icon("chart-bar"),
    miniContentPanel(
      checkboxInput("side_display", label = "Show auxiliary graph", value = FALSE),
      textOutput("big_plot_comment_top"),
      plotOutput("big_plot", width="100%",
                 brush = brushOpts(id = "main_ruler",
                                   delayType = "debounce",
                                   resetOnNew = TRUE)),
      textOutput("big_plot_comment_bottom"),
      downloadButton("save_plot",  "Download graphic")
    )
  )

}
#' @export
compare_panel <- function() {
  miniTabPanel(
    "Compare", icon = icon("book-open"),
      radioGroupButtons("compare_what", "Show what?",
                        c("main plot" = "main", "aux. plot" = "aux", "both" = "both")),
      side_by_side_table(
        current = plotOutput("compare_plot1", brush = brushOpts(id = "comp_ruler")),
        frozen  = plotOutput("compare_plot2", brush = brushOpts(id = "comp_ruler"))
    )
  )
}
#' @export
stats_panel <- function() {
  miniTabPanel(
    "Stats", icon = icon("binoculars"),
    miniContentPanel(
      side_by_side_table(
        current = htmlOutput("current_stats"),
        frozen = htmlOutput("frozen_stats")
      )))
}

#' @export
codebook_panel <- function() {
  miniTabPanel(
    "Codebook", icon = icon("user-secret"),
    miniContentPanel(
      htmlOutput("codebook")))
}
#' @export
debug_panel <- function() {
  miniTabPanel(
    "Debug", icon = icon("car"), # bug
    p("output$debug1 rendered here:"),
    verbatimTextOutput("debug1"),
    p("output$debug2 rendered here:"),
    verbatimTextOutput("debug2")
  )
}

