#' Create controls at top of app
#'
#' As a matter of design policy, each Little App has a row of controls
#' across the top of the window. These controls are always shown, irrespective
#' of which tab is showing. `ui_top_controls` is a function that creates the set of controls,
#' which then needs to be placed in an appropriate container such as a `miniUI::miniButtonBlock`.
#'
#' @export
ui_top_controls <- function() {
  tagList(
    span(
      selectInput("sample_size", "", c("n = 5" = 5, "n = 10"  = 10,
                                       "n = 20" = 20, "n = 50" = 50,
                                       "n  = 100" = 100, "n = 200" = 200, "n = 500" = 500,
                                       "n = 1000" = 1000, "All" = "All"),
                  selected = 50),
      title = "set sample size"
    ),
    "   ",
    span(
      actionBttn(
        inputId = "new_sample",
        label = "",
        icon = icon("dice"),
        size= "md"),
      title  = "Draw a new sample of size n"
    ),
    "   ",
    span(
      actionBttn(
        inputId = "freeze",
        label = "", #"freeze",
        icon = icon("snowflake"),
        size = "md"),
      title = "Freeze the current plot"
    ),
    "   ",
    span(
      actionBttn(
        inputId = "show_model",
        label = "", #"model"
        icon = icon("signature"),
        size = "md"),
      title = "Control model shape"
    ),
    "   ",
    span(
      actionBttn(
        inputId = "show_annotations",
        label = "", #"annotations"
        icon = icon("i-cursor"),
        size = "md"),
      title = "Show statistical annotations"
    ),
    "   ",
    span(
      switchInput(
        inputId = "randomize",
        label = "randomize response",
        size = "mini",
        labelWidth = "80px",
        value = as.logical(FALSE)),
      title = "For Null Hypothesis testing. Use random shuffling to eliminate the relationship between the response and explanatory variables"
    ),
    "   ",
    span(
      actionBttn(
        inputId = "bookmark",
        label = "", #"bookmark"
        icon = icon("bookmark"),
        size = "md"),
      title = "Bookmark state of app"
    )
  )
}
