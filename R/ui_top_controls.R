#' Create controls at top of app
#'
#' As a matter of design policy, each Little App has a row of controls
#' across the top of the window. These controls are always shown, irrespective
#' of which tab is showing. `ui_top_controls` is a function that creates the set of controls,
#' which then needs to be placed in an appropriate container such as a `miniUI::miniButtonBlock`.
#'
#' @param title character string. The name to be used for the button that brings
#' up the modal containing the app-specific controls
#' @param ... UI controls to be added on top.
#'
#' @export
ui_top_controls <- function(title = "App controls",  ...) {
    div(
    span(
      selectInput("sample_size", "", c("n&nbsp;=&nbsp;5" = 5, "n&nbsp;=&nbsp;10" = 10,
                                       "n&nbsp;=&nbsp;20" = 20, "n&nbsp;=&nbsp;50" = 50,
                                       "n&nbsp;=&nbsp;100" = 100, "n&nbsp;=&nbsp;200" = 200,
                                       "n&nbsp;=&nbsp;500" = 500,
                                       "n&nbsp;=&nbsp;1000" = 1000, "All" = "All"),
                  selected = 50),
      title = "set sample size"
    ),
    "&nbsp;&nbsp;&nbsp;",
    span(
      actionBttn(
        inputId = "new_sample",
        label = "",
        icon = icon("dice"),
        size= "md"),
      title  = "Draw a new sample of size n"
    ),
    "&nbsp;&nbsp;&nbsp;",
    span(
      actionBttn(
        inputId = "freeze",
        label = "", #"freeze",
        icon = icon("snowflake"),
        size = "md"),
      title = "Freeze the current plot"
    ),
    "&nbsp;&nbsp;&nbsp;",
    span(
      actionBttn(
        inputId = "show_model",
        label = "", #"model"
        icon = icon("signature"),
        size = "md"),
      title = title
    ),
    "&nbsp;&nbsp;&nbsp;",
    span(
      switchInput(
        inputId = "randomize",
        label = "shuffle",
        size = "mini",
        labelWidth = "60px",
        value = as.logical(FALSE)),
      title = "For Null Hypothesis testing. Use random shuffling to eliminate the relationship between the response and explanatory variables"
    ),
    span(
    switchInput(
      inputId = "resample",
      label = "resample",
      size = "mini",
      labelWidth = "60px",
      value = as.logical(FALSE)),
    title = "Resample from the current sample rather than generating a new sample"
    ),

    "&nbsp;&nbsp;&nbsp;",
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
