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
    span(actionBttn(inputId="n_select","n=50", size = "sm"),
         title = "Sets sample size.\n\nIf selected sample size is bigger than\nthe number of rows in the data frame,\nthis shows the number of rows."),
    "  ",
    span(
      actionBttn(
        inputId = "new_sample",
        label = "",
        icon = icon("dice"),
        size= "md"),
      title  = "Draw a new sample of size n"
    ),
    "  ",
    span(
      actionBttn(
        inputId = "freeze",
        label = "", #"freeze",
        icon = icon("snowflake"),
        size = "md"),
      title = "Freeze the current plot"
    ),
    "  ",
    span(
      actionBttn(
        inputId = "show_app_params",
        label = "", #"model"
        icon = icon("signature"),
        size = "md"),
      title = title
    ),
    "  ",
    span(
      switchInput(
        inputId = "randomize",
        label = "shuffle",
        size = "mini",
        labelWidth = "60px",
        value = as.logical(FALSE)),
      title = "For Null Hypothesis testing. Use random shuffling to eliminate the relationship between the response and explanatory variables"
    ),
    "  ",
    span(
    checkboxInput(
      inputId = "resample",
      label = "resample",
      #size = "mini",
      #labelWidth = "60px",
      value = as.logical(FALSE)),
    title = "Resample from the current sample rather than generating a new sample"
    ),

    "  ",
    span(
      actionBttn(
        inputId = "bookmark",
        label = "", #"bookmark"
        icon = icon("bookmark"),
        size = "xs"),
      title = "Bookmark state of app"
    )
  )
}
