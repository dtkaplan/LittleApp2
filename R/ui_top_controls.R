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
    uiOutput("next_to_new_sample", inline = TRUE), # A placeholder
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
        icon = icon("bars"), #"signature"),
        size = "md"),
      title = title
    ),
    "  ",
    span(
      awesomeCheckbox(
        inputId = "randomize",
        label = "shuffle",
        value = as.logical(FALSE)),
      title = "Random shuffle to eliminate the relationship between the response and explanatory variables"
    ),
    # "  ",
    # span(
    # checkboxInput(
    #   inputId = "resample",
    #   label = "resample",
    #   #size = "mini",
    #   #labelWidth = "60px",
    #   value = as.logical(FALSE)),
    # title = "Resample from the current sample rather than generating a new sample"
    # ),
    "  ",
    span(
      actionBttn("show_explain",
                 label = NULL,
                 style = "simple",
                 color = "primary",
                 size  = "xs",
                 icon = icon("info")
      ),title = "Explain basic app controls."),
    "  ",
    span(
      actionBttn("show_metadata",
                 label = NULL,
                 style = "simple",
                 color = "primary",
                 size = "xs",
                 icon = icon("eye")  #icon("user-secret")
      ), title = "Show codebook."),

    "  ",
    span(
      actionBttn(
        inputId = "bookmark",
        label = "", #"bookmark"
        icon = icon("bookmark"),
        size = "xs"),
      title = "Bookmark/Download data sample"
    ),
    uiOutput("far_right", inline = TRUE), # A placeholder
  )
}
