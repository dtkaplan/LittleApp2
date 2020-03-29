# reactive services for the outputs such as plots

#-----------
# Display outputs (non-modal)

output$big_plot <- renderPlot({
  res <- try(main_calculation())
  if (inherits(res, "tryError")) return(NULL)

  if (input$side_display && !is.null(res$side)) {
    gridExtra::grid.arrange(res$main,
                            res$side,
                            nrow = 1,
                            widths = c(3,1))
  } else {
    res$main
  }

})


output$compare_plot1 <- renderPlot({
  construct_compare_plot(
    main_calculation(),
    input$compare_what)
})

output$compare_plot2 <- renderPlot({
  if (input$freeze == 0 ) {
    ggformula::gf_label(1 ~ 1, label="No model frozen yet!")
  } else {
    construct_compare_plot(
      frozen_calculation(),
      input$compare_what)
  }
})


# The stats outputs
output$current_stats <- renderText({
  res <- try(main_calculation())
  if (inherits(res, "tryError")) {
    return(
      html("Not enough variation in this sample. See data tab.")
    )
  }
  res <- res$stats
  if (inherits(res,  "html")) {
    res  #  it's an  error  message
  } else {
    format_stats(res) # an app-specific function
  }
})

output$frozen_stats <- renderText({
  res <- frozen_calculation()$stats
  if (inherits(res,  "html")) {
    res  #  it's an  error  message
  } else {
    format_stats(res) # an app-specific function
  }
})

output$save_plot = downloadHandler(
  filename = 'test.png',
  content = function(file) {
    device <- function(..., width = 4, height = 3) {
      grDevices::png(..., width = width, height = height,
                     res = 300, units = "in")
    }
    ggsave(file, plot = main_calculation()$main, device = device)
  })

# The codebook

#' Services for the codebook
#'
output$codebook <- renderText({
  req(input$package)
  req(Current_frame())
  if (input$package == "UPLOAD") {
    # special treatment
    return(HTML(p("Sorry, but uploaded files don't have documentation available here.)")))
  }
  # Values of input$frame are package namespaced, e.g. "mosaicData::CPS85"
  components <- unlist(strsplit(Current_frame(), "::"))
  package <- input$package
  data_name <- Current_frame()
  db <- tools::Rd_db(package)
  this_name <- paste0(data_name, ".Rd")
  if (this_name %in% names(db) ) {
    doc_contents <- db[[this_name]]
    HTML(
      paste(
        capture.output(tools::Rd2HTML(doc_contents)),
        collapse  = "\n")
    )
  } else {
    "Sorry, but there's no documentation for this frame."
  }
})

output$preview_plot <- renderPlot({
  res <- try(main_calculation())
  if (inherits(res, "tryError") || !is.list(res)) NULL
  else res$main
})

#------------
# Display facts about brushed area in the plot with the ruler

observeEvent(input$main_ruler, {
  req(current_sample())
  with(input$main_ruler, {
    yinfo <-
      glue("{response_name()}-axis: {signif(ymin,3)} to {signif(ymax, 3)} giving  ∆ = {signif(ymax - ymin, 3)}\n\n")

    if(explanatory_name() != "none_selected" &&
       is.numeric(raw_data()[[explanatory_name()]])) {
      slope <- (ymax - ymin) / (xmax - xmin)
      xinfo <- glue("{explanatory_name()}-axis: {signif(xmin, 3)} to {signif(xmax, 3)} giving  ∆ = {signif(xmax - xmin, 3)}\n\n")
      slope_info <- glue("Slope of box diagonals: ± {signif(slope, 3)}")
    } else {
      xinfo <- slope_info <- ""
    }

    if (input$side_display) {
      yinfo <- "Turn off side display to see readouts from ruler."
      xinfo <- ""
      slope_info <- ""
    }
    showModal(
      modalDialog(p(yinfo), p(xinfo), p(slope_info),
                  title = "Ruler info.", easyClose  = TRUE)
    )
  })
})

# Same thing, but for compare plot
observeEvent(input$comp_ruler, {
  req(current_sample())
  with(input$comp_ruler, {
    yinfo <-
      glue("{response_name}-axis: {signif(ymin,3)} to {signif(ymax, 3)} giving  ∆ = {signif(ymax - ymin, 3)}\n\n")

    if(explanatory_name() != "none_selected" &&
       is.numeric(raw_data()[[explanatory_name()]])) {
      slope <- (ymax - ymin) / (xmax - xmin)
      xinfo <- glue("{explanatory_name()}-axis: {signif(xmin, 3)} to {signif(xmax, 3)} giving  ∆ = {signif(xmax - xmin, 3)}\n\n")
      slope_info <- glue("Slope of box diagonals: ± {signif(slope, 3)}")
    } else {
      xinfo <- slope_info <- ""
    }

    if (input$compare_what != "data plot") {
      yinfo <- "Turn off side display to see readouts from ruler."
      xinfo <- ""
      slope_info <- ""
    }
    showModal(
      modalDialog(p(yinfo), p(xinfo), p(slope_info),
                  title = "Ruler info.", easyClose  = TRUE)
    )
  })
})

## For the documentation
observeEvent(input$show_explain, {
  showModal(
    modalDialog(htmlOutput("explain_text"),
                title  = "Explaining the App ...",  easyClose = TRUE)
  )
},
  ignoreNULL = FALSE
)

## For the codebook
observeEvent(input$show_metadata, {
  showModal(
    modalDialog(htmlOutput("codebook"),
                title  = "Codebook",  easyClose = TRUE)
  )
}
)





#-------
#  SAMPLE SIZE LOGIC

# When the modal for setting sample size is called
# then input$sample_size will exist and have a value.
# We can use that value to update the sample size.

observeEvent(input$sample_size, {
  if (n_size() != input$sample_size) n_size(input$sample_size)
})

observeEvent(nrow(current_sample()), {
  updateActionButton(
    session,  "n_select",
    label = paste0("n=",
                   nrow(current_sample())))
})

## For the sample size
observeEvent(input$n_select,  {
  showModal(
    modalDialog(
      radioGroupButtons("sample_size", "Set sample size",
                        c("n = 5" = 5, "n = 10" = 10,
                          "n = 20" = 20, "n = 50" = 50,
                          "n = 100" = 100, "n = 200" = 200,
                          "n = 500" = 500,
                          "n = 1000" = 1000, "All" = "All"),
                        selected = n_size(), 1))
  )
})

#-------------
# Handle the bookmark

observeEvent(input$bookmark, {
  state <-  list(
    package = input$package,
    frame = Current_frame(),
    n_size = n_size(),
    response  = response_name(),
    explanatory = explanatory_name()
    )
  if ("covariate" %in% names(input)){
    state$covariate = covariate_name()
  }
  if ("covariate2" %in% names(input)){
    state$covariate2 = covariate2_name()
  }
  state <- jsonlite::toJSON(state)

  #cat(names(session$clientData))
  myURL <-
    paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname, ":",
      session$clientData$url_port,
      session$clientData$url_pathname,
      "?state=", state)

  showModal(
    modalDialog(
      p("Use this URL to restore data..."),
      p( myURL),
      p("use url_search")
    )
  )
})

somethings_wrong_with_data <- reactive({
  showModal(
    modalDialog(p("This sample doesn't have enough variation. It might be that there is just one level of the explanatory variable in the sample or that one of the levels has only one row."),
                p("Go to the data tab to see the raw data in the sample."),
                br(),
                p("Debugging info ..."),
                p(glue::glue("package: {input$package}")),
                p(glue::glue("dataframe: {Current_frame()}")),
                p(glue::glue("response variable: {response_name()}")),
                p(glue::glue("explanatory variable: {explanatory_name()}")),
                p(glue::glue("sample size: {n_size()}")),
                title = "Something's wrong :-(", easyClose  = TRUE)
  )
})
