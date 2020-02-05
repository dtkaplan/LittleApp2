# reactive services for the outputs such as plots



output$big_plot <- renderPlot({
  if (input$side_display) {
    gridExtra::grid.arrange(main_calculation()$main,
                            main_calculation()$side,
                            nrow = 1,
                            widths = c(3,1))
  } else {
    main_calculation()$main
  }

})

output$compare_plot1 <- renderPlot({
  if (input$compare_what == "data plot") main_calculation()$main
  else if (input$compare_what == "model values") main_calculation()$side
  else {
    gridExtra::grid.arrange(main_calculation()$main,
                            main_calculation()$side,
                            nrow = 1,
                            widths = c(3,1))
  }
})

output$compare_plot2 <- renderPlot({
  if (input$freeze == 0 ) {
    ggformula::gf_label(1 ~ 1, label="No model frozen yet!")
  } else {
    if (input$compare_what == "data plot") frozen_calculation()$main
    else if (input$compare_what == "model values") frozen_calculation()$side
    else {
      gridExtra::grid.arrange(frozen_calculation()$main,
                              frozen_calculation()$side,
                              nrow = 1,
                              widths = c(3,1))
    }
  }
})

# The stats outputs
output$current_stats <- renderText({
  current_stats()
})

output$frozen_stats <- renderText({
  frozen_stats()
})

# The codebook

#' Services for the codebook
#'
output$codebook <- renderText({
  req(input$package)
  req(input$frame)
  if (input$package == "UPLOAD") {
    # special treatment
    return(HTML(p("Sorry, but uploaded files don't have documentation available here.)")))
  }
  components <- unlist(strsplit(input$frame, "::"))
  package <- input$package
  data_name <- input$frame
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
    HTML(p("Sorry, but there's no documentation for this frame."))
  }
})

output$preview_plot <- renderPlot({main_calculation()$main})

# Display facts about brushed area in the plot with the ruler

observeEvent(input$main_ruler, {
  req(current_sample())
  with(input$main_ruler, {
    yinfo <-
      glue::glue("{input$response}-axis: {signif(ymin,3)} to {signif(ymax, 3)} giving  ∆ = {signif(ymax - ymin, 3)}\n\n")

    if(input$explanatory != "none_selected" &&
       is.numeric(raw_data()[[input$explanatory]])) {
      slope <- (ymax - ymin) / (xmax - xmin)
      xinfo <- glue::glue("{input$explanatory}-axis: {signif(xmin, 3)} to {signif(xmax, 3)} giving  ∆ = {signif(xmax - xmin, 3)}\n\n")
      slope_info <- glue::glue("Slope of box diagonals: ± {signif(slope, 3)}")
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
      glue::glue("{input$response}-axis: {signif(ymin,3)} to {signif(ymax, 3)} giving  ∆ = {signif(ymax - ymin, 3)}\n\n")

    if(input$explanatory != "none_selected" &&
       is.numeric(raw_data()[[input$explanatory]])) {
      slope <- (ymax - ymin) / (xmax - xmin)
      xinfo <- glue::glue("{input$explanatory}-axis: {signif(xmin, 3)} to {signif(xmax, 3)} giving  ∆ = {signif(xmax - xmin, 3)}\n\n")
      slope_info <- glue::glue("Slope of box diagonals: ± {signif(slope, 3)}")
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

