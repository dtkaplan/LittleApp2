#' Services for specifying and accessing data


observe({})

# update available frames
observeEvent(req(input$package), {
  available_frames <- get_package_frames(input$package)
  updateSelectInput(session, "frame",
                    choices = c("Select data frame:" = "", available_frames))
})


# update response and explanatory variable choices
observe({
  req(input$package) # for dependency
  req(input$frame) # for dependency
  if (isTruthy(raw_data()))
    var_names <- names(raw_data())
  else
    var_names <- character(1)

  updateSelectInput(session, "response",
                    choices = c("Select response var:" = "",
                                var_names))
  # If there's only one variable, hide the explanatory chooser
  if (length(var_names) < 2) hide("explanatory") else show("explanatory")
  updateSelectInput(session, "explanatory",
                    choices = c("Select explanatory var:" = "",
                                var_names))
  # These aren't being hidden when called for. I don't know why.
  if ('covariate' %in% names(input)) {
    if (length(var_names) < 3) hide("covariate") else show("covariate")
    updateSelectInput(session, "covariate",
                      choices = c("Select covariate:" = "",
                                  var_names))

    if('covariate2' %in% names(input)) {
      if (length(var_names) < 4) hide("covariate2") else show("covariate2")
      updateSelectInput(session, "covariate2",
                        choices = c("Select second covar:" = "",
                                    var_names))
    }
  }
})


raw_data <- eventReactive(req(input$frame), {
  if (input$package == "UPLOAD") {
    stop("Haven't implemented uploading data yet.")
  }
  if (isTruthy(input$frame)) {
    my_env = new.env() # where to put the data
    data(list = input$frame,  package = input$package, envir = my_env)

    my_env[[input$frame]] # this is where data() puts the frame
  } else {
    NA
  }
})


output$frame_preview <- renderText({
  req(current_sample())
  show_n <- min(50, nrow(current_sample()))
  paste(capture.output(head(data.frame(current_sample()), show_n)), collapse = "\n")
})

# Always put variables in order: response, explanatory, covar, covar2
current_variables <- reactive({
  res <- req(input$response)

  if (isTruthy(input$explanatory)) res <-  c(res, input$explanatory)
  else return(res)

  if (isTruthy(input$covariate)) res <-  c(res, input$covariate)
  else return(res)

  if (isTruthy(input$covariate2)) res <-  c(res, input$covariate2)


  res[!duplicated(res)] # kill off repeats

})

frozen_sample <- eventReactive(input$freeze, {
  current_sample()
})

observeEvent(input$freeze, {
  frozen_calculation() # Just to make sure the reactive gets run
})

# The reactive does the work, but it won't necessarily get called until
# the compare tab comes up. The above observer takes care  of that.
frozen_calculation <- eventReactive(input$freeze, {
  main_calculation()
})

current_sample <- reactive({
  if (!isTruthy(raw_data())) return(NA)
  the_variables <- current_variables()


  input$new_sample # for the dependency
  input$sample_size
  input$stratify

  Raw_data <- na.omit( raw_data()[the_variables] )
  if (input$sample_size == "All") {
    choose_n  <- nrow(Raw_data)
  } else {
     choose_n <-  min(nrow(Raw_data), as.numeric(input$sample_size))
  }
  Res <- dplyr::sample_n(Raw_data, size = choose_n)

  if (input$randomize)  Res[[1]] <- sample(Res[[1]])

  Res
})

# If response is multi-level categorical, show one level and
# and lump remaining into "Others". This is so that a simple model can be fitted.
with_dicotomous_response <- reactive({
  data <- current_sample()
  resp <- data[[1]]
  if (is.logical(resp)) data[[1]] <- as.numeric(data[[1]])
  else if (!is.numeric(resp)) {
    if (length(unique(resp)) > 2) {
      data[[1]] <- forcats::fct_lump(data[[1]], n = 1)
    }
  }

  data
})

