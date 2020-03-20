#' Services for specifying and accessing data

# Get the query string and use it to set up everything
# getURLState <- eventReactive(input$frame, {
#   query_string <- session$clientData$url_search
#
#   cat("Query string is:", query_string, "\n")
#   query_string
# })

n_size <- reactiveVal(50, label = "sample_n")

# Store the uploaded raw data
Uploaded_data <- reactiveVal(NULL)
# Store the current sample for future bootstrap draws
# This will be NA when resampling is OFF.
Saved_sample <- reactiveVal(NA)

# update available frames
observeEvent(input$package, {
  if (input$package  == "UPLOAD") {
    updateSelectInput(session, "frame", choices  = "uploaded_dataset")
    3 + 7
    8 * 9
  } else {
    available_frames <- get_package_frames(input$package)
    updateSelectInput(session, "frame",
                    choices = c("Select data frame:" = "", available_frames),
                    # Randomize the choice of a frame
                    selected = sample(available_frames, 1))
  }
})


# update response and explanatory variable choices
observeEvent(req(input$frame), {
  if (isTruthy(raw_data()))
    var_names <- names(raw_data())
  else
    var_names <- c("", "")


  randomly_selected <- sample(var_names, 2)
  updateSelectInput(session, "response",
                    choices = c("Select response var:" = "",
                                var_names),
                    selected = randomly_selected[1])
  # If there's only one variable, hide the explanatory chooser
  if (length(var_names) < 2) hide("explanatory") else show("explanatory")
  updateSelectInput(session, "explanatory",
                    choices = c("Select explanatory var:" = "",
                                var_names),
                    selected = randomly_selected[2])
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

observeEvent(input$uploaded_file, {
  inFile <- input$uploaded_file
  if (is.null(inFile))
    return(NULL)

  Sheet <- read.csv(inFile$datapath)#, header = input$header,
                    #sep = input$sep, quote = input$quote)
  Uploaded_data(Sheet)
})

data_upload_controls <- function() {
  shiny::tagList(
    shiny::fileInput('uploaded_file', 'Choose file to upload',
                     accept = c(
                       'text/csv',
                       'text/comma-separated-values',
                       'text/tab-separated-values',
                       'text/plain',
                       '.csv',
                       '.tsv'
                     )
    )
  )
}
observeEvent(req(input$package == "UPLOAD"), {
  showModal(
    modalDialog(title="Upload your own file.",
                data_upload_controls())

  )
})

raw_data <- reactive({
  req(input$frame) #  for dependency
  Uploaded_data() #  for dependency

  if (input$package == "UPLOAD") {
    req(Uploaded_data())
    return(Uploaded_data())
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
  res <- res[res %in% names(raw_data())]

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

# How many levels in each  variable
count_levels <- reactive({
  S <- current_sample()

  unlist(lapply(S,  function(x) {length(unique(x))}))
})

is_sample_plotable <- reactive({
  # They must all be > 1 to be plotable
  all(count_levels() > 1)
})

current_sample <- reactive({
  input$new_sample # for the dependency
  # Handle resampling  specially
  if (is.data.frame(Saved_sample())) {
    res <- dplyr::sample_n(Saved_sample(),
                           size  = n_size(), replace=TRUE )
    return(res)
  }

  if (!isTruthy(raw_data())) return(NA)
  the_variables <- current_variables()
  the_variables <- the_variables[the_variables %in% names(raw_data())]
  # the following is to  avoid legacy variable  names  from previous  dataset.
  req(the_variables) # will  be triggered if <the_variables> is empty



  Raw_data <- na.omit( raw_data()[the_variables] )
  if (n_size() == "All") {
    choose_n  <- nrow(Raw_data)
  } else {
     choose_n <-  min(nrow(Raw_data), as.numeric(n_size()))
  }
  Res <- dplyr::sample_n(Raw_data, size = choose_n)

  if (input$randomize)  Res[[1]] <- sample(Res[[1]])

  Res
})

observe({
  # n_size() # for the dependency
  #input$stratify # for the dependency
  # req(input$response)
  # req(input$explanatory
  # input$covariate
  # input$covariate2
  # input$frame

  if (input$resample) Saved_sample(isolate(current_sample()))
  else Saved_sample(NA)
})

# If response is multi-level categorical, show one level and
# and lump remaining into "Others". This is so that a simple model can be fitted.
with_dicotomous_response <- reactive({
  data <- current_sample()
  resp <- data[[1]]
  if (is.logical(resp)) data[[1]] <- as.numeric(data[[1]])
  else if (!is.numeric(resp) &&  !inherits(resp, "Date")) {
    if (length(unique(resp)) > 2) {
      data[[1]] <- forcats::fct_lump_n(
        data[[1]], n = 1,
        ties.method = "random")
    }
  }

  data
})

