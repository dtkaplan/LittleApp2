#' Services for specifying and accessing data

n_size <- reactiveVal(50, label = "sample_n")

# Store the uploaded raw data
Uploaded_data <- reactiveVal(NULL)
# Store the current sample for future bootstrap draws
# This will be NA when resampling is OFF.
Saved_sample <- reactiveVal(NA)
# Immediate access to the new frame name
Current_frame <- reactiveVal(NA)
observe({ Current_frame(input$frame) })
# Immediate access to the new variable names
Current_vars <- reactiveVal(NA)

# update Current_vars()
observe({
  vars <- isolate(Current_vars())
  vars[1] <- req(input$response)
  Current_vars(vars)
})
observe({
  vars <- isolate(Current_vars())
  vars[2] <- req(input$explanatory)
  Current_vars(vars)
})
observe({
   vars <- isolate(Current_vars())
   if (isTruthy(input$covariate)) {
     vars[3] <- input$covariate
   } else {
     vars <- vars[1:2]
   }
   Current_vars(vars)
   })
observe({
  vars <- isolate(Current_vars())
  if (isTruthy(input$covariate2)) {
    vars[4] <- input$covariate2
  } else {
    vars <- vars[1:min(3, length(vars))]
  }
  Current_vars(vars)
})

response_name <- reactive({Current_vars()[1]})
explanatory_name <- reactive({Current_vars()[2]})
covariate_name <- reactive({Current_vars()[3]})
covariate2_name <- reactive({Current_vars()[4]})

# update available frames
observeEvent(input$package, {
  if (input$package  == "UPLOAD") {
    updateSelectInput(session, "frame", choices  = "uploaded_dataset")
    Current_frame("uploaded_dataset") # update the state
  } else {
    available_frames <- package_data_names(input$package)
    new_frame <- if (input$package == "LittleApp2") "NHANES2" else sample(available_frames, 1)
    Current_frame(new_frame) # store it away for immediate use
    updateSelectInput(session, "frame",
                    choices = c("Select data frame:" = "", available_frames),
                    # Randomize the choice of a frame
                    selected = new_frame)
  }
}, ignoreNULL = TRUE)

observeEvent(Current_frame(), {
    req(isTruthy(raw_data()))
    var_names <- names(raw_data())

    randomly_selected <- sample(var_names, 2)
    Current_vars(randomly_selected) # store away for immediate access
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
                        choices = c("None" = "",
                                    var_names))
    }

    if('covariate2' %in% names(input)) {
      if (length(var_names) < 4) hide("covariate2") else show("covariate2")
      updateSelectInput(session, "covariate2",
                        choices = c("None" = "",
                                    var_names))
      }
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
                data_upload_controls(),
                footer = modalButton("Go back to the app ..."))

  )
})

raw_data <- reactive({
  req(Current_frame()) #  for dependency
  Uploaded_data() #  for dependency

  if (input$package == "UPLOAD") {
    inFile <- input$uploaded_file
    if (is.null(inFile)) return(NULL)

    Sheet <- read.csv(inFile$datapath)
    Uploaded_data(Sheet)
    var_names <- names(Sheet)

    randomly_selected <- sample(var_names, 2)
    Current_vars(randomly_selected) # store away for immediate access
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
    return(Sheet)
  }
  if (isTruthy(Current_frame())) {
    #db <- tools::Rd_db(input$package)
    #frames_in_packages <- gsub("\\.Rd", "", names(db))
    frames_in_packages <- package_data_names(input$package)
    req(Current_frame() %in% frames_in_packages)
    my_env = new.env() # where to put the data
    data(list = Current_frame(),  package = input$package, envir = my_env)

    the_data <- my_env[[Current_frame()]] # this is where data() puts the frame
  } else {
    NA
  }

  while (ncol(the_data) < 2)
    the_data$just_random_stuff <- runif(nrow(the_data))

  the_data
})


output$frame_preview <- renderText({
  req(current_sample())
  show_n <- min(50, nrow(current_sample()))
  paste(capture.output(head(data.frame(current_sample()), show_n)), collapse = "\n")
})

# Always put variables in order: response, explanatory, covar, covar2
current_variables <- reactive({
  res <- c(req(response_name()), explanatory_name(),
           covariate_name(), covariate2_name())
  res <- res[!is.na(res)]
  res <- res[!duplicated(res)] # kill off repeats

  res[res %in% names(raw_data())]
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
    unlist(lapply(current_sample(),  function(x) {length(unique(x))}))
})

is_sample_plotable <- reactive({
  # They must all be > 1 to be plotable
  if (exists("app_specific_plotable")) app_specific_plotable()
  else all(count_levels() > 1)
})

random_seed <- reactive({
  input$new_sample # for the dependency
  Sys.time()
  })

current_sample <- reactive({
  req(isTruthy(raw_data()))
  input$stratify # for the dependence

  # The resampling system hasn't been responding to new_sample
  # Handle resampling specially
  # if (is.data.frame(Saved_sample())) {
  #   res <- dplyr::sample_n(Saved_sample(),
  #                          size  = n_size(), replace=TRUE )
  #   return(res)
  # }

  the_variables <- current_variables()

  the_variables <-
    the_variables[the_variables %in% names(raw_data()),
                  drop = FALSE]
  # the following is to  avoid legacy variable  names  from previous  dataset.
  req(the_variables) # will  be triggered if <the_variables> is empty

  Raw_data <- na.omit( raw_data()[the_variables, drop = FALSE] )
  if (n_size() == "All") {
    choose_n  <- nrow(Raw_data)
  } else {
     # Return <nmax> rows for all groups that are big
     # enough and all the rows for groups that are smaller than nmax
     choose_n <-  min(nrow(Raw_data), as.numeric(n_size()))
  }

  set.seed(random_seed())
  if (input$stratify && !is.numeric(Raw_data[[explanatory_name()]])) {
    Res <- Raw_data %>%
      dplyr::group_by(!!as.name(explanatory_name())) %>%
      dplyr::mutate(.index.in.group = sample(row_number())) %>%
      dplyr::filter(.index.in.group <= choose_n) %>%
      dplyr::select(- .index.in.group) %>%
      dplyr::ungroup()
  } else {
    Res <- Raw_data[
      sample.int(nrow(Raw_data), as.integer(choose_n)),
      ,  # all columns
      drop = FALSE]
  }

  # Shuffle the response variable?
  if (input$randomize)  {
    Res$.orig.order. <- 1:nrow(Res) # keep track of the original
    random_order <-   sample(Res$.orig.order.)
    Res$.orig.order. <- Res$.orig.order.[random_order]
    Res[[1]] <- Res[[1]][random_order]
  }
  Res
})

#---------Set up to draw a resample---
# When resampling is pressed for the first time, save the current sample to
# the Saved_sample reactive value.
# When resampling is turned off, Saved_sample becomes NA
# which serves as a flag to get_current_sample()
observeEvent(input$resample, {
  if (input$resample) Saved_sample(isolate(current_sample()))
  else Saved_sample(NA)
})



