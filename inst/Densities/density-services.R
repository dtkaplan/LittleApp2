# For the Points-and-density app

app_title <- reactive({"Points and Densities"})

# for plotting the density and violin broken into region
hviolin <- function(x, adjust = 1, breaks = mean(x, na.rm=TRUE), center = 1,
                    scale = 3,
                    half = c("both", "top", "bottom")) {
  half <- match.arg(half)
  pts <- density(x, adjust = adjust)

  Pts <- tibble::tibble(.x = pts$x, .y = pts$y, raw = pts$y)
  spread <- diff(range(Pts$.x, na.rm = TRUE))
  # scale the y values to give an area suited to the graphics frame
  current_area <- mean(Pts$.y) * spread
  desired_area <- spread / scale
  Pts$.y <- Pts$.y * desired_area / current_area

  # figure out the colors
  color_vals <- numeric(length = nrow(Pts))
  breaks_colors <- c(breaks, Inf)
  for (k in length(breaks):1) {
    color_vals[Pts$.x < breaks_colors[k]] <- k
  }
  Pts$region <- as.factor(color_vals)
  # Find the center position in  x for each region
  spacing <- diff(range(Pts$.x)) /  (nrow(Pts) -1)
  Percent_labels <- Pts %>%
    group_by(region) %>%
    summarize(where = sum(.x * raw) / sum(raw),
              what = 100 * sum(raw) * spacing) %>%
    mutate(what = paste0(round(what,  1), "%"))

  Upside_down <- Pts[nrow(Pts):1, ] # in reverse order

  # show the bottom of the drawn shape
  if (half == "top") Upside_down$.y <- 0
  else if(half == "both") Upside_down$.y <- - Upside_down$.y
  else if(half == "bottom") {
    Upside_down$.y <- 0
    Pts$.y <- - Pts$.y
  }
  else {
    stop("Invalid value for argument <half>. Contact developer.")
  }

  Pts = dplyr::bind_rows(Pts, Upside_down)
  Pts$.y <- Pts$.y + center

  # Figure out where the fraction labels go
  # Get the fraction between each pair of breaks
  positions <- extendrange(range(x, na.rm = TRUE))
  positions <- c(positions[1], breaks, positions[2])
  breaks <- c(-Inf, breaks, Inf) # add another Inf to the left side

  # fractions <- numeric(length(breaks) - 1)
  # for (k in 1:length(fractions)) {
  #   fractions[k] <- 100 * mean(x >= breaks[k] & x < breaks[k+1])
  # }
  #positions <- (positions[1:length(fractions)] + positions[2:length(positions)])/2

  list(pts = Pts,
       density_scale = desired_area / current_area,
       labels = Percent_labels)
}

#-----------------------
# Input widgets
#-----------------------

Annots <- reactiveValues(
  density_mode = "density", # or violin
  show_normal = FALSE,
  show_population  = FALSE,
  extra_alpha = 1.0, # not being used
  ordinary_x = c(0, 0),
  far_left = -1,
  far_right = 1,
  bandwidth = "default",
  orientation  = "On X-axis",
  center_point = "mean",
  x_breaks = numeric(5)
)

# initialize the divisions
observe({
  response_name()
  isolate({
    data <- current_sample()
    x <- as.numeric(data[[1]])
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    breaks <- c(m + c(-2, -1, 0, 1, 2) * s)
    Annots$x_breaks <<- breaks
  })
})

observeEvent(Annots$center_point, {
  data <- current_sample()
  x <- as.numeric(data[[1]])
  breaks <- Annots$x_breaks
  if (Annots$center_point == "median") breaks[3]  = median(x, na.rm = TRUE)
  else breaks[3] = mean(x, na.rm = TRUE)
  Annots$x_breaks <<- breaks
})


observeEvent(input$choose_breaks, {
  cat("Controls ruler\n")
  # Find the one of the current breaks closest to an edge of the brush
  breaks <- Annots$x_breaks[c(1,2,4,5)] # leave the middle one in the center
  break_spread <- diff(range(breaks)) / 50 # definition of "close"
  if (Annots$orientation == "On X-axis") {
    lower_side <- input$choose_breaks$xmin
    upper_side <- input$choose_breaks$xmax
  } else {
    lower_side <- input$choose_breaks$ymin
    upper_side <- input$choose_breaks$ymax
  }

  left_dists <- abs(breaks - lower_side)
  right_dists <- abs(breaks - upper_side)
  # Make sure you're pretty close to an existing break before you move it.
  if (min(left_dists) > break_spread && min(right_dists) > break_spread) {
    # Not close enough to identify clearly which line to move
    return()
  }
  left_smallest <- which.min(left_dists)
  right_smallest <- which.min(right_dists)
  if (left_dists[left_smallest] < right_dists[right_smallest]) {
    breaks[left_smallest] <- upper_side
  } else {
    breaks[right_smallest] <- lower_side
  }

  Annots$x_breaks <<- c(breaks[1:2], Annots$x_breaks[3], breaks[3:4]) # put the middle one back
})

observeEvent(input$main_ruler, {
  cat("Main ruler\n")
  # Find the one of the current breaks closest to an edge of the brush
  breaks <- Annots$x_breaks[c(1,2,4,5)] # leave the middle one in the center



  break_spread <- diff(range(breaks)) / 50 # definition of "close"
  if (Annots$orientation == "On X-axis") {
    lower_side <- input$main_ruler$xmin
    upper_side <- input$main_ruler$xmax
  } else {
    lower_side <- input$main_ruler$ymin
    upper_side <- input$main_ruler$ymax
  }


  # This is duplicated for the Controls modal graphic. Break it out into  a function.
  left_dists <- abs(breaks - lower_side)
  right_dists <- abs(breaks - upper_side)

  # Make sure you're pretty close to an existing break before you move it.
  if (min(left_dists) > break_spread && min(right_dists) > break_spread) {
    # Not close enough to identify clearly which line to move
    return()
  }
  left_smallest <- which.min(left_dists)
  right_smallest <- which.min(right_dists)
  if (left_dists[left_smallest] < right_dists[right_smallest]) {
    breaks[left_smallest] <- upper_side
  } else {
    breaks[right_smallest] <- lower_side
  }

  breaks <- c(breaks[1:2], Annots$x_breaks[3], breaks[3:4]) # put the middle one back
  # This  is  where the logic of the function  would end.


  Annots$x_breaks <<- breaks
  })

observeEvent(input$density_mode, {Annots$density_mode <- input$density_mode})
observeEvent(input$extra_alpha, {Annots$extra_alpha <- input$extra_alpha})
observeEvent(input$show_normal, {Annots$show_normal <- input$show_normal})
observeEvent(input$center_point, {Annots$center_point <- input$center_point})
observeEvent(input$bandwidth, {Annots$bandwidth <- input$bandwidth})
observeEvent(input$show_population, {Annots$show_population <- input$show_population})
observeEvent(input$orientation, {Annots$orientation <- input$orientation})

observeEvent(input$show_app_params, {
  showModal(
    modalDialog(
      title = paste("Controls for", app_title(), "Little App"), easyClose  = TRUE,
      plotOutput("select_breaks",
                 brush = brushOpts(id  = "choose_breaks",

                                   direction = "xy",
                                   resetOnNew = TRUE)),
      p("Density display:"),
      tags$ul(
        tags$li(p("One or two sided display of density"),
                radioGroupButtons("density_mode",
                                  choices = c("density", "violin"),
                                  selected = Annots$density_mode),
                tags$br()),
        tags$li(p("Smoothing: relative size of region in which the density is calculated."),
                radioGroupButtons("bandwidth",
                                  choices = c("tiny", "small", "default",  "large", "very large"),
                                  selected = Annots$bandwidth),
                tags$br()),
        tags$li(p("Compare this sample to the whole 'population'."),
                checkboxInput("show_population", "Show population", Annots$show_population)),
        tags$li(p("Compare to a normal distribution with  the same mean and variance."),
                checkboxInput("show_normal", "Show normal dist:", Annots$show_normal)),
        tags$li(p("How the 'center' of the distribution  is shown."),
                radioGroupButtons("center_point", choices = c("mean", "median"),
                                  selected = Annots$center_point))
      ),
      tags$hr(),
      tags$ul(
        p("Most Little Apps have both a response and explanatory variable. Here, there
          is just one variable being displayed. This variable could be shown along
          the X axis or along the Y axis."),
        tags$li(radioGroupButtons("orientation",
                                  choices = c("On Y-axis", "On X-axis"),
                                  selected = Annots$orientation))
      )

    )
  )
})

# Draw the plot in the modal control menu
output$select_breaks <- renderPlot({
  main_calculation()$main
})

# Set sensible limits for the x-axis

x_limits <- reactive({
  raw_x <- raw_data()[[response_name()]]
  if (is.numeric(raw_x)) {
    extendrange(
      range(raw_x, na.rm=TRUE))
  } else {
    # Extend categorical variables by a substantial amount
    xlimits <- range(as.numeric(raw_x)) + c(-1, 1)
  }
})

main_calculation <- reactive({
  Current_vars(Current_vars()[1]) # kill the explanatory var.
  data <- current_sample()

  data$.y <- 1

  plot_formula <- as.formula(glue(".y ~ {response_name()}"))
  jitter_width <- if (is.numeric(data[[1]])) 0 else 0.2
  set.seed(101)
  P <- gf_jitter(plot_formula, data = data,
                 width = jitter_width, height = 0.2,
                 alpha = point_alpha(nrow(data)),
                 seed)

  adjust <- if (is.numeric(data[[1]])) 1 else 0.25
  show_half <- switch(Annots$density_mode,
                      density =  "top",
                      violin = "both")
  extra_adjust <- switch(Annots$bandwidth,
                         "tiny" = 0.2,
                         "small" = 0.5,
                         "default" = 1,
                         "large" = 2,
                         "very large" = 5)
  ribbon <- hviolin(as.numeric(data[[1]]), half = show_half,
                    scale = 10, adjust = adjust * extra_adjust,
                    breaks = sort(Annots$x_breaks),
                    center = 1.3)
  pop_values <- na.omit(as.numeric(raw_data()[[response_name()]]))

  population_ribbon <- hviolin(pop_values, half = show_half,
                               scale = 10, adjust = adjust * extra_adjust,
                               center = 1)
  P <- P %>%
    gf_polygon(.y ~ .x, data = ribbon$pts,
               alpha = 0.3, fill = ~ region,
               inherit = FALSE) %>%
    # gf_polygon(.y ~ .x, data = population_ribbon$pts,
    #            alpha = 0.5, fill = NA, color = "blue",
    #            inherit = FALSE) %>%
    gf_theme(legend.position="none") %>%
    gf_label(0.7 ~ + where, data = ribbon$labels,  alpha = 0.3,
            label  = ~ what, fill =  ~ region, inherit = FALSE, size=3.5) +
    coord_cartesian(ylim = c(0.5, 2),
                    xlim = x_limits(),
                    expand = FALSE)

  P <- P %>%
    gf_vline(xintercept = Annots$x_breaks)

  if (Annots$orientation == "On Y-axis") {
    P <- P + coord_flip()
    P <- P %>% gf_theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y = element_text(angle = 30, hjust = 1))
  } else {
    P <- P %>% gf_theme(
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1))


  }

  return(list(main=P, side = P, stats=list(a=1, b=2)))
})

