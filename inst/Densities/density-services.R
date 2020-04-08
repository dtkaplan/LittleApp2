# For the Points-and-density app

app_title <- reactive({"Points and Densities"})

# for plotting with ribbon
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




  fractions <- numeric(length(breaks) - 1)
  for (k in 1:length(fractions)) {
    fractions[k] <- 100 * mean(x >= breaks[k] & x < breaks[k+1])
  }
  positions <- (positions[1:length(fractions)] + positions[2:length(positions)])/2

  list(pts = Pts,
       density_scale = desired_area / current_area,
       labels = data.frame(where = positions,
                           what = paste0(signif(fractions,3), "%"),
                           region = 1:length(positions))
       )
}

#-----------------------
# Input widgets
#-----------------------

Common <- reactiveValues(
  density_mode = "density", # or violin
  show_normal = FALSE,
  show_population  = FALSE,
  extra_alpha = 1.0,
  ordinary_x = c(0, 0),
  far_left = -1,
  far_right = 1,
  bandwidth = 1
)

main_calculation <- reactive({
  Current_vars(Current_vars()[1]) # kill the explanatory var.
  data <- current_sample()

  data$.y <- 1
  plot_formula <- as.formula(glue(".y ~ {response_name()}"))
  jitter_width <- if (is.numeric(data[[1]])) 0 else 0.2
  P <- gf_jitter(plot_formula, data = data,
                 width = jitter_width, height = 0.2,
                 alpha = point_alpha(nrow(data)))

  adjust <- if (is.numeric(data[[1]])) 1 else 0.25
  show_half <- "top"
  ribbon <- hviolin(as.numeric(data[[1]]), half = show_half,
                    scale = 10, adjust = adjust,
                    center = 1)
  pop_values <- na.omit(as.numeric(raw_data()[[response_name()]]))
  population_ribbon <- hviolin(pop_values, half = show_half,
                               scale = 10, adjust = 2 * adjust,
                               center = 1)
  P <- P %>%
    gf_polygon(.y ~ .x, data = ribbon$pts,
               alpha = 0.3, fill = ~ region,
               inherit = FALSE) %>%
    # gf_polygon(.y ~ .x, data = population_ribbon$pts,
    #            alpha = 0.5, fill = NA, color = "blue",
    #            inherit = FALSE) %>%
    gf_theme(legend.position="none",
             axis.text.x = element_text(angle = 30, hjust = 1)) %>%
    gf_text(0.5 ~ + where, data = ribbon$labels,
            label  = ~ what, color ~ as.factor(region)) +
    coord_cartesian(ylim = c(0,2))
    #gf_lims(y = c(0,2))

  return(list(main=P, side = P, stats=list(a=1, b=2)))
})
