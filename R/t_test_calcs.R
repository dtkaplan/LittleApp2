#' Two-sample t-test plot
#' NEED TO WRITE FORMAT_STATS !!!!!!!!!!
#' @export
t_test_calcs <-  function(formula, data, level = 0.95,
                               show_mean = TRUE, show_ci = TRUE,
                               show_t = TRUE, var_equal = TRUE,
                               y_range = NULL) {
  if (rlang::f_rhs(formula) == 1) {
    return(
      one_sample_t_plot(formula  = formula, data  = data, level = level,
                                     show_mean = show_mean, show_ci = show_ci,
                                     null_hypothesis = 0, y_range = y_range)
    )
  }
  var_y <- as.character(formula[[2]])
  if (is.null(y_range)) y_range = range(as.numeric(data[[var_y]]), na.rm = TRUE)

  color_formula <- formula[c(1,3)] # one-sided formula

  if (!is.numeric(data[[2]])) {
    data[[2]]  <- forcats::fct_lump(data[[2]], n = 1)
    # turn it into  two  levels because this is a t-test
  }
  if (!is.numeric(data[[1]])) {
    data[[1]] <- as.numeric(data[[1]])
  }

  P <-
    LA_dot_layer(formula = formula, data = data,
                 color = color_formula, width =  0.15, height = 0,
                 alpha = point_alpha(nrow(data)), seed = 101) %>%
    gf_theme(legend.position = "none")
  Stats <-
    df_stats(formula, data = data,  mn = mean,
             ci = ci.mean(level = !!level)) %>%
    mutate(xpos = c(1.25, 1.75))


  if (show_mean) {
    P <- do.call(gf_errorbar, list(P, mn + mn ~ xpos,
                                   data = Stats,
                                   color = color_formula,
                                   width = 0.25,
                                   size = 2,
                                   inherit = FALSE,
                                   show.legend = FALSE))
  }


  if (show_ci) {
    P <- do.call(gf_errorbar, list(P,
                                   ci_lower + ci_upper ~ xpos,
                                   data = Stats,
                                   color = color_formula,
                                   width = 0.13,
                                   size = 1.5,
                                   inherit = FALSE,
                                   show.legend = FALSE))
  }

  tmp <- stats::t.test(formula, data = data,
                       var.equal = var_equal, conf.level = level)

  if (show_t) {
    left_mean <- tmp$estimate[2]
    res <- left_mean + tmp$conf.int

    res <- as.list(res)
    names(res) <- c("low", "high")
    res$left_mean <- left_mean
    res$midpoint <- 1.5
    res$right_of_midpoint <- 1.6
    res$p_label <- nice_p(tmp$p.value, 3)

    T_stats <- as.data.frame(res) %>%
      mutate(low = pmax(low, y_range[1]),
             high = pmin(high, y_range[2]))
    P <- P %>%
      gf_errorbar(low + high ~ midpoint, data = T_stats,
                  width = .3, show.legend = FALSE,
                  inherit = FALSE) %>%
      gf_text(high ~ right_of_midpoint, color = "black", label = ~ p_label,
              data = T_stats, vjust = 0)
  }


  # As much as possible, keep all samples on same scale,
  #  but display whole of confidence interval
  total_range <- range(y_range, c(min(Stats$ci_lower), max(Stats$ci_upper)))
  P <- P %>% gf_lims(y = total_range)

  side <- ggplot(mtcars, aes(x=1,y=1)) + geom_text(label="No supplemental\n plot for t-test.")
  list(main = P, side = side, stats = tmp)
}

#' One-sample t test plot
#'
one_sample_t_plot <- function (formula, data, level = 0.95,
                               show_mean = TRUE, show_ci = TRUE,
                               null_hypothesis = 0, y_range = NULL) {
  var_y <- as.character(formula[[2]])
  if (is.null(y_range)) y_range = range(as.numeric(data[[1]]), na.rm = TRUE)


  P <-
    LA_dot_layer(formula = formula, data = data, color = "black", width =  0.15, height = 0,
                 alpha = point_alpha(nrow(data)), seed = 101) %>%
    gf_theme(legend.position = "none")

  if (!is.numeric(data[[1]])) data[[1]]  <- as.numeric(data[[1]])

  Stats <-
    df_stats(formula, data = data,  mn = mean,
             ci = ci.mean(level = !!level)) %>%
    mutate(xpos = c(1.5))

  if (show_mean) {
    P <- gf_errorbar(P, mn + mn ~ xpos,
                     data = Stats,
                     color = "blue",
                     width = 0.25,
                     size = 2,
                     inherit = FALSE,
                     show.legend = FALSE)
  }

  tmp <- stats::t.test(data[[var_y]], mu = null_hypothesis,
                       conf.level = level)

  if (show_ci) {
    res <- data.frame(conf.int = tmp$conf.int)

    res$top <- max(tmp$conf.int)
    res$midpoint <- 1.75
    res$p_label <- nice_p(tmp$p.value, 3)

    T_stats <- as.data.frame(res)

    P <-
      P <- do.call(gf_errorbar, list(P,
                                     ci_lower + ci_upper ~ xpos,
                                     data = Stats,
                                     color = "blue",
                                     width = 0.13,
                                     size = 1.5,
                                     inherit = FALSE,
                                     show.legend = FALSE)) %>%
      gf_hline(yintercept = null_hypothesis) %>%
      gf_text(top ~ midpoint, color = "black", label = ~ p_label,
              data = T_stats, vjust = 0)
  }
  # As much as possible, keep all samples on same scale,
  #  but display whole of confidence interval
  total_range <- range(c(y_range, min(Stats$ci_lower), max(Stats$ci_upper)))
  side <- ggplot(mtcars, aes(x=1,y=1)) + geom_text(label="No supplemental\n plot for t-test.")

  list(main = P %>% gf_lims(y = total_range), side = side, stats = tmp)

}

LA_dot_layer <- function(formula, data, color, width, height, alpha = 1, seed = 101) {
  P <- gf_jitter(formula,
                 data = data,
                 color = color,
                 width = width,
                 height = height,
                 alpha = alpha,
                 seed = seed
  ) %>% gf_theme(legend.position = "top")

  if (rlang::f_rhs(formula) == 1)
    P <- P %>% gf_lims(x = c(0, 2)) %>% gf_theme(no_x_axis)

  P
}

no_x_axis <- ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                            axis.text.x = ggplot2::element_blank(),
                            axis.ticks.x = ggplot2::element_blank())

nice_p <- function(p, n=2) {
  order <- floor(log10(p))
  if (order < -4) return("p value < 0.0001")
  first_two_digits <- round(p * 10^-(order-(n-1)))
  paste0("p value: 0.", paste0(rep("0", -(order+1)), collapse=""), first_two_digits)
}
