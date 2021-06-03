#' Main graphic for center/spread app
#'
#' Makes the standard graphic for the Center & Spread app. The response variable
#' is expected to be quantitative, the explanatory variable categorical.
#'
#' @param formula specifying the response and explanatory variables. It is assumed that the
#' response variable is quantitative. If it's a 0/1 variable, the labels can be specified.
#' @param data the data frame containing the variables in the formula.
#' @param yrange the vertical range of the graphic (optional)
#' @param labels For a 0/1 response variable, categorical labels (if any) to be used for the 0 and 1
#' @param annots a list with elements
#' values.
#' @export
center_spread_graph <- function(formula, data, yrange = NULL, labels = NULL,
                                annots = list(prob_level=0.95,
                                              show_median=TRUE,
                                              show_summary_interval = TRUE,
                                              show_mean = TRUE,
                                              show_sd = TRUE,
                                              show_ci = TRUE,
                                              show_violin = TRUE)
                                              ) {
  yvar <- formula[[2]]
  xvar <- formula[[3]]
  if (xvar == "1") {
    data <- data %>% select(!!yvar) %>%
      mutate(no_explan_variable = " ")
    xrange <- c(0, 2)
    xvar <- "no_explan_variable"
    formula[[3]] <- as.name(xvar)
    mod_formula <- as.formula(paste(yvar, "~ 1"))
  } else {
    mod_formula = formula
  }

  jitter_height <-
    if (length(unique(data[[yvar]])) > 2) 0
    else 0.1 # If it's a zero/one variable, still jitter it

  Stats <- mosaicCore::df_stats(formula, data = data,
                                mean = mean, mean = mosaicCore::ci.mean(!!annots$prob_level),
                                median = median, sd = sd,
                                summary = coverage(!!annots$prob_level),
                                n = length(),
                                na.action = "na.pass")
  P <- gf_jitter(formula, data = data, seed = 101,
               width = 0.10, height = jitter_height,
               alpha = point_alpha(nrow(data)))

  if (annots$show_median) {
    this_formula <- as.formula(glue::glue("median + median ~ {xvar}"))
    P <- P %>% gf_errorbar(this_formula, data = Stats,
                           inherit = FALSE, color = "blue", size = 2)
  }
  if (annots$show_summary_interval) {
    this_formula <-
      as.formula(glue::glue("summary_lower + summary_upper ~ {xvar}"))
    P <- P %>% gf_errorbar(this_formula, data = Stats,
                           inherit = FALSE, color = "gray", alpha = 0.5,  size = 4)
  }
  if (annots$show_sd & nrow(data) > 1) {
    For_sd_ruler <-
      mosaicCore::df_stats(formula, data = data,
                           mean = mean, sd = sd) %>%
      mutate(center = mean, `pos_sd` = mean + sd, neg_sd =  mean - sd,
             pos_2sd = mean + 2*sd, neg_2sd =  mean - 2*sd,) %>%
      select( - sd,  - center)
    For_sd_ruler_labels <- For_sd_ruler %>%
      tidyr::gather(key = label, value = vertical, `pos_sd`, neg_sd, pos_2sd, neg_2sd, mean) %>%
      mutate(label = gsub("pos_", "+", label)) %>%
      mutate(label = gsub("neg_", "–", label))

    this_formula <- as.formula(glue::glue("`pos_sd` + neg_sd ~ {xvar}"))
    P <- P %>% gf_errorbar(this_formula, data = For_sd_ruler,
                           inherit = FALSE, color = "red", width = 0.1)
    this_formula <- as.formula(glue::glue("pos_2sd + mean ~ {xvar}"))
    P <- P %>% gf_errorbar(this_formula, data = For_sd_ruler,
                           inherit = FALSE, color = "red", width = 0.15)
    this_formula <- as.formula(glue::glue("neg_2sd + mean ~ {xvar}"))
    P <- P %>% gf_errorbar(this_formula, data = For_sd_ruler,
                           inherit = FALSE, color = "red", width = 0.15)
    this_formula <- as.formula(glue::glue("vertical ~ {xvar}"))
    P <- P %>% gf_text(this_formula, label = ~ label,
                       inherit = FALSE, color = "red",
                       data = For_sd_ruler_labels, nudge_x = 0.1, hjust = 0)
  }
  if (annots$show_mean) {
    this_formula <- as.formula(glue::glue("mean + mean ~ {xvar}"))
    P <- P %>% gf_errorbar(this_formula, data = Stats,
                           inherit = FALSE, color = "black", size = 2)
  }
  if (annots$show_ci && nrow(data) > 1) {
    this_formula <- as.formula(glue::glue("mean_lower + mean_upper ~ {xvar}"))
    P <- P %>% gf_errorbar(this_formula, data = Stats,
                           inherit = FALSE, color = "black", size = 1, width = 0.8)
  }

  if (annots$show_violin & nrow(data) > 1)
    P <- P %>% gf_violin(alpha = 0.2, fill = "blue")


  if (!is.null(labels)) {
    # format the  y  scale nicely  for a dichotomous
    # variable
    # and
    lims <- c(-.1, 1.1)
  }  else {
    lims <- if (is.null(yrange)) range(data[[yvar]]) else yrange
  }

  #  make sure that +- 2 sd shows up on the graph
  if (annots$show_sd) {
    lims[1] <- min(lims[1], min(For_sd_ruler$neg_2sd))
    lims[2] <- max(lims[2], max(For_sd_ruler$pos_2sd))
  }

  if ( !is.null(labels)) {
    P <- P %>%
      gf_refine(
        scale_y_continuous(
          yvar, breaks  =  c(0, .25, .5, .75, 1),
          labels = c(labels[1], .25,  .5, .75, labels[2]),
          limits = lims))
  } else if (!is.null(yrange)) {
    # put the y scale on a common footing for all samples
    P <- P %>% gf_lims(y  = lims)
  }

  mod <- lm(mod_formula, data = data)
  mod_stats <- list(fitted = sd(fitted(mod), na.rm = TRUE),
                    resid  = sd(resid(mod), na.rm = TRUE),
                    model = mod,
                    stat_table = Stats)

return(list(P = P, S = mod_stats))
}
