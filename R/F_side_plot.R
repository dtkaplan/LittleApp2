#' Graphics for F  app
#'
#' plot_values() creates the auxilliary graphic showing
#' the distribution of response and model values. Note
#' that covariates are not displayed even if they  have been
#' included in the model values. This simplifies the display.
#'
#' @param raw the response variable as a vector
#' @param fitted the fitted model values as a vector
#' @param explan the explanatory variable as a vector
#' @param dflex integer, degrees of flexibility (which is n - (degfreedom+1))
#' @param sd if `TRUE`, show the interval of plus-or-minus one
#' standard deviation
#' @examples
#' with(mosaicData::Galton,
#' F_side_plot(height, fitted(lm(height ~ father)),
#'   explan = father))
#' @import ggplot2
#' @import ggformula
#' @import dplyr
#' @import shiny
#' @import shinyWidgets
#' @importFrom shinyjs hide show
#'
#' @export
F_side_plot <- function(raw, fitted,
                        explan = "bogus",
                        dflex = 1,  sd = TRUE) {

  Pts <- data.frame(raw = raw, model = fitted) %>%
    tidyr::gather(key = source,  value = v) %>%
    mutate(xpos = source)
  if (is.numeric(explan)) {
    explan <- explan  -  median(explan)
    explan <- ((explan ) /
                 diff(range(explan))) / 2
    Pts  <- Pts %>%
      mutate(xpos = c(explan + 2,  explan + 1))
    P <-
      gf_jitter(v ~ source,  data = Pts,  alpha = 0) %>%
      gf_jitter(v ~ xpos, data = Pts, width = 0.1,
                           color =  ~  source,  alpha  = 0.2)
  } else {
    P <- gf_jitter(v ~ source, data  = Pts,
                              color = ~ source, alpha  = .2,
                              width = 0.4, height = 0)
  }

  Stats <-  mosaicCore::df_stats(v ~ source, data = Pts,
                                 m = mean, var = var)  %>%
    mutate(high  =  m  + sqrt(var), low = m - sqrt(var))
  R2val <- Stats$var[1] /  Stats$var[2]

  if (sd)
    P <- P %>%
    gf_errorbar(high + low ~ source, data = Stats,
                           inherit  = FALSE) %>%
    gf_point(m ~ source, data = Stats, color = "black",
                        inherit = FALSE)

  P  <-
    P %>% gf_labs(x = "", y = "") %>%
    gf_theme(axis.ticks.x = element_blank(),
                        axis.text.y = element_blank(),
                        legend.pos = "none") %>%
    gf_refine(scale_colour_manual(
      values = c("blue", "black")))
  return(list(P = P, stats =
                list(R2=R2val, n=length(raw), dflex=dflex,
                     var_raw = Stats$var[2], var_model = Stats$var[1])))

}
