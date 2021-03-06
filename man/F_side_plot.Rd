% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/F_side_plot.R
\name{F_side_plot}
\alias{F_side_plot}
\title{Graphics for F  app}
\usage{
F_side_plot(raw, fitted, explan = "bogus", dflex = 1, sd = TRUE)
}
\arguments{
\item{raw}{the response variable as a vector}

\item{fitted}{the fitted model values as a vector}

\item{explan}{the explanatory variable as a vector}

\item{dflex}{integer, degrees of flexibility (which is n - (degfreedom+1))}

\item{sd}{if \code{TRUE}, show the interval of plus-or-minus one
standard deviation}
}
\description{
plot_values() creates the auxilliary graphic showing
the distribution of response and model values. Note
that covariates are not displayed even if they  have been
included in the model values. This simplifies the display.
}
\examples{
with(mosaicData::Galton,
F_side_plot(height, fitted(lm(height ~ father)),
  explan = father))
}
