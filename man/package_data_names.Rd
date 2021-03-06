% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package_data_names.R
\name{package_data_names}
\alias{package_data_names}
\title{Get the names of all data sets in a package}
\usage{
package_data_names(name, lib.loc = NULL)
}
\arguments{
\item{name}{Name of the package as a character string}

\item{lib.loc}{Optional character string naming the library location}
}
\description{
This is constructed from the internals of \code{data()}
It returns the names of the datasets identified in R's internal
compressed dataset database for the package
}
