#' Get the names of all data sets in a package
#'
#' This is constructed from the internals of `data()`
#' It returns the names of the datasets identified in R's internal
#' compressed dataset database for the package
#'
#' @param name Name of the package as a character string
#' @param lib.loc Optional character string naming the library location

#' @export
package_data_names <- function(name,  lib.loc = NULL)
{
  path <- find.package(name, lib.loc)
  if (is.null(lib.loc))
    path <- c(path.package(name, TRUE), if (!length(name)) getwd(),
              path)
  path<- unique(normalizePath(path[file.exists(path)]))
  if (!dir.exists(file.path(path, "data"))) return(NULL)
  dataDir <- file.path(path, "data")

  names(readRDS(paste0(dataDir, "/Rdata.rdx"))$variables)
}
