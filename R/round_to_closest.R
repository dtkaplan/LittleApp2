#' "Round" a vector of numerical values to the closest element of a
#' set of discrete  values.
#' @param x the vector of values
#' @param discrete a discrete set of numerical values
#' @export
round_to_closest <- function(x, discrete) {
  discrete <- sort(discrete)
  distance <-  outer(x, discrete,
                     function(x,y) abs(x-y))
  best_inds <- rep(1, length(x))
  for (k in 2:length(discrete)) {
    best_inds[distance[, k] <  distance[ , k-1] ] <- k
  }

  discrete[best_inds]
}
