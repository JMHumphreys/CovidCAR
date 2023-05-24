#' Generate a temporal index for a given set of dates
#'
#' This function returns the date from \code{y} that is the closest to each date in \code{x}.
#'
#' @param x A date vector, typically a column in a dataframe indicating date of observed case or incidence.
#' @param y A vector of dates to search for matches, typically a customized series with specified interval length.
#' @return A vector of dates from \code{y} that are nearest matches for dates given in \code{x}
#
#' @export
time_index = function(x, y) {
  nx = length(x)
  ind = numeric(nx)
  for(i in seq_len(nx)){
    ind[i] = which.min(abs(x[i] - y))
  }
  y[ind]
}
