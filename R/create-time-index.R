#' Generate a temporal index for a given set of dates
#'
#' This function returns the date from \code{y} that is the closest to each date in \code{x}.
#'
#' @param x A date vector, typically a column in a dataframe indicating date of observed case or incidence.
#' @param y A vector of dates to search for matches, typically a customized series with specified interval length.
#' @return A vector of dates from \code{y} that are nearest matches for dates given in \code{x}
#' @examples
#' Recode observed_dates to the biweekly intervals (14-day time periods).
#' observed_dates <- seq(as_date("2021-07-23"), as_date("2021-09-23"), by =1 )
#' MyResult <- time_index(observed_dates, seq(min(observed_dates), max(observed_dates), by = "14 days"))
#' MyResult
#'  [1] "2021-07-23" "2021-07-23" "2021-07-23" "2021-07-23" "2021-07-23" "2021-07-23" "2021-07-23" "2021-07-23" "2021-08-06" "2021-08-06" "2021-08-06"
#'  [12] "2021-08-06" "2021-08-06" "2021-08-06" "2021-08-06" "2021-08-06" "2021-08-06" "2021-08-06" "2021-08-06" "2021-08-06" "2021-08-06" "2021-08-06"
#'  [23] "2021-08-20" "2021-08-20" "2021-08-20" "2021-08-20" "2021-08-20" "2021-08-20" "2021-08-20" "2021-08-20" "2021-08-20" "2021-08-20" "2021-08-20"
#'  [34] "2021-08-20" "2021-08-20" "2021-08-20" "2021-09-03" "2021-09-03" "2021-09-03" "2021-09-03" "2021-09-03" "2021-09-03" "2021-09-03" "2021-09-03"
#'  [45] "2021-09-03" "2021-09-03" "2021-09-03" "2021-09-03" "2021-09-03" "2021-09-03" "2021-09-17" "2021-09-17" "2021-09-17" "2021-09-17" "2021-09-17"
#'  [56] "2021-09-17" "2021-09-17" "2021-09-17" "2021-09-17" "2021-09-17" "2021-09-17" "2021-09-17" "2021-09-17"
#' @export
time_index = function(x, y) {
  nx = length(x)
  ind = numeric(nx)
  for(i in seq_len(nx)){
    ind[i] = which.min(abs(x[i] - y))
  }
  y[ind]
}
