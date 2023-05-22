#' Convert Date String to Date Object
#'
#' Takes a date string in YYYY-MM-DD format and returns a date object, or a null replacement value if input is null.
#'
#' @param x Date string in YYYY-MM-DD format
#' @param null_replacement_value Value to return if input is null
#' @return A date object representing the input string
#' @export cast_date_string from Covid19Forecasts package (private)
#' @examples
#' cast_date_string("2022-03-02", "No date")
#' cast_date_string(NULL, "No date")
#exported from Covid19Forecasts package (private)
#used in get_covid19_obs()
cast_date_string <- function(x, null_replacement_value) {
  if (is.null(x))
    return(null_replacement_value)
  newx <- ymd(x, quiet = TRUE)
  if (is.na(newx)) cli_abort("Date {.val {x}} failed to parse as YYYY-MM-DD")
  as.character(newx)
}

