#' Get the model name from a filename
#'
#' This function takes in a filename in .csv format and returns the model name. The function removes the ".csv" extension, finds the last occurrence of "-" in the filename, and removes all text preceding this last dash (including the dash itself).
#'
#' @param filename A character string representing the filename.
#' @return A character string representing the model name.
#' @examples
#' get_model_name("model-1.csv")
#' [1] "1"
#' get_model_name("results-model-2.csv")
#' [1] "2"
get_model_name <- function(filename) {
  # First, we remove the ".csv" extension from the filename.
  filename <- sub(".csv$", "", filename)

  # Next, we find the last occurrence of "-" in the filename.
  last_dash_index <- max(gregexpr("-", filename)[[1]])

  # Finally, we remove all text preceding this last dash (including the dash itself).
  result <- substr(filename, last_dash_index + 1, nchar(filename))

  return(result)
}
