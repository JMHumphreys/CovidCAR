#' Get API key from secrets.yaml file
#'
#' This function retrieves an API key by reading a secrets.yaml file stored in the current working directory. The specified 'api_name' is used to retrieve the corresponding API key.
#'
#' @param api_name A character string representing the name of the API key to be retrieved from secrets.yaml file.
#'
#' @return A character vector containing the value of the requested API key.
#'
#' @import yaml read_yaml
#'
#' @examples
#' #Read API token for Twitter API from secrets.yaml
#' get_api("Twitter_API")
#'
#' #Read API token for OpenWeather API from secrets.yaml
#' get_api("OpenWeather_API")
#'
#' @export
get_api <- function(api_name){
  yaml::read_yaml(file.path(getwd(), "secrets.yaml"),
                  readLines.warn = FALSE)[[paste(api_name)]]
}
