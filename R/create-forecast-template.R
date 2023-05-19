#' Create a Forecast Template
#'
#' This function creates a forecast template for the given dataset where the output includes all unique locations from the input data and the specified forecast horizon.
#'
#' @param train_data Input data used to create the forecast template.
#'
#' @return Returns a data frame containing the forecast template.
#' The output has the following columns: \code{location}, \code{date}, \code{value}, \code{signal}, \code{day}, and \code{trn_tst}.
#' Each row of the output dataframe will contain a unique location from the input dataset, and dates ranging from \code{forecast_horiz_start} to \code{forecast_horiz_end}.
#' The values in the \code{value} column are left as NA.
#' The \code{signal} column is set to "hosp" indicating hospital data.
#' The \code{day} column specifies the weekday for the corresponding date.
#' The \code{trn_tst} column indicates whether the observation was part of the training or test dataset. For the forecast template, all rows will have "test".
#' The final output contains only distinct observations with respect to columns \code{date} and \code{location}.
#' Additionally, the dataset returned by this function contains an additional column named 'Region' joined from the original input data.
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#' # create sample data
#' date_data <- seq(as.Date("2021-01-01"), as.Date("2021-01-31"), by = "day")
#' loc_data <- c('loc1', 'loc2')
#' val_data <- c(1, 2)
#' hosp_data <- rep('hosp', length(date_data))
#' day_data <- weekdays(date_data)
#' trn_tst_data <- 'train'
#' data <- data.frame(date = rep(date_data, length(loc_data)),
#'                    location = rep(loc_data, each = length(date_data)),
#'                    value = rep(val_data, each = length(date_data)),
#'                    signal = hosp_data,
#'                    day = rep(day_data, length(loc_data)),
#'                    trn_tst = trn_tst_data)
#'
#' create_forecast_template(data, as.Date("2021-02-01"), as.Date("2021-02-10"))
#'
#' @importFrom dplyr select distinct left_join
#' @importFrom lubridate as_date weekdays
#' @export
create_forecast_template <- function(train_data) {

  if(class(forecast_horiz_start) != "Date"){
    cli_abort("Run setup_analysis() to designate key analysis dates before proceeding")}

  unique_loc <- unique(train_data$location)
  len_loc <- length(unique_loc)

  date <- as_date(seq(forecast_horiz_start, forecast_horiz_end, 1))
  len_for <- length(date)

  location <- rep(unique_loc, each = len_for)
  value <- rep(NA_real_, len_loc * len_for)
  signal <- rep("hosp", len_loc * len_for)
  day <- weekdays(as_date(date))
  trn_tst <- rep("test", len_loc * len_for)

  forecast_template <- data.frame(
    location = location,
    date = date,
    value = value,
    signal = signal,
    day = day,
    trn_tst = trn_tst
  )

  get_Region_table <- train_data %>% select(location_name, location, Region) %>% distinct()
  forecast_template <- left_join(forecast_template, get_Region_table, by = "location")

  train_data <- rbind(train_data, forecast_template)
  train_data <- train_data %>%
    distinct(date, location, .keep_all = TRUE)

  return(train_data)
}
