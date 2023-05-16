#' This function creates a forecast template using the input training data and specific dates for forecasting.

#' @param train_data A data frame containing the training data with columns for location, date, and value
#' @param forecast_horiz_start The starting date for the forecast horizon in yyyy-mm-dd format as a character string
#' @param forecast_horiz_end The ending date for the forecast horizon in yyyy-mm-dd format as a character string
#' @return A data frame containing the forecast template with columns for location, date, value, signal, day, trn_tst, and region_table
#' @import dplyr
create_forecast_template <- function(train_data, forecast_horiz_start, forecast_horiz_end){

  #Convert the forecast horizon into a character vector
  forecast_horiz_vector <- as.character(seq(forecast_horiz_start, forecast_horiz_end, 1))

  #Find the unique locations from the input training data
  unique_loc <- unique(train_data$location)

  #Calculate the length of the unique locations and the forecast horizon
  len_loc <- length(unique_loc)
  len_for <- length(forecast_horiz_vector)

  #Create vectors for location, date, value, signal, day, and trn_tst
  location <- rep(unique_loc, len_for)
  date <- rep(forecast_horiz_vector, len_loc)
  value <- rep(NA, len_loclen_for)
  signal <- rep("hosp", len_loclen_for)
  day <- weekdays(as.Date(date))
  trn_tst <- rep("test", len_loc*len_for)

  #Combine the vectors into a data frame to create the forecast template
  forecast_template <- data.frame(location, date, value, signal, day, trn_tst)

  #Extract the region table from the input training data
  get_Region_table <- train_data[,c(1:3)] %>% distinct()

  #Merge the region table with the forecast template based on location
  forecast_template <- left_join(forecast_template, get_Region_table, by="location")

  #Return the completed forecast template
  return(forecast_template)
}
