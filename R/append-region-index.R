#' Extract Region Index from Spatial Object and Partition Training and Testing Data
#'
#' This function extracts region data for each location from a given shapefile. It also adds additional columns for day of week and training or testing data flag.
#'
#' @param train_data A data frame containing training data.
#' @param States Shapefile object containing state boundaries and regions.
#' @param train_end_date A Date object till which we want to keep the data for training purposes.
#'
#' @return Returns the modified train_data dataframe with new columns: day, trn_tst, and Region.
#'
#' @import dplyr
#'
#' @examples
#' append_region_index(train_df, states_sf, as.Date("2020-12-31"))
#'
#' @export
append_region_index <- function(train_data, polys, train_end_date) {
  library(dplyr)

  Regions_data <- polys@data

  idx <- match(train_data$location_name, Regions_data$name)

  train_data <- train_data %>%
    mutate(day = weekdays(date),
           trn_tst = if_else(date <= train_end_date, "train", "test"),
           Region = ifelse(is.na(idx), NA, Regions_data$Region[idx]))

  return(train_data)
}
