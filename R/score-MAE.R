#' Calculate Mean Absolute Error given forecast and truth data
#'
#' This function calculates the Mean Absolute Error (MAE) for a set of forecasts and corresponding true values.
#'
#' @param forecast_data A dataframe, path or list object containing forecast information
#' @param truth A dataframe containing true values to compare forecasts against
#' @param ingest Character value indicating how forecast data is being passed.
#' Options are "dataframe", "path", or "list". Default is "dataframe".
#' @param missing Character value indicating how to handle missing or NaN values in the truth data.
#' Options are "remove" or "zero". If "remove", any data points with missing true values will be removed from calculation.
#' If "zero", any data points with missing true values will be assumed to have a true value of 0. Default is "remove".
#'
#' @return Returns a data frame with columns for Model, MAE, MAPE, and Rank, sorted by ascending order of MAE.
#' @export
#'
#' @examples
#' score_MAE(forecast_data = my_forecasts, truth = my_truth, ingest = "dataframe", missing = "remove")
#'
score_MAE <- function(forecast_data, truth, ingest = c("dataframe", "path", "list"), missing = c("remove","zero")) {

  if (ingest == "path" & is.character(forecast_data)) {
    fnames <- list.files(forecast_data, pattern='csv', full.names = TRUE, recursive = TRUE)

    tmp_df <- lapply(fnames, function(file) {
      read.csv(file) %>%
        mutate(prediction = value,
               model = get_model_name(file),
               date = as.Date(target_end_date),
               forecast_date = as.Date(forecast_date),
               location_name = location) %>%
        filter(type == "point") %>%
        select(model, date, location_name, forecast_date, prediction)
    }) %>% do.call("rbind", .)
  }

  if (ingest == "list" & is.list(forecast_data)) {
    tmp_df <- lapply(forecast_data, function(file) {
      read.csv(file) %>%
        mutate(prediction = value,
               model = get_model_name(file),
               date = as.Date(target_end_date),
               forecast_date = as.Date(forecast_date),
               location_name = location) %>%
        filter(type == "point") %>%
        select(model, date, location_name, forecast_date, prediction)
    }) %>% do.call("rbind", .)
  }

  if (ingest == "dataframe" & is.data.frame(forecast_data)) {
    stopifnot(length(forecast_data$model_name) != 0)

    tmp_df <- forecast_data %>%
      mutate(prediction = value,
             model = model_name,
             date = as.Date(target_end_date),
             forecast_date = as.Date(forecast_date),
             location_name = location) %>%
      filter(type == "point") %>%
      select(model, date, location_name, forecast_date, prediction)
  }

  #match to truth
  truth <- truth %>%
    mutate(true_value = value,
           location_name = location) %>%
    select(date, location_name, true_value)

  tmp_df <- left_join(tmp_df, truth, by=c("date","location_name"))

  no_obs <- length(which(is.na(tmp_df$true_value)))
  if(no_obs > 0 & missing == "remove"){
    cli_alert(paste0("A total of ", paste0(no_obs), " forecasts weren't evaluatd due lack of truth data"))
    tmp_df <- tmp_df %>% filter(is.na(true_value) == FALSE)
  }

  if(no_obs > 0 & missing == "zero"){
    cli_alert(paste0("A total of ", paste0(no_obs), "forecasts lacked truth data and were filled with (zero)"))
    tmp_df$true_value[is.na(tmp_df$true_value)] = 0
  }

  #evaluate models
  # Split the dataframe into subsets based on the values in the subset column
  subsets <- split(tmp_df, tmp_df[["model"]])

  # Calculate MAE and MAPE for each subset
  output_list <- lapply(subsets, function(subset) {

    efore <- subset[["true_value"]] - subset[["prediction"]]
    mae <- round(mean(abs(efore)),1)
    mape <- round(mean(abs(efore / sum(subset[["true_value"]]))) * 100,5)

    result <- data.frame(model=unique(subset$model), MAE=mae, MAPE=mape)
    return(result)

  })

  # Combine the output list into a single dataframe
  output_df <- do.call(rbind, output_list)
  rownames(output_df) <- 1:nrow(output_df)
  output_df = arrange(output_df, MAE)
  output_df$maeRank = 1:nrow(output_df)

  # Return the final dataframe
  return(output_df)

}
