#' Score WIS function
#'
#' This function scores the weighted interval scoring (WIS) for a given forecast data against the truth.
#'
#' @param forecast_data A dataframe,directory path,or list object providing forecast data with columns including: model, date, location_name, forecast_date, quantile, and value.
#' Can also be a path or a list of paths to csv files.
#' @param truth A dataframe containing the true values to compare the forecast data against, with columns including: date, location_name, and value.
#' @param ingest A character vector specifying the type of input for 'forecast_data', which can be either "dataframe", "path", or "list".
#' @param missing A character vector specifying the method to handle missing true values in the scoring process. It can be either "remove" or "zero".
#'
#' @return Returns a dataframe with scores for each model, date, location, and forecast_date combination based on the WIS metric.
#'
#' @examples
#' # score the WIS for some example forecast and truth dataframes
#' library(scoringutils)
#' example_forecast <- data.frame(model = c("model1","model2"), date = as.Date(c("2020-06-01","2020-06-02")),
#' location_name = c("location1","location2"), forecast_date = as.Date(c("2020-06-03","2020-06-04")),
#' quantile = 0.5, value = c(10,20))
#' example_truth <- data.frame(date = as.Date(c("2020-06-03","2020-06-04")), location_name = c("location1","location2"), value = c(8,22))
#' score_WIS(example_forecast, example_truth, "dataframe", "remove")
score_WIS <- function(forecast_data, truth, ingest = c("dataframe", "path", "list"), missing = c("remove","zero")) {

  if (ingest == "path" & is.character(forecast_data)) {
    fnames <- list.files(forecast_data, pattern='csv', full.names = TRUE, recursive = TRUE)

    tmp_df <- lapply(fnames, function(file) {
      read.csv(file) %>%
        mutate(prediction = value,
               model = get_model_name(file),
               date = as.Date(target_end_date),
               forecast_date = as.Date(forecast_date),
               location_name = location) %>%
        filter(type == "quantile") %>%
        select(model, date, location_name, forecast_date, quantile, prediction)
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
        filter(type == "quantile") %>%
        select(model, date, location_name, forecast_date, quantile, prediction)
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
      filter(type == "quantile") %>%
      select(model, date, location_name, forecast_date, quantile, prediction)
  }

      #match to truth
      truth_data <- truth %>%
        mutate(true_value = value,
               location_name = location) %>%
        select(date, location_name, true_value)

      tmp_df <- left_join(tmp_df, truth_data, by=c("date","location_name"))

      no_obs <- length(which(is.na(tmp_df$true_value)))
      if(no_obs > 0 & missing == "remove"){
        cli_alert(paste0("A total of ", paste0(no_obs), " predictions weren't evalauted due lack of truth data"))
        tmp_df <- tmp_df %>% filter(is.na(true_value) == FALSE)
      }

      if(no_obs > 0 & missing == "zero"){
        cli_alert(paste0("A total of ", paste0(no_obs), " predictions lacked truth data and were filled with (zero)"))
        tmp_df$true_value[is.na(tmp_df$true_value)] = 0
      }

       #score models
       scores_out <- tmp_df %>%
         scoringutils::score() %>%
         scoringutils::summarize_scores(by = c("model", "date", "location_name", "forecast_date")) %>%
         dplyr::select(model, date, location_name, forecast_date, WIS = interval_score) %>%
         as.data.frame()

      return(scores_out)

}
