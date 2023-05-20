score_WIS <- function(forecast_data, truth, ingest = c("dataframe", "path", "list")) {

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
      truth_data <- truth_data %>%
        mutate(true_value = value,
               location_name = location) %>%
        select(date, location_name, true_value)

      tmp_df <- left_join(tmp_df, truth_data, by=c("date","location_name"))

      no_obs <- length(which(is.na(tmp_df$true_value)))
      if(no_obs > 0){
        cli_alert(paste0("A total of ", paste0(no_obs), " predictions weren't evalauted due lack of truth data"))
      }

       tmp_df <- tmp_df %>% filter(is.na(true_value) == FALSE)

       #score models
       scores_out <- tmp_df %>%
         scoringutils::score() %>%
         scoringutils::summarize_scores(by = c("model", "date", "location_name", "forecast_date")) %>%
         dplyr::select(model, date, location_name, forecast_date, WIS = interval_score) %>%
         as.data.frame()

      return(scores_out)

}
