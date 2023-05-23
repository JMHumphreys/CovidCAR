#' Get COVID-19 forecasts from various sources
#'
#' This function allows you to obtain COVID-19 forecasts based on selected data source. The data can come from covidHubUtils, cache or test depending on preference.
#'
#' @param source A character vector to specify where the forecast data should be obtained from (default is 'covidHubUtils'). Accepted values include "covidHubUtils" for fetching data online, "cache" for reading saved data from local cache, and "test" for loading test data locally.
#' @param models A character vector of models to be included in the query.
#'        Default is NULL which includes COVIDhub-trained_ensemble, COVIDhub-ensemble, and COVIDhub-baseline.
#' @param write_copy A logical value indicating if a copy of the downloaded data should be written to the analysis directory. Default is TRUE.
#'
#' @return Returns a dataframe containing COVID-19 forecasts with columns for model name, forecast date, location, target end date, type, quantile, and value.
#'
#' @import cli_alert
#' @importFrom methods as
#' @importFrom readr read_delim
#' @importFrom jsonlite toJSON
#' @importFrom arrow open_dataset schema mutate filter collect
#' @importFrom dplyr select across case_when
#' @importFrom lubridate ymd as_date
#'
#' @examples
#' #get Hub forecasts using default arguments:
#' get_hub_forecasts()
#'
#' #get Hub forecasts specifying model(s):
#' get_hub_forecasts(models = c("COVIDhub-ensemble"))
#'
#' #read saved data from cache:
#' get_hub_forecasts(source = "cache")
#'
#' #load test data:
#' get_hub_forecasts(source = "test")
#'
#' @export
get_hub_forecasts <- function(source = c("covidHubUtils","cache","test"),
                              models = NULL,
                              write_copy = TRUE) {

  if(source == "cache" & class(su_yaml$local_cache_dir) != "character"){
    cli_abort("Where's the cache? Change 'source=' or run setup_analysis() to designate local cache location")}

  # Load state crosswalk data
  cli_alert("Loading location crosswalk")
  state_crosswalk <- read_delim("https://www2.census.gov/geo/docs/reference/state.txt", delim = "|") %>%
    select(location = STATE, geo_value = STUSAB, location_name = STATE_NAME)

  if(is.null(models)){
    models <- c("COVIDhub-trained_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")

  }

  # Obtain data based on the chosen source
  switch(source,
         "covidHubUtils" = {
           cli_alert("Fetching COVID-19 forecasts using covidHubUtils")

           signal_data <- load_forecasts(
             models = models,
             hub = c("US"),
             dates = as_date(su_yaml$forecast_date),
             date_window_size = 0,
             targets = paste(0:27, "day ahead inc hosp"),
             source = "zoltar",
             verbose = FALSE
           ) %>%
             filter(location != "US") %>%
             select(model, forecast_date, location, target_end_date, type,  quantile, value)
         },
         "cache" = {
           cli_alert("Reading COVID-19 forecasts from local cache")

           #date checks
           forecast_date <- as_date(su_yaml$forecast_date)
           forecast_end <- as_date(forecast_date, format = "%Y-%m-%d") + 27

           begin_date <- cast_date_string(forecast_date, "0000-00-00")
           end_date <- cast_date_string(forecast_end, "9999-99-99")

           #cache parquet files index with arrow
           query <- arrow::open_dataset(
             file.path(su_yaml$local_cache_dir, "forecast"),
             partitioning = arrow::schema(forecast_date = arrow::string())
           )

           #match index names
           query <- query %>%
             mutate(signal = case_when(
               str_detect(target, "cum death") ~ "cum_death",
               str_detect(target, "inc death") ~ "death",
               str_detect(target, "inc hosp") ~ "hosp"
             ))

           #filter query
           query <- query %>%
             filter(signal == "hosp",
                    forecast_date == begin_date,
                    target_end_date <= forecast_end,
                    location %in% state_crosswalk$location,
                    model %in% models)

           #states only
           signal_data <- collect(query) %>%
             mutate(across(c(forecast_date, target_end_date), ymd)) %>%
             select(model, forecast_date, location, target_end_date, type,  quantile, value)
         },
         "test" = {
           cli_alert("Loading test forecast data for the period 2021-08-23 through 2021-09-19")
           load("data/Covid19_forecasts_2021-08-23.RData")
           signal_data <- signal_data
         }

  )

        if(write_copy == TRUE){

          cli_alert("Writing forecast data to analysis directory")

          fdir_name <- paste0(su_yaml$out_dir_name,"/hub_forcasts")
          if (!dir.exists(fdir_name)) {
            dir.create(fdir_name)
          }

          # Write to .csv file in newly created directory
          filename_loop <- paste0(fdir_name, "/", as_date(su_yaml$forecast_date), "-", paste0(source), "-run", Sys.Date(), ".csv")
          signal_data <- as.data.frame(signal_data)
          write.csv(signal_data, file = paste0(filename_loop), row.names = FALSE)
        }

  return(as.data.frame(signal_data))
}
