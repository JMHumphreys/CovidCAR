#' Obtain COVID-19 Observation Data
#'
#' This function obtains the COVID-19 observation data based on the chosen source.
#'
#' @param source A character string indicating the source of the data to be obtained. Allowed values are "covidcast" (to fetch data using covidcast), "cache" (to read data from local cache) and "test" (to load test data).
#' @param start_date A character string indicating the start date of the period for which the data is needed. The date should be in the format "YYYY-MM-DD".
#' @param end_date A character string indicating the end date of the period for which the data is needed. The date should be in the format "YYYY-MM-DD".
#' @param write_copy A logical value indicating whether the data obtained should be written to a file or not. The default value is TRUE.
#'
#' @return Returns a data frame containing COVID-19 observation data based on the chosen source.
#'
#' @details If source = "covidcast", the function fetches the COVID-19 observation data using the covidcast package. If source = "cache", the function reads the COVID-19 observation data from the local cache. If source = "test", the function loads test data for the period 2021-06-28 through 2021-09-20. The function also loads state crosswalk data and performs some date checks and data filtering based on the selected source.
#'
#' If write_copy = TRUE, the data obtained is written to a .csv file in a newly created directory named "observations" within the analysis directory.
#'
#' @importFrom readr read_delim
#' @importFrom dplyr select mutate left_join filter
#' @importFrom lubridate ymd
#' @importFrom arrow open_dataset collect as.Date
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
