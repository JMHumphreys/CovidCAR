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
get_covid19_obs <- function(source = c("covidcast","cache","test"), start_date, end_date, write_copy = TRUE) {

  if(source == "cache" & class(su_yaml$local_cache_dir) != "character"){
    cli_abort("Where's the cache? Change 'source=' or run setup_analysis() to designate local cache location")}

  # Load state crosswalk data
  cli_alert("Loading location crosswalk")
  state_crosswalk <- read_delim("https://www2.census.gov/geo/docs/reference/state.txt", delim = "|") %>%
    select(location = STATE, geo_value = STUSAB, location_name = STATE_NAME)

  # Obtain data based on the chosen source
  switch(source,
         "covidcast" = {
           cli_alert("Fetching COVID-19 observations using covidcast")
           signal_data <- covidcast::covidcast_signal(data_source = "hhs",
                                                      signal = "confirmed_admissions_covid_1d",
                                                      start_day = start_date, end_day = end_date,
                                                      geo_type = "state", time_type = "day") %>%
             mutate(signal = "hosp",
                    date = time_value,
                    geo_value = toupper(geo_value)) %>%
             select(date, geo_value, value, signal) %>%
             left_join(state_crosswalk, by = "geo_value") %>%
             select(-geo_value)
           signal_data$location_name[signal_data$location_name == "U.S. Virgin Islands"] = "Virgin Islands"
         },
         "cache" = {
           cli_alert("Reading COVID-19 observations from local cache")

           #date checks
           min_date <- as.Date(start_date, format = "%Y-%m-%d")
           max_date <- as.Date(end_date, format = "%Y-%m-%d")

           start_date <- cast_date_string(start_date, "0000-00-00")
           end_date <- cast_date_string(end_date, "9999-99-99")

           #cache parquet files index with arrow
           query <- arrow::open_dataset(file.path(su_yaml$local_cache_dir, "observed")) %>%
             mutate(across(date, as.character))

           # filter by dates
           query <- filter(query, between(date, start_date, end_date))
           query <- filter(query, signal == "hosp")

           #states only
           signal_data <- collect(query) %>%
             filter(location != "US") %>%
             mutate(across(date, ymd))
         },
         "test" = {
           cli_alert("Loading test data for the period 2021-06-28 through 2021-09-20")
           load("data/Covid19_hosp.RData")
           signal_data <- Covid19_hosp
         }

  )

        if(write_copy == TRUE){

          cli_alert("Writing observation data to analysis directory")

          fdir_name <- paste0(su_yaml$out_dir_name,"/observations")
          if (!dir.exists(fdir_name)) {
            dir.create(fdir_name)
          }

          # Write to .csv file in newly created directory
          filename_loop <- paste0(fdir_name, "/", as_date(su_yaml$forecast_date), "-", paste0(source), "-run", Sys.Date(), ".csv")
          signal_data <- as.data.frame(signal_data)
          write.csv(signal_data, file = paste0(filename_loop), row.names = FALSE)
        }

  return(signal_data)
}
