#' Obtain COVID-19 observed data based on the chosen source
#'
#' This function obtains COVID-19 observed data from either the covidcast API, local cache or test data and returns them as a data frame.
#'
#' @param source A character string indicating the source of data to be obtained. Possible values are "covidcast" for obtaining data from the covidcast API, "cache" for obtaining data from local cache and "test" for obtaining test data.
#' @param start_date A character string in the format "YYYY-MM-DD" indicating the start date for the data to be obtained.
#' @param end_date A character string in the format "YYYY-MM-DD" indicating the end date for the data to be obtained.
#' @return Data frame containing COVID-19 observed data.
#' @export
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
           cli_alert("Fetching COVID-19 observed data from covidcast")
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
           cli_alert("Loading COVID-19 forecast functions")
           source('covid19Forecast-utils.R')
           min_date <- as.Date(start_date, format = "%Y-%m-%d")
           max_date <- as.Date(end_date, format = "%Y-%m-%d")
           signal_data <- get_observed_series(signals ="hosp", start_date = min_date,
                                              end_date = max_date, cache_dir = su_yaml$local_cache_dir)
           cli_alert("Returning data fetched from local cache")
         },
         "test" = {
           cli_alert("Loading test data for the period 2021-06-28 through 2021-09-20")
           load("data/Covid19_hosp.RData")
           signal_data <- Covid19_hosp
         }
  )

  return(as.data.frame(signal_data))
}