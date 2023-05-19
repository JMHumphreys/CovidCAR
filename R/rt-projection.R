#' Project Rt values using ARIMA or DLM method
#'
#' This function projects Rt (reproductive rate) values for a given time period and location using either an ARIMA or DLM model.
#'
#' @param train_data A dataframe with columns for date, location_name, and value, where value is a numeric vector representing the number of new cases or incidences for that day and location.
#' @param forecast_horiz_start A date string representing the start of the time period to project Rt values for.
#' @param forecast_horiz_end A date string representing the end of the time period to project Rt values for.
#' @param mean_si A numeric value representing the mean serial interval for the disease being modeled. Default is 5.7.
#' @param std_si A numeric value representing the standard deviation of the serial interval for the disease being modeled. Default is 2.
#' @param forecast_horizon a positive integer specifying the number of periods to forecast
#' @param method A character vector specifying the method to use for projecting Rt values. Valid options are "arima" or "dlm". Default is "arima".
#'
#' @return A dataframe with the same columns as `train_data`, plus an additional column `Rt` representing the projected reproductive rate for each day and location.
#'
#' @importFrom dplyr filter select rename arrange mutate bind_rows
#' @importFrom EpiEstim make_config estimate_R
#' @importFrom forecast forecast_time_series
#'
#' @examples
#' # Load example data
#' data(covid_cases)
#'
#' # Project Rt values using default parameters and ARIMA method
#' Rt_df <- Rt_projection(train_data = covid_cases,
#'                         forecast_horiz_start = "2020-04-12",
#'                         forecast_horiz_end = "2020-04-26")
#'
#' # Project Rt values using DLM method
#' Rt_df <- Rt_projection(train_data = covid_cases,
#'                         forecast_horiz_start = "2020-04-12",
#'                         forecast_horiz_end = "2020-04-26",
#'                         method = "dlm")
#'
#' @export
Rt_projection <- function(train_data, mean_si = 5.7, std_si = 2, forecast_horizon = 28, method = c("arima", "dlm")) {

  if(is.null(su_yaml$forecast_horiz_start) == TRUE){
    cli_abort("Run setup_analysis() to designate key analysis dates before proceeding")}

  locs_loop <- unique(train_data$location_name)
  Rt_df <- NULL # initialize output

  for (i in 1:length(locs_loop)) {

    tmp.frame <- train_data %>%
      filter(location_name == locs_loop[i])

    inc_vect <- tmp.frame %>%
      filter(!is.na(value), date < as_date(su_yaml$forecast_horiz_start)) %>%
      select(date, value) %>%
      rename(dates = date, I = value) %>%
      arrange(dates)

    if (sum(inc_vect$I) <= 3) {
      tmp.frame$Rt <- 0
    } else {
      # Run EpiEstim, 1 week window
      t_steps <- nrow(inc_vect)
      t_start <- seq(2, t_steps - 6)
      config_lit <- EpiEstim::make_config(
        t_start = t_start,
        t_end = t_start + 6,
        mean_si = mean_si,
        std_si = std_si
      )
      Rt_interpolate <- EpiEstim::estimate_R(
        incid = inc_vect,
        method = "parametric_si",
        config = config_lit)

      Rt_raw = c(rep(NA,7), Rt_interpolate$R[,"Median(R)"]) #7-day lag; keep copy before forecast

      if (method == "arima") {
        # Run ARIMA function
        arima_out <- try(forecast_time_series(Rt_interpolate$R[,"Median(R)"],
                                              forecast_horizon = forecast_horizon), silent = TRUE)
      } else if (method == "dlm") {
        # Run INLA DLM
        arima_out <- try(inla_dlm(Rt_interpolate$R[,"Median(R)"],
                                  forecast_horizon = forecast_horizon), silent = TRUE)
      } else {
        stop("Specify Method as 'arima' or 'dlm'")
      }

      # Handle errors from forecast model functions
      if (inherits(arima_out, "try-error")) {
        stop(paste("Error in", ifelse(method == "arima", "forecast_time_series()", "inla_dlm()"), ": ", conditionMessage(arima_out)))
      }

      # Match results to dates
      match_arima <- data.frame(
        date = c(inc_vect$dates, seq(as_date(su_yaml$forecast_horiz_start), as_date(su_yaml$forecast_horiz_end), 1)),
        Rt_raw = c(Rt_raw, rep(NA, length(seq(as_date(su_yaml$forecast_horiz_start), as_date(su_yaml$forecast_horiz_end), 1)))),
        Rt = c(arima_out[["obs_fitted"]], arima_out[["pred_trend"]])
      ) %>%
        mutate(date = as_date(date))

      tmp.frame = left_join(tmp.frame, match_arima, by = "date")

    }

    Rt_df <- bind_rows(Rt_df, tmp.frame)
    cli_alert_success("{paste0(locs_loop[i])} completed!")


  }

  return(Rt_df)

}
