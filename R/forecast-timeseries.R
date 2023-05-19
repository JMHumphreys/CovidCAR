#' Forecast Time Series
#'
#' This function takes in a time series data and generates a forecast for specified number of periods.
#' The function fits an ARIMA model to the data and uses it to generate the forecast.
#' If the data has a clear seasonal pattern, the function fits a seasonal ARIMA model.
#' The function returns two lists: obs_fitted (the fitted values over the observation period)
#' and pred_trend (the predicted trend for the forecast horizon).
#'
#' @param ts_data a time series object or a numeric vector
#' @param forecast_horizon a positive integer specifying the number of periods to forecast
#' @param season_period an optional parameter specifying the seasonal period of the time series
#' @return a list containing the observations and predicted trend for the forecast horizon
#' @examples
#' # generate a time series data
#' ts_data <- ts(rnorm(100), frequency = 12, start = c(2014, 1))
#' # generate a forecast for next 6 periods using auto.arima() with default settings
#' forecast_time_series(ts_data, 6)
forecast_time_series <- function(ts_data, forecast_horizon, season_period = NULL) {

  if(is.null(su_yaml$forecast_horiz_start) == TRUE){
    cli_abort("Run setup_analysis() to designate key analysis dates before proceeding")}

  # Convert the time series to a ts object if it is not already
  if (!is.ts(ts_data)) {
    ts_data <- ts(ts_data)
  }

  # Check if the time series is seasonal or not
  if (is.null(season_period)) {
    # Use auto.arima() to fit a non-seasonal ARIMA model to the data
    model_fit <- auto.arima(ts_data)
    # Generate a forecast using forecast() function
    forecast_results <- forecast(model_fit, h = forecast_horizon)
  } else {
    # Find the seasonal period of the time series
    max_lag <- which(diff(sign(diff(acf(diff(ts_data),
                                        plot = FALSE)$acf)))==-2 & acf(diff(ts_data),                                                                                       plot = FALSE)$acf[-1]>0.5)[1]
    if(is.na(max_lag)){
      stop("The time series doesn't have a clear seasonal pattern.")
    }
    # Use auto.arima() to fit a seasonal ARIMA model to the data
    model_fit <- auto.arima(ts_data, seasonal = TRUE, period = max_lag)
    # Generate a forecast using forecast() function
    forecast_results <- forecast(model_fit, h = forecast_horizon*max_lag)
  }

  arima_out = list()
  #7 day lag (EpiEstim) + fitted over observation period
  arima_out[["obs_fitted"]] = c(rep(NA, 7), model_fit$fitted)
  #forecast 28 days
  arima_out[["pred_trend"]] = as.numeric(forecast_results$mean)

  # Return the results
  return(arima_out)
}
