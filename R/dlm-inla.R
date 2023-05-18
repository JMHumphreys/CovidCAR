#' Dynamic Linear Model using INLA
#'
#' This function fits a dynamic linear model (DLM) to the input time series data, and produces forecasts for the specified number of periods using the INLA method.
#'
#' @param ts_data Time-series data as numeric vector
#' @param forecast_horizon The number of periods for which forecasts are required
#'
#' @return The function returns a list with two elements:
#' \itemize{
#' \item \code{obs_fitted}: A numeric vector representing the fitted values over the observation period including a 7-day lag (EpiEstim).
#' \item \code{pred_trend}: A numeric vector representing the predicted values for the forecast horizon.
#' }
#'
#' @examples
#' inla_dlm(c(10,12,16,18,22,26,30,34,40),5)
#'
#' @importFrom inla inla
#' @export

inla_dlm <- function(ts_data, forecast_horizon){

  obs_len = length(ts_data)
  ts_df = as.data.frame(c(ts_data, rep(NA, forecast_horizon)))
  names(ts_df) = "Rt_trend"
  ts_df$id.1 = ts_df$id.2 = ts_df$id.3 = 1:nrow(ts_df)

  dlm.form <- Rt_trend ~ -1 + f(id.1, model = "rw2", constr = FALSE) +
    f(id.2, model = "linear", mean.linear = 0, prec.linear = 0.001)

  dlm.mod <- inla(
    dlm.form,
    family = "gaussian",
    data = ts_df,
    control.predictor = list(compute = TRUE)
  )

  arima_out = list()
  #7 day lag (EpiEstim) + fitted over observation period
  arima_out[["obs_fitted"]] = c(rep(NA, 7), dlm.mod$summary.fitted.values$mean[1:obs_len])
  #forecast 28 days
  arima_out[["pred_trend"]] = as.numeric(dlm.mod$summary.fitted.values$mean[(obs_len+1):max(ts_df$id.1)])

  # Return the results
  return(arima_out)
}
