#' Extract forecasts from a model
#'
#' This function extracts forecasts from an INLA model object then formats
#' data for submission to the Covid19-forecast-hub (https://github.com/reichlab/covid19-forecast-hub).
#'
#' @param nrm.stk An object of class 'inla.stack'
#' @param mod_out The fitted model object
#' @param train_data The training data used to fit the model
#' @param return_quants A vector specifying the quantiles to extract
#' @param forecast_date The forecast report date
#'
#' @return A data frame with columns for the forecast date, location, target,
#' target end date, type of value (quantile or point), quantile value (if
#' applicable), and forecast value as required for submission to Covid19-forecast-hub.
#'
#' @export
extract_forecasts <- function(nrm.stk, mod_out, train_data, return_quants, forecast_date) {

  # Extract summary fitted values
  idat <- inla.stack.index(nrm.stk, "nrm")$data
  getValues <- mod_out$summary.fitted.values[idat,c(3:25)]
  names(getValues) = paste("q", return_quants, sep="_")

  # Apply observation scale and set negative values to 0
  getValues <- data.frame(lapply(getValues, obs_scale))
  getValues[getValues < 0] <- 0

  # Get range of q_0.5
  range(getValues$q_0.5)

  # Add location and target end date
  getValues <- getValues %>%
    mutate(location = train_data$location,
           target_end_date = train_data$date)

  # Reshape data to long format
  getValues <- reshape2::melt(getValues, c("location", "target_end_date"))
  names(getValues) <- c("location", "target_end_date", "quantile", "value")
  getValues$quantile <- gsub("q_", "", getValues$quantile)

  # Add forecast date and calculate target
  getValues <- getValues %>%
    mutate(forecast_date = forecast_date,
           target = as.numeric(target_end_date - forecast_date)) %>%
    subset(target >= 0) %>%
    mutate(target = paste(target, "day ahead inc hosp", sep=" "),
           type = "quantile") %>%
    select(forecast_date, location, target, target_end_date, type, quantile, value)

  # Group by and summarize for US data
  us_sum <- getValues %>%
    group_by(forecast_date, target, target_end_date, type, quantile) %>%
    summarise(value = sum(value)) %>%
    mutate(location = "US")

  # Combine US data with other data
  getValues <- bind_rows(getValues, us_sum)

  # Calculate point estimate and add to data
  point_est <- getValues %>%
    filter(quantile == 0.5) %>%
    mutate(type = "point",
           quantile = NA)

  getValues <- bind_rows(getValues, point_est)

  return(getValues)
}
