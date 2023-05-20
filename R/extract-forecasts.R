#' Extract forecasts from a model
#'
#' This function extracts forecasts from an INLA model object then formats
#' data for submission to the Covid19-forecast-hub (https://github.com/reichlab/covid19-forecast-hub).
#'
#' @param nrm.stk An object of class 'inla.stack'
#' @param mod_out The fitted model object
#' @param train_data The training data used to fit the model
#'
#' @return A data frame with columns for the forecast date, location, target,
#' target end date, type of value (quantile or point), quantile value (if
#' applicable), and forecast value as required for submission to Covid19-forecast-hub.
#'
#' @export
extract_forecasts <- function(mod_out, dataStack, train_data, team = "TeamName") {

  if(is.null(su_yaml$forecast_date) == TRUE){
    cli_abort("Run setup_analysis() to designate key analysis dates before proceeding")}

  forecast_paths = list()
  return_quants = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

  for(i in 1:length(mod_out)){

    # Extract summary fitted values
    idat <- inla.stack.index(dataStack, "nrm")$data
    getValues <- mod_out[[i]]$summary.fitted.values[idat,c(3:25)]
    names(getValues) = paste("q", return_quants, sep="_")

    # Apply observation scale and set negative values to 0
    getValues <- data.frame(lapply(getValues, obs_scale))
    getValues[getValues < 0] <- 0

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
      mutate(forecast_date = as_date(su_yaml$forecast_date),
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

    # Create directory if it doesn't exist
    fdir_name <- paste0(su_yaml$out_dir_name,"/forecasts")
    if (!dir.exists(fdir_name)) {
      dir.create(fdir_name)
    }

    cli_progress_done()
    cli_progress_step("Writing model forecasts to analysis directory: ", names(mod_out)[i])

    # Write to .csv file in newly created directory
    filename_loop <- paste0(fdir_name, "/", as_date(su_yaml$forecast_date), "-", paste0(team), "-", names(mod_out)[i],".csv")
    forecast_paths[[paste0(names(mod_out)[i])]] = filename_loop
    write.csv(getValues, file = paste0(filename_loop), row.names = FALSE)
  }

  #forecast_paths <- list.files(fdir_name, pattern='csv', full.names = TRUE, recursive = TRUE)
  #names(forecast_paths) = names(mod_out)
  forecast_paths <<- forecast_paths

}
