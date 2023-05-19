#' Function to set up CovidCAR analysis
#'
#' This function sets up the analysis by converting the report_date argument to a date object, calculating the start and end dates of the training period and forecast horizon, generating a full time span (as a character vector) from the training start date to the forecast horizon end date, setting the local cache directory, creating an output directory if it doesn't exist, and returning the specified output directory.
#'
#' @param report_date Date string in yyyy-mm-dd format
#' @param training_period Numeric value representing the length of the training period in days
#' @param forecast_horizon Numeric value representing the length of the forecast horizon in days
#' @param local_cache_dir Character string representing the path to the local cache directory
#' @param output_dir Character string representing the path to the output directory
#'
#' @return The specified output directory is returned but not used within this function
#' @export

setup_analysis <- function(report_date, training_period, forecast_horizon, output_dir=NULL, local_cache_dir=NULL){

  # Convert report_date to a date object
  forecast_date <<- as_date(report_date)

  # Calculate the start and end dates of the training period
  train_start_date <<- forecast_date - training_period
  train_end_date <<- forecast_date   #8wk period

  # Calculate the start and end dates of the forecast horizon
  forecast_horiz_start <<- forecast_date+1 #1 day after training period
  forecast_horiz_end <<- forecast_horiz_start + (forecast_horizon - 1)

  # Generate a full time span from the training start date to the forecast horizon end date
  full_time_span <<- as.character(seq(train_start_date, forecast_horiz_end, 1))

  # Set the local cache directory
  local_cache_dir <<- local_cache_dir

  # Return the output directory (not used in this function but can be accessed later if needed)
  output_dir <<- output_dir

  # got cache?
  if(is.null(local_cache_dir) == FALSE){cli_alert("Your local cache will be available to get_covid19_obs()")
  } else{cli_alert("No local cache provided, see get_covid19_obs() to retrieve observation data")}

  # assign working dir
  if(is.null(output_dir) == FALSE){
    out_dir_name <<- paste0(output_dir, "/", forecast_date, "-CovidCAR-run", Sys.Date())
  } else{
    out_dir_name <<- paste0(getwd(), "/", forecast_date, "-CovidCAR-run", Sys.Date())
  }

  if (!dir.exists(out_dir_name)) {
    dir.create(out_dir_name)
  }

  cli_alert(paste0("Analysis outputs will be written to ", paste(out_dir_name)))
}
