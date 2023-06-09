% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-forecast-template.R
\name{create_forecast_template}
\alias{create_forecast_template}
\title{Create a Forecast Template}
\usage{
create_forecast_template(train_data)
}
\arguments{
\item{train_data}{Input data used to create the forecast template.}
}
\value{
Returns a data frame containing the forecast template.
The output has the following columns: \code{location}, \code{date}, \code{value}, \code{signal}, \code{day}, and \code{trn_tst}.
Each row of the output dataframe will contain a unique location from the input dataset, and dates ranging from \code{forecast_horiz_start} to \code{forecast_horiz_end}.
The values in the \code{value} column are left as NA.
The \code{signal} column is set to "hosp" indicating hospital data.
The \code{day} column specifies the weekday for the corresponding date.
The \code{trn_tst} column indicates whether the observation was part of the training or test dataset. For the forecast template, all rows will have "test".
The final output contains only distinct observations with respect to columns \code{date} and \code{location}.
Additionally, the dataset returned by this function contains an additional column named 'Region' joined from the original input data.
}
\description{
This function creates a forecast template for the given dataset where the output includes all unique locations from the input data and the specified forecast horizon.
}
\examples{
library(dplyr)
library(lubridate)
# create sample data
date_data <- seq(as.Date("2021-01-01"), as.Date("2021-01-31"), by = "day")
loc_data <- c('loc1', 'loc2')
val_data <- c(1, 2)
hosp_data <- rep('hosp', length(date_data))
day_data <- weekdays(date_data)
trn_tst_data <- 'train'
data <- data.frame(date = rep(date_data, length(loc_data)),
                   location = rep(loc_data, each = length(date_data)),
                   value = rep(val_data, each = length(date_data)),
                   signal = hosp_data,
                   day = rep(day_data, length(loc_data)),
                   trn_tst = trn_tst_data)

create_forecast_template(data, as.Date("2021-02-01"), as.Date("2021-02-10"))

}
