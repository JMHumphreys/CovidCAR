% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score-WIS.R
\name{score_WIS}
\alias{score_WIS}
\title{Score WIS function}
\usage{
score_WIS(
  forecast_data,
  truth,
  ingest = c("dataframe", "path", "list"),
  missing = c("remove", "zero")
)
}
\arguments{
\item{forecast_data}{A dataframe,directory path,or list object providing forecast data with columns including: model, date, location_name, forecast_date, quantile, and value.
Can also be a path or a list of paths to csv files.}

\item{truth}{A dataframe containing the true values to compare the forecast data against, with columns including: date, location_name, and value.}

\item{ingest}{A character vector specifying the type of input for 'forecast_data', which can be either "dataframe", "path", or "list".}

\item{missing}{A character vector specifying the method to handle missing true values in the scoring process. It can be either "remove" or "zero".}
}
\value{
Returns a dataframe with scores for each model, date, location, and forecast_date combination based on the WIS metric.
}
\description{
This function scores the weighted interval scoring (WIS) for a given forecast data against the truth.
}
\examples{
# score the WIS for some example forecast and truth dataframes
library(scoringutils)
example_forecast <- data.frame(model = c("model1","model2"), date = as.Date(c("2020-06-01","2020-06-02")),
location_name = c("location1","location2"), forecast_date = as.Date(c("2020-06-03","2020-06-04")),
quantile = 0.5, value = c(10,20))
example_truth <- data.frame(date = as.Date(c("2020-06-03","2020-06-04")), location_name = c("location1","location2"), value = c(8,22))
score_WIS(example_forecast, example_truth, "dataframe", "remove")
}
