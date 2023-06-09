% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score-MAE.R
\name{score_MAE}
\alias{score_MAE}
\title{Calculate Mean Absolute Error given forecast and truth data}
\usage{
score_MAE(
  forecast_data,
  truth,
  ingest = c("dataframe", "path", "list"),
  missing = c("remove", "zero")
)
}
\arguments{
\item{forecast_data}{A dataframe, path or list object containing forecast information}

\item{truth}{A dataframe containing true values to compare forecasts against}

\item{ingest}{Character value indicating how forecast data is being passed.
Options are "dataframe", "path", or "list". Default is "dataframe".}

\item{missing}{Character value indicating how to handle missing or NaN values in the truth data.
Options are "remove" or "zero". If "remove", any data points with missing true values will be removed from calculation.
If "zero", any data points with missing true values will be assumed to have a true value of 0. Default is "remove".}
}
\value{
Returns a data frame with columns for Model, MAE, MAPE, and Rank, sorted by ascending order of MAE.
}
\description{
This function calculates the Mean Absolute Error (MAE) for a set of forecasts and corresponding true values.
}
\examples{
score_MAE(forecast_data = my_forecasts, truth = my_truth, ingest = "dataframe", missing = "remove")

}
