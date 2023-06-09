% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-forecasts.R
\name{extract_forecasts}
\alias{extract_forecasts}
\title{Extract forecasts from a model}
\usage{
extract_forecasts(mod_out, dataStack, train_data, team = "TeamName")
}
\arguments{
\item{mod_out}{The fitted model object}

\item{train_data}{The training data used to fit the model}

\item{nrm.stk}{An object of class 'inla.stack'}
}
\value{
A data frame with columns for the forecast date, location, target,
target end date, type of value (quantile or point), quantile value (if
applicable), and forecast value as required for submission to Covid19-forecast-hub.
}
\description{
This function extracts forecasts from an INLA model object then formats
data for submission to the Covid19-forecast-hub (https://github.com/reichlab/covid19-forecast-hub).
}
