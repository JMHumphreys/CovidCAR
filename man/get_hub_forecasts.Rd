% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-hub-forecasts.R
\name{get_hub_forecasts}
\alias{get_hub_forecasts}
\title{Get COVID-19 forecasts from various sources}
\usage{
get_hub_forecasts(
  source = c("covidHubUtils", "cache", "test"),
  models = NULL,
  write_copy = TRUE
)
}
\arguments{
\item{source}{A character vector to specify where the forecast data should be obtained from (default is 'covidHubUtils'). Accepted values include "covidHubUtils" for fetching data online, "cache" for reading saved data from local cache, and "test" for loading test data locally.}

\item{models}{A character vector of models to be included in the query.
Default is NULL which includes COVIDhub-trained_ensemble, COVIDhub-ensemble, and COVIDhub-baseline.}

\item{write_copy}{A logical value indicating if a copy of the downloaded data should be written to the analysis directory. Default is TRUE.}
}
\value{
Returns a dataframe containing COVID-19 forecasts with columns for model name, forecast date, location, target end date, type, quantile, and value.
}
\description{
This function allows you to obtain COVID-19 forecasts based on selected data source. The data can come from covidHubUtils, cache or test depending on preference.
}
\examples{
#get Hub forecasts using default arguments:
get_hub_forecasts()

#get Hub forecasts specifying model(s):
get_hub_forecasts(models = c("COVIDhub-ensemble"))

#read saved data from cache:
get_hub_forecasts(source = "cache")

#load test data:
get_hub_forecasts(source = "test")

}
