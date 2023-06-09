% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/propose-weights.R
\name{propose_weights}
\alias{propose_weights}
\title{Create model weights based on ranking and apply to forecast data}
\usage{
propose_weights(
  forecast_data,
  ingest = c("dataframe", "path", "list"),
  rank_df,
  rankCol = NULL,
  drop = NULL,
  team = "TeamName",
  mod_name = "my_ensemble"
)
}
\arguments{
\item{forecast_data}{A dataframe, path to directory with csv files, or list of csv files containing the forecasts to be weighted.}

\item{ingest}{A character vector that determines whether the forecast_data parameter represents a dataframe ("dataframe"), a directory path as a string (e.g. "./forecast_data") of csv files containing forecast data ("path"), or a list of strings each representing the path to a csv file ("list").}

\item{rank_df}{A dataframe containing columns for each model's name and its rank.}

\item{rankCol}{The name of the column in the \code{rank_df} dataframe that contains the ranking information.}

\item{drop}{An integer indicating how many models at the bottom of the ranking should be excluded from the analysis.}

\item{team}{A string representing the name of the team generating the forecast data.}

\item{mod_name}{A string representing the name of the weighted ensemble model.}
}
\value{
Returns a dataframe containing the calculated weights, which is ordered by rank, and writes a csv file of the weighted ensemble forecast to a specified directory.
}
\description{
This function creates weights based on a ranked dataframe of models. It takes in either a path containing csv files, a list of csv files, or a dataframe with a column named "model_name" containing the respective model names and applies the weights to each model's predictions. The output is a new csv file with the weighted ensemble forecast and a dataframe of the calculated weights.
}
\examples{
props_weights <- propose_weights(forecast_data = "path/to/forecast_data", ingest = "path", rank_df, rankCol = "rank", drop = 2, team = "my_team", mod_name = "weighted_ensemble")

}
