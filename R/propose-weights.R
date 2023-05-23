#' Create model weights based on ranking and apply to forecast data
#'
#' This function creates weights based on a ranked dataframe of models. It takes in either a path containing csv files, a list of csv files, or a dataframe with a column named "model_name" containing the respective model names and applies the weights to each model's predictions. The output is a new csv file with the weighted ensemble forecast and a dataframe of the calculated weights.
#'
#' @param forecast_data A dataframe, path to directory with csv files, or list of csv files containing the forecasts to be weighted.
#'
#' @param ingest A character vector that determines whether the forecast_data parameter represents a dataframe ("dataframe"), a directory path as a string (e.g. "./forecast_data") of csv files containing forecast data ("path"), or a list of strings each representing the path to a csv file ("list").
#'
#' @param rank_df A dataframe containing columns for each model's name and its rank.
#'
#' @param rankCol The name of the column in the \code{rank_df} dataframe that contains the ranking information.
#'
#' @param drop An integer indicating how many models at the bottom of the ranking should be excluded from the analysis.
#'
#' @param team A string representing the name of the team generating the forecast data.
#'
#' @param mod_name A string representing the name of the weighted ensemble model.
#'
#' @return Returns a dataframe containing the calculated weights, which is ordered by rank, and writes a csv file of the weighted ensemble forecast to a specified directory.
#'
#' @examples
#' props_weights <- propose_weights(forecast_data = "path/to/forecast_data", ingest = "path", rank_df, rankCol = "rank", drop = 2, team = "my_team", mod_name = "weighted_ensemble")
#'
#' @import tidyverse
#' @importFrom lubridate as_date
#' @import cli.alerts
#' @export
propose_weights <- function(forecast_data, ingest = c("dataframe", "path", "list"),
                            rank_df, rankCol = NULL, drop=NULL,
                            team = "TeamName", mod_name = "my_ensemble"){

  if(is.null(rankCol) & is.numeric(drop)){
    cli_alert_danger("No rankCol but dropping models? Models will be dropped from end of list!")



  }

  if(is.null(rankCol)){
    rank_df$rankCol = 1
    rankCol = "rankCol"
    cli_alert_warning("No rankCol provided, ensemble assumes equal weighting")
  }

  if(is.numeric(drop) == TRUE){

    dropsies <- nrow(rank_df) - drop
    weights_tmp <- arrange(rank_df, rankCol)[1:dropsies,]
    cut_mods <- rank_df[(dropsies+1):nrow(rank_df),]$model
    cli_alert("Dropped models: {paste0(cut_mods)}")

  } else{

    weights_tmp = arrange(rank_df, rankCol)
  }


    if (ingest == "path" & is.character(forecast_data)) {
    fnames <- list.files(forecast_data, pattern='csv', full.names = TRUE, recursive = TRUE)

    tmp_df <- lapply(fnames, function(file) {
      read.csv(file) %>%
        mutate(model = get_model_name(file)) %>%
        filter(model %in% weights_tmp$model)
    }) %>% do.call("rbind", .)
  }

  if (ingest == "list" & is.list(forecast_data)) {
    tmp_df <- lapply(forecast_data, function(file) {
      read.csv(file) %>%
        mutate(model = get_model_name(file)) %>%
        filter(model %in% weights_tmp$model)
    }) %>% do.call("rbind", .)
  }

  if (ingest == "dataframe" & is.data.frame(forecast_data)) {
    if(length(forecast_data$model) == 0){
      cli_abort("A 'model' column indicating model names is required when reading as a dataframe")
    }

    tmp_df <- forecast_data %>%
      mutate(model = model) %>%
      filter(model %in% weights_tmp$model)
  }

  weights_tmp$weight <- 1 - weights_tmp[[paste0(rankCol)]]/sum(weights_tmp[[paste0(rankCol)]])
  weights_tmp$weight <- weights_tmp$weight/sum(weights_tmp$weight)

  tmp_df$weight <- with(weights_tmp,
                        weight[match(
                          tmp_df$model,
                                 model)])

  tmp_df <- tmp_df %>%
    mutate(new_pred = value*weight) %>%
    group_by(forecast_date, location, target, target_end_date, type, quantile) %>%
    summarise(value = sum(new_pred))

    # Create directory if it doesn't exist
    fdir_name <- paste0(su_yaml$out_dir_name,"/forecasts")
    if (!dir.exists(fdir_name)) {
      dir.create(fdir_name)
    }

    cli_alert(paste0("Writing ensemble '", paste0(mod_name), "' to analysis directory"))

    # Write to .csv file in newly created directory
    filename_loop <- paste0(fdir_name, "/", as_date(su_yaml$forecast_date), "-", paste0(team), "-", paste0(mod_name),".csv")
    write.csv(tmp_df, file = paste0(filename_loop), row.names = FALSE)

    col_label = paste0(rankCol, "_weights")
    weights_tmp[,col_label] <- weights_tmp$weight

    weights_tmp <- weights_tmp %>% select(-weight)

    return(weights_tmp)

}
