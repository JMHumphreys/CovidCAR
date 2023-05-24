#' Plot comparison of forecasts
#'
#' This function takes in forecast data and truth data, compares models and plots the relative overall performance over time. Multiple file ingest formats are available. The function can be scaled using a specified model for normalization. Graphics are written to file.
#'
#' @param forecast_data A dataframe containing forecast predictions as exported from the RAPIDD API, or a list of such dataframes.
#' @param ingest A character string specifying the format of the \code{forecast_data} input. Can be one of 'dataframe', 'path' or 'list'. Defaults to 'dataframe'.
#' @param hub_forecasts (optional) A dataframe containing hub forecasts predictions as exported from the RAPIDD API.
#' @param truth A dataframe containing true values.
#' @param missing A character string specifying how to handle missing values in the true values dataset. Can be one of 'remove' or 'zero'. Default is 'remove'.
#' @param scale_model (optional) A character string specifying the name of the model to use as a reference for scaling scores.
#' @param limit (optional) A numeric value for the upper limit of the score scale. Default is NULL.
#' @param write_copy (optional) Logical indicating whether to save outputs to the current working directory. Default is TRUE.
#'
#' @details The function applies the scoringutils::score() function to calculate interval scores (WIS), then performs a normalization step if \code{scale_model} is provided. Graphics are written to file in the current working directory.
#'
#' @return A ggplot2 object that shows the relative overall performance of different models compared to each other.
#'
#' @examples
#' # Load example data
#' data('example_test_forecasts')
#' data('example_hub_forecasts')
#' data('example_truth_data')
#'
#' # Plot comparison of forecasts, with test data input as a dataframe
#' plot_forecasts_compare(forecast_data = example_test_forecasts,
#' hub_forecasts = example_hub_forecasts,
#' truth = example_truth_data)
plot_forecasts_compare <- function(forecast_data, ingest = c("dataframe", "path", "list"),
                                   hub_forecasts = NULL,
                                   truth, missing = c("remove","zero"),
                                   scale_model = NULL,
                                   limit = NULL,
                                   write_copy = TRUE){

    if (ingest == "path" & is.character(forecast_data)) {
      fnames <- list.files(forecast_data, pattern='csv', full.names = TRUE, recursive = TRUE)

      tmp_df <- lapply(fnames, function(file) {
        read.csv(file) %>%
          mutate(prediction = value,
                 model = get_model_name(file),
                 date = as.Date(target_end_date),
                 forecast_date = as.Date(forecast_date),
                 location_name = location) %>%
          filter(type == "quantile") %>%
          select(model, date, location_name, forecast_date, quantile, prediction)
      }) %>% do.call("rbind", .)
    }

    if (ingest == "list" & is.list(forecast_data)) {
      tmp_df <- lapply(forecast_data, function(file) {
        read.csv(file) %>%
          mutate(prediction = value,
                 model = get_model_name(file),
                 date = as.Date(target_end_date),
                 forecast_date = as.Date(forecast_date),
                 location_name = location) %>%
          filter(type == "quantile") %>%
          select(model, date, location_name, forecast_date, quantile, prediction)
      }) %>% do.call("rbind", .)
    }

    if (ingest == "dataframe" & is.data.frame(forecast_data)) {
      stopifnot(length(forecast_data$model_name) != 0)

      tmp_df <- forecast_data %>%
        mutate(prediction = value,
               model = model_name,
               date = as.Date(target_end_date),
               forecast_date = as.Date(forecast_date),
               location_name = location) %>%
        filter(type == "quantile") %>%
        select(model, date, location_name, forecast_date, quantile, prediction)
    }

    #prepare hub forecasts data
  hub_forecasts <- hub_forecasts %>%
    mutate(prediction = value,
           model = model,
           date = as.Date(target_end_date),
           forecast_date = as.Date(forecast_date),
           location_name = location) %>%
    filter(type == "quantile") %>%
    select(model, date, location_name, forecast_date, quantile, prediction)

   comb_data <- rbind(tmp_df, hub_forecasts)

    #match to truth
    truth <- truth %>%
      mutate(true_value = value,
             location_name = location) %>%
      select(date, location_name, true_value)

    comb_data <- left_join(comb_data, truth, by=c("date","location_name"))

    no_obs <- length(which(is.na(comb_data$true_value)))
    if(no_obs > 0 & missing == "remove"){
      cli_alert(paste0("A total of ", paste0(no_obs), " predictions weren't evalauted due lack of truth data"))
      comb_data <- comb_data %>% filter(is.na(true_value) == FALSE)
    }

    if(no_obs > 0 & missing == "zero"){
      cli_alert(paste0("A total of ", paste0(no_obs), " predictions lacked truth data and were filled with (zero)"))
      comb_data$true_value[is.na(comb_data$true_value)] = 0
    }

    #score models
    scores_out <- comb_data %>%
      scoringutils::score() %>%
      scoringutils::summarize_scores(by = c("model", "date", "location_name", "forecast_date")) %>%
      dplyr::select(model, date, location_name, forecast_date, WIS = interval_score) %>%
      as.data.frame()


    if(is.null(scale_model) == FALSE){
      scaler_df <- scores_out %>%
        filter(model == paste0(scale_model)) %>%
        mutate(scale_val = WIS) %>%
        select(date, location_name, forecast_date, scale_val)

      scores_out <- left_join(scores_out, scaler_df, by =c("date", "location_name", "forecast_date"))
      scores_out <- scores_out %>% filter(is.na(scale_val) == FALSE)

      scores_out <- scores_out %>%
        mutate(s.WIS = WIS/scale_val) %>%
        group_by(model, date) %>%
        summarise(WIS = mean(s.WIS))

      # Create directory if it doesn't exist
      fdir_name <- paste0(su_yaml$out_dir_name,"/comparisons")
      if (!dir.exists(fdir_name)) {
        dir.create(fdir_name)
      }

      cli_alert(paste0("Writing comparison scores to analysis directory"))

      # Write to .csv file in newly created directory
      num_files <- length(list.files(fdir_name))
      filename_loop <- paste0(fdir_name, "/", as_date(su_yaml$forecast_date), "-compare-models-v", num_files, ".csv")
      write.csv(scores_out, file = paste0(filename_loop), row.names = FALSE)

      if(is.numeric(limit) == TRUE){
        scores_out$WIS[scores_out$WIS >= limit] = limit

      }

      comp_plot <- ggplot() +
        geom_line(data=scores_out,
                  aes(date, WIS, group = model, col = model), linewidth = 1, linetype = "solid") +
        scale_x_date(date_breaks = "1 week",
                     labels=scales::date_format("%Y-%m-%d"),
                     limits = as.Date(c(min(scores_out$date),max(scores_out$date)))) +
        xlab(" ") +
        ggtitle(" ") +
        viridis::scale_color_viridis("Model",
                                     discrete=T,
                                     option = "turbo",
                                     direction = 1,
                                     na.value = "white") +
        ylab("Scaled WIS Score") +
        #annotate("text", as_date("2022-10-20"), y=2.5, label= "CFA-CAR (dashed line)", size=6, fontface="bold") +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              panel.border = element_blank(),
              legend.title = element_text(size = 16, face = "bold", hjust=0.5),
              legend.text = element_text(size=10, face="bold"),
              strip.text = element_text(size=16, face="bold"),
              strip.background = element_blank(),
              legend.position="bottom", #c(0.7, 0.7),
              legend.direction = "horizontal",
              legend.box = "horizontal",
              legend.key.size = unit(0,"line"),
              legend.key.width = unit(2,"line"),
              axis.text.y = element_text(face="bold", size=19),
              axis.text.x = element_text(face="bold", size=16, vjust=0.5,
                                         hjust=1, angle=90),
              axis.title.x = element_text(size=12, face="bold"),
              axis.title.y = element_text(size=18, face="bold"),
              plot.title = element_text(size=18, face="bold", hjust=0.5)) +
        guides(color = guide_legend(title.position = "top", label.position = "bottom"))

    }

  return(comp_plot)

}
