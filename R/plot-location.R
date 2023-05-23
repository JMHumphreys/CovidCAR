#' Plot Location
#'
#' The function \code{plot_location} is used to create a report on hospital incidence in the provided location(s) with forecasted data. The report can include all locations or specific locations as per user input.
#'
#' @param plot_path A character vector containing the paths of the CSV files with the forecasted data.
#' @param model A character vector specifying the type of model.
#' @param loc A character vector speciyfing the location to be visualized. If this parameter is NULL, the function will generate reports for all available locations. Default value is NULL.
#'
#' @return This function returns a ggplot object containing the visualization of hospital incidence in the specified location(s) with forecasted data.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#'
plot_location <- function(plot_path = plot_paths, model = NULL, loc = NULL){

  fdir_name <- paste0(su_yaml$out_dir_name,"/reports")
  if (!dir.exists(fdir_name)) {
    dir.create(fdir_name)
  }

  if(is.null(loc)){

    get_file <- read.csv(plot_path[[grep(model, plot_path)]])
    locs_vect <- unique(get_file$location_name)

    cli_alert_warning("No location provided in 'loc=', writing all locations to PDF in the analysis directory")

    pdf_name <- paste0(fdir_name, "/", as_date(su_yaml$forecast_date), "-all_locations-", model,".pdf")

        pdf(pdf_name, onefile = TRUE)
        for(i in 1:length(locs_vect)){

             get_file_f <- get_file %>% filter(location_name == locs_vect[i])

              MinDate = as_date(min(get_file_f$target_end_date))
              MaxDate = as_date(max(get_file_f$target_end_date))
              cast_start = as_date(su_yaml$forecast_horiz_start)

              get_file_f$target_end_date = as_date(get_file_f$target_end_date)

              loop.plot = ggplot() +
                geom_ribbon(data = get_file_f,
                            aes(ymin=q_0.05, ymax=q_0.95, x=target_end_date), fill = "steelblue", alpha = 0.25) +
                geom_ribbon(data = get_file_f,
                            aes(ymin=q_0.25, ymax=q_0.75, x=target_end_date), fill = "steelblue", alpha = 0.25) +
                geom_point(data=get_file_f,
                           aes(target_end_date, truth), col="red", shape=1,stroke=1) +
                geom_line(data=get_file_f,
                          aes(target_end_date, q_0.5), col="black", linewidth=1) +
                geom_vline(xintercept = cast_start,
                           linetype = "dashed",
                           col = "gray40",
                           size = 1) +
                scale_x_date(date_breaks = "1 week",
                             labels=scales::date_format("%Y-%m-%d"),
                             limits = c(MinDate, MaxDate)) +
                xlab("") +
                ggtitle(locs_vect[i]) +
                ylab("Hospital Incidence") +
                theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.background = element_blank(),
                      plot.background = element_blank(),
                      panel.border = element_blank(),
                      legend.title = element_text(size = 16, face = "bold"),
                      strip.text = element_text(size=16, face="bold"),
                      strip.background = element_blank(),
                      legend.position="bottom",
                      legend.direction = "horizontal",
                      legend.box = "horizontal",
                      legend.key.size = unit(1,"line"),
                      legend.key.width = unit(3,"line"),
                      axis.text.x = element_text(face="bold", size=12, vjust=0.5,
                                                 hjust=1, angle=90),
                      axis.text.y = element_text(face="bold", size=12, vjust=0.5),
                      axis.title.x = element_text(size=12, face="bold"),
                      axis.title.y = element_text(size=18, face="bold"),
                      plot.title = element_text(face="bold", size=22,
                                                vjust=0.5, hjust=0.5)) +
                guides(color = guide_legend(title.position = "top", label.position = "bottom"))

                plot(loop.plot)
}

  dev.off()

  } else{

    get_file <- read.csv(plot_path[[grep(model, plot_path)]]) %>%
      filter(location_name == loc)
    locs_vect <- unique(get_file$location_name)

    get_file_f <- get_file %>% filter(location_name == locs_vect[i])

    MinDate = as_date(min(get_file_f$target_end_date))
    MaxDate = as_date(max(get_file_f$target_end_date))
    cast_start = as_date(su_yaml$forecast_horiz_start)

    get_file_f$target_end_date = as_date(get_file_f$target_end_date)

    ggplot() +
      geom_ribbon(data = get_file_f,
                  aes(ymin=q_0.05, ymax=q_0.95, x=target_end_date), fill = "steelblue", alpha = 0.25) +
      geom_ribbon(data = get_file_f,
                  aes(ymin=q_0.25, ymax=q_0.75, x=target_end_date), fill = "steelblue", alpha = 0.25) +
      geom_point(data=get_file_f,
                 aes(target_end_date, truth), col="red", shape=1,stroke=1) +
      geom_line(data=get_file_f,
                aes(target_end_date, q_0.5), col="black", linewidth=1) +
      geom_vline(xintercept = cast_start,
                 linetype = "dashed",
                 col = "gray40",
                 size = 1) +
      scale_x_date(date_breaks = "1 week",
                   labels=scales::date_format("%Y-%m-%d"),
                   limits = c(MinDate, MaxDate)) +
      xlab("") +
      ggtitle(locs_vect[i]) +
      ylab("Hospital Incidence") +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.border = element_blank(),
            legend.title = element_text(size = 16, face = "bold"),
            strip.text = element_text(size=16, face="bold"),
            strip.background = element_blank(),
            legend.position="bottom",
            legend.direction = "horizontal",
            legend.box = "horizontal",
            legend.key.size = unit(1,"line"),
            legend.key.width = unit(3,"line"),
            axis.text.x = element_text(face="bold", size=12, vjust=0.5,
                                       hjust=1, angle=90),
            axis.text.y = element_text(face="bold", size=12, vjust=0.5),
            axis.title.x = element_text(size=12, face="bold"),
            axis.title.y = element_text(size=18, face="bold"),
            plot.title = element_text(face="bold", size=22,
                                      vjust=0.5, hjust=0.5)) +
      guides(color = guide_legend(title.position = "top", label.position = "bottom"))

  }


}
